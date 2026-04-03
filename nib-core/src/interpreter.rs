//! Compile code to bytecode then run it

use std::fs::read_to_string;
use std::path::{Path, PathBuf};

use symbol_table::static_symbol;

use crate::ast;
use crate::common::{Error, Name, Result, Signature, Symbol, sym};
use crate::core::{desugar, desugar_expression};
use crate::interpreter::bytecode::*;
use crate::interpreter::compile::{Module, compile, compile_expression};
use crate::interpreter::heap::{Array, Bytes, Heap, Table, Value, ValueRepr, set_value};
use crate::interpreter::vm::Stack;
use crate::parser::{parse_declarations, parse_expression};

pub mod bytecode;
pub mod compile;
pub mod foreign;
pub mod heap;
pub mod prims;
mod tests;
pub mod vm;

pub struct Runtime {
    heap: Heap,
    global_env: Value,
    local_env: Value,
    stack: Stack,
    call_stack: Stack,
    code: Value,
    ip: usize,
    frame_args: i64,
    ffi_signatures: Vec<Signature>,
    options: Options,
}

pub struct Options {
    output_core: bool,
    trace: bool,
    log_missing_keys: bool,
    trace_gc_level: u8,
    lib_paths: Vec<String>,
}

const DEFAULT_HEAP_SIZE: usize = 1000000;
const DEFAULT_STACK_SIZE: usize = 10000;
const DEFAULT_CALL_STACK_SIZE: usize = 10000;

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Options {
    fn new() -> Self {
        Options {
            output_core: false,
            trace: false,
            log_missing_keys: true,
            trace_gc_level: 0,
            lib_paths: Vec::new(),
        }
    }
}

impl Runtime {
    pub fn new() -> Self {
        let heap = Heap::new(DEFAULT_HEAP_SIZE);
        let mut runtime = Runtime {
            heap,
            global_env: Value::nil(),
            local_env: Value::nil(),
            stack: Stack::new(Value::nil()),      // Dummy stack
            call_stack: Stack::new(Value::nil()), // Dummy stack
            code: Value::nil(),
            ip: 0,
            frame_args: 0,
            ffi_signatures: Vec::new(),
            options: Options::new(),
        };
        let global_env = Value::from(Table::make(&mut runtime));
        let stack = Value::from(Array::make(&mut runtime, DEFAULT_STACK_SIZE));
        let call_stack = Value::from(Array::make(&mut runtime, DEFAULT_CALL_STACK_SIZE));
        runtime.global_env = global_env;
        runtime.stack = Stack::new(stack);
        runtime.call_stack = Stack::new(call_stack);
        runtime.register_intrinsics();
        runtime
    }

    pub fn load(&mut self, path: &Path, reload: bool) -> Result<()> {
        let id = self.package_name(path)?;
        if self.check_package(&id, reload)? {
            if let Some(libpath) = self.find_lib(path) {
                let code = read_to_string(&libpath)?;
                let file = libpath
                    .as_os_str()
                    .to_str()
                    .ok_or(self.err("Filenames must be utf-8"))?;
                self.execute_code(file, &code)
            } else {
                self.error(&format!(
                    "couldn't find library {}",
                    path.as_os_str().to_string_lossy()
                ))
            }
        } else {
            Ok(())
        }
    }

    fn check_package(&mut self, id: &Symbol, reload: bool) -> Result<bool> {
        if self.has_package(id)? && !reload {
            return Ok(false);
        }
        self.package_table()
            .insert(self, Value::symbol(id), Value::bool(true));
        Ok(true)
    }

    pub fn eval(&mut self, name: &Option<Symbol>, code: &str, reload: bool) -> Result<()> {
        let s = if let Some(id) = name {
            if !self.check_package(id, reload)? {
                return Ok(());
            }
            id.as_str()
        } else {
            ""
        };
        self.execute_code(s, code)
    }

    pub fn find_lib(&self, path: &Path) -> Option<PathBuf> {
        let libpath = self.get_name(&Name::str("nib.libpath"))?;
        let arr = libpath.get_array();
        for v in arr.values() {
            if let Ok(p) = self.get_string(v) {
                let prefix = Path::new(&p);
                let candidate = prefix.join(path);
                if candidate.exists() {
                    return Some(candidate);
                }
            }
        }
        None
    }

    pub fn execute_code(&mut self, name: &str, code: &str) -> Result<()> {
        let file = if name.is_empty() {
            None
        } else {
            Some(name.to_owned())
        };
        let mut module = ast::Module::new(file, code);
        parse_declarations(&mut module)?;
        let core = desugar(module)?;
        if self.options.output_core {
            println!("{}", core);
        }
        let bytecode = compile(core)?;
        self.run_module(bytecode)
    }

    pub fn set_output_core(&mut self, output: bool) {
        self.options.output_core = output;
    }

    pub fn package_table(&mut self) -> Table {
        self.get_name(&Name::str("nib.packages"))
            .unwrap()
            .get_table()
    }

    pub fn has_package(&mut self, id: &Symbol) -> Result<bool> {
        Ok(!self.package_table().get(Value::symbol(id)).is_nil())
    }

    pub fn set_tracing(&mut self, tracing: bool) {
        self.options.trace = tracing;
    }

    pub fn set_log_missing_keys(&mut self, log: bool) {
        self.options.log_missing_keys = log;
    }

    pub fn set_trace_gc(&mut self, trace: u8) {
        self.options.trace_gc_level = trace;
    }

    pub fn package_name(&mut self, path: &Path) -> Result<Symbol> {
        let basename = path
            .file_stem()
            .ok_or(self.err("No filename in package check"))?;
        let validname = basename.to_str().ok_or(self.err(&format!(
            "Weird package name {}, keep to unicode",
            basename.to_string_lossy()
        )))?;
        Ok(sym(validname))
    }

    pub fn run_module(&mut self, bytecode: Module) -> Result<()> {
        self.local_env = self.make_local_env(&bytecode);
        let bc = Bytes::with(self, &bytecode.byte_code);
        self.code = Value::from(bc);
        self.ip = 0;
        self.run()
    }

    pub fn run_expression(&mut self, code: &str) -> Result<Value> {
        let expression = parse_expression(code)?;
        let core = desugar_expression(expression)?;
        if self.options.output_core {
            println!("{}", core);
        }
        let compiled = compile_expression(core)?;
        self.run_module(compiled)?;
        Ok(self.stack.pop())
    }

    pub fn make_local_env(&mut self, module: &compile::Module) -> Value {
        let mut array = Array::make(self, module.local_env_size);
        for (blob, &idx) in &module.data {
            let bytes = Bytes::with(self, blob);
            array.set(idx, Value::from(bytes));
        }
        Value::from(array)
    }

    pub fn set_global(&mut self, sym: &Symbol, value: &Value) {
        let mut env = self.global_env.get_table();
        env.insert(self, Value::symbol(sym), *value);
    }

    pub fn get_global(&self, sym: &Symbol) -> Value {
        let env = self.global_env.get_table();
        env.get(Value::symbol(sym))
    }

    pub fn add_name(&mut self, name: &Name, val: &Value) -> Result<()> {
        match name {
            Name::Qualified(path, leaf) => {
                let s = Value::from(*leaf);
                let t = self.get_or_create_module_path(path, self.global_env)?;
                t.get_table().insert(self, s, *val);
            }
            Name::Plain(n) => {
                self.set_global(n, val);
            }
        }
        Ok(())
    }

    pub fn get_name(&self, name: &Name) -> Option<Value> {
        let val = match name {
            Name::Qualified(path, leaf) => {
                if let Some(t) = self.get_module_path(path, self.global_env) {
                    let key = Value::from(*leaf);
                    t.get_table().get(key)
                } else {
                    Value::nil()
                }
            }
            Name::Plain(name) => self.get_global(name),
        };
        if val.is_nil() { None } else { Some(val) }
    }

    pub fn get_module_path(&self, path: &[Symbol], start: Value) -> Option<Value> {
        let mut rest = path;
        let mut table = start;
        while !rest.is_empty() {
            let sym = Value::from(rest[0]);
            table = {
                let t = table.get_table();
                let v = t.get(sym);
                match v.get_repr() {
                    ValueRepr::Table => v,
                    _ => {
                        return None;
                    }
                }
            };
            rest = &rest[1..];
        }
        Some(table)
    }

    pub fn get_or_create_module_path(&mut self, path: &[Symbol], start: Value) -> Result<Value> {
        let mut rest = path;
        let mut table = start;
        ensure_type(&table, ValueRepr::Table)?;
        while !rest.is_empty() {
            let sym = &rest[0];
            table = {
                let mut t = table.get_table();
                let key = Value::from(*sym);
                let v = t.get(key);
                match v.get_repr() {
                    ValueRepr::Table => v,
                    ValueRepr::Nil => {
                        let nt = Value::from(Table::make(self));
                        t.insert(self, key, nt);
                        nt
                    }
                    _ => {
                        return self.error(&format!("Illegal module path {:?}", path));
                    }
                }
            };
            rest = &rest[1..];
        }
        Ok(table)
    }

    pub fn get_type_table(&self, val: &Value) -> Result<Value> {
        let typ = match val.get_repr() {
            ValueRepr::Nil => self.get_global(&static_symbol!("nil_type")),
            ValueRepr::Undefined => Value::nil(),
            ValueRepr::Bool => self.get_global(&static_symbol!("bool")),
            ValueRepr::Integer => self.get_global(&static_symbol!("int")),
            ValueRepr::Pointer => self.get_global(&static_symbol!("pointer")),
            ValueRepr::Char => self.get_global(&static_symbol!("char")),
            ValueRepr::Float => self.get_global(&static_symbol!("float")),
            ValueRepr::BoxedInteger => todo!(),
            ValueRepr::Symbol => self.get_global(&static_symbol!("symbol")),
            ValueRepr::CallContinuation => self.get_global(&static_symbol!("call_continuation")),
            ValueRepr::PartialApplication => {
                self.get_global(&static_symbol!("partial_application"))
            }
            ValueRepr::Array => {
                let arr = val.get_array();
                let mut type_table = arr.type_table();
                if type_table == Value::nil() {
                    type_table = self.get_global(&static_symbol!("array"));
                }
                type_table
            }
            ValueRepr::Bytes => {
                let bytes = val.get_bytes();
                let mut type_table = bytes.type_table();
                if type_table == Value::nil() {
                    type_table = self.get_global(&static_symbol!("bytes"));
                }
                type_table
            }
            ValueRepr::Table => {
                let table = val.get_table();
                let mut type_table = table.type_table();
                if type_table == Value::nil() {
                    type_table = self.get_global(&static_symbol!("table"));
                }
                type_table
            }
            ValueRepr::Closure => {
                let closure = val.get_closure();
                let mut type_table = closure.type_table();
                if type_table == Value::nil() {
                    type_table = self.get_global(&static_symbol!("function"));
                }
                type_table
            }
            ValueRepr::Object => todo!(),
        };
        ensure_type(&typ, ValueRepr::Table)?;
        Ok(typ)
    }

    pub fn get_type_id(&self, val: &Value) -> Result<Symbol> {
        let tt = self.get_type_table(val)?.get_table();
        let tid = tt.get(Value::symbol(&sym("type_id")));
        if !tid.is_symbol() {
            return self.error("Type table has no type_id");
        }
        Ok(tid.get_symbol())
    }
}

pub(super) fn ensure_type(val: &Value, repr: ValueRepr) -> Result<()> {
    if val.get_repr() != repr {
        Err(Error::runtime_error(&format!(
            "Expected {:?}, has {:?}",
            repr,
            val.get_repr()
        )))
    } else {
        Ok(())
    }
}
