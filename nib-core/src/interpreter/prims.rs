use std::{collections::HashMap, ffi::c_void, path::Path, sync::LazyLock};

use symbol_table::static_symbol;

use crate::{
    common::{Name, Result, Symbol, sym},
    core::Arity,
    interpreter::{
        Runtime,
        bytecode::{
            INSTR_ACOS, INSTR_ADD, INSTR_ALLOC_ARRAY, INSTR_ALLOC_BYTES, INSTR_ALLOC_TABLE,
            INSTR_ARRAY_REF, INSTR_ARRAY_SET, INSTR_ARRAY_SIZE, INSTR_ASIN, INSTR_ATAN,
            INSTR_BITAND, INSTR_BITNOT, INSTR_BITOR, INSTR_BITSHIFT, INSTR_BITXOR, INSTR_BYTES_REF,
            INSTR_BYTES_SET, INSTR_BYTES_SIZE, INSTR_CALL, INSTR_CEILING, INSTR_CMP, INSTR_COS,
            INSTR_DIV, INSTR_EQ, INSTR_EXP, INSTR_FLOOR, INSTR_GT, INSTR_GTE, INSTR_IS_ARRAY,
            INSTR_IS_BOOL, INSTR_IS_BYTES, INSTR_IS_CHAR, INSTR_IS_CLOSURE, INSTR_IS_FLOAT,
            INSTR_IS_INTEGER, INSTR_IS_NIL, INSTR_IS_PAP, INSTR_IS_POINTER, INSTR_IS_SYMBOL,
            INSTR_IS_TABLE, INSTR_LOG, INSTR_LT, INSTR_LTE, INSTR_MOD, INSTR_MUL, INSTR_NEG,
            INSTR_ROUND, INSTR_SET_TYPE, INSTR_SIN, INSTR_SUB, INSTR_TABLE_DELETE, INSTR_TABLE_GET,
            INSTR_TABLE_SET, INSTR_TABLE_SIZE, INSTR_TAN, INSTR_TOINT, INSTR_TYPE,
        },
        ensure_type,
        heap::{Array, Bytes, Closure, Code, Table, Value, ValueRepr},
    },
    runtime::Interpreter,
};

pub type PrimFn = fn(&mut Runtime) -> Result<()>;

impl Runtime {
    pub(super) fn register_intrinsics(&mut self) {
        self.register_type_tables();
        self.register_primitives();
        self.register_nib_namespace().unwrap();
        self.register_system_constants().unwrap();
        self.register_foreign_interface().unwrap();
    }

    pub(super) fn register_primitives(&mut self) {
        self.set_global(&sym("global"), &self.global_env.clone());

        self.register_primitive(
            "_prim_print_representation",
            prim_print_representation,
            Arity::Fixed(1),
        );
        self.register_primitive("_prim_project", prim_project, Arity::VarArg(2, 1));
        self.register_primitive("_prim_array_make", prim_array_make, Arity::VarArg(1, 0));
        self.register_primitive("_prim_array_match", prim_array_match, Arity::Fixed(1));
        self.register_primitive("_prim_match", prim_match, Arity::Fixed(1));
        self.register_primitive("_prim_string_print", prim_string_print, Arity::Fixed(1));
        self.register_primitive("_prim_to_string", prim_to_string, Arity::Fixed(1));
        self.register_primitive("_prim_load", prim_load, Arity::Fixed(1));
        self.register_primitive("_prim_symbol_make", prim_symbol_make, Arity::Fixed(1));
        self.register_primitive("_prim_symbol_name", prim_symbol_name, Arity::Fixed(1));
        self.register_primitive("_prim_get_path", prim_get_path, Arity::VarArg(2, 1));
        self.register_primitive("_prim_bytes_make", prim_bytes_make, Arity::VarArg(1, 0));
        self.register_primitive("_prim_table_keys", prim_table_keys, Arity::Fixed(1));
        self.register_primitive("_prim_table_clear", prim_table_clear, Arity::Fixed(1));
        self.register_primitive("_prim_exit", prim_exit, Arity::Fixed(1));
        self.register_primitive("_prim_panic", prim_panic, Arity::Fixed(1));
        self.register_primitive("_prim_string_pack", prim_string_pack, Arity::Fixed(1));
        self.register_primitive("_prim_string_unpack", prim_string_unpack, Arity::Fixed(1));
        self.register_primitive(
            "_prim_string_substring",
            prim_string_substring,
            Arity::Fixed(3),
        );
        self.register_primitive("_prim_to_char", prim_to_char, Arity::Fixed(1));
        self.register_primitive("_prim_to_pointer", prim_to_pointer, Arity::Fixed(1));
        self.register_primitive("_prim_apply", prim_apply, Arity::Fixed(2));
    }

    pub(super) fn register_type_tables(&mut self) {
        self.register_type("string", "string");
        self.register_type("nil_type", "nil");
        self.register_type("bool", "bool");
        self.register_type("int", "int");
        self.register_type("float", "float");
        self.register_type("char", "char");
        self.register_type("pointer", "pointer");
        self.register_type("symbol", "symbol");
        self.register_type("bytes", "bytes");
        self.register_type("array", "array");
        self.register_type("table", "table");
        self.register_type("function", "function");
        self.register_type("call_continuation", "call_continuation");
        self.register_type("partial_application", "partial_application");
    }

    fn register_type(&mut self, table_name: &str, type_name: &str) {
        let new_table = Value::from(Table::make(self));
        self.set_global(&sym(table_name), &new_table);
        let tname = self.make_symbol(type_name);
        self.add_name(&Name::str(&format!("{}.type_id", table_name)), &tname)
            .unwrap();
    }

    pub(super) fn register_system_constants(&mut self) -> Result<()> {
        let os = self.make_string(std::env::consts::OS);
        self.add_name(&Name::str("system.os"), &os)?;
        let family = self.make_string(std::env::consts::FAMILY);
        self.add_name(&Name::str("system.os_family"), &family)?;
        let arch = self.make_string(std::env::consts::ARCH);
        self.add_name(&Name::str("system.arch"), &arch)?;
        let dll_ext = self.make_string(std::env::consts::DLL_EXTENSION);
        self.add_name(&Name::str("system.dll_extension"), &dll_ext)?;
        let dll_prefix = self.make_string(std::env::consts::DLL_PREFIX);
        self.add_name(&Name::str("system.dll_prefix"), &dll_prefix)?;
        let exe_ext = self.make_string(std::env::consts::EXE_EXTENSION);
        self.add_name(&Name::str("system.exe_extension"), &exe_ext)?;
        Ok(())
    }

    pub(super) fn register_nib_namespace(&mut self) -> Result<()> {
        let table = Value::from(Table::make(self));
        self.add_name(&Name::str("nib.packages"), &table)?;
        Ok(())
    }

    pub fn register_primitive(
        &mut self,
        name: &str,
        fun: fn(&mut Runtime) -> Result<()>,
        arity: Arity,
    ) {
        let prim = self.make_primitive(fun, arity);
        self.set_global(&sym(name), &prim);
    }

    pub fn make_primitive(&mut self, fun: fn(&mut Runtime) -> Result<()>, arity: Arity) -> Value {
        let code = Code::Extern(fun as *const c_void);
        let (args, vararg) = match arity {
            Arity::Fixed(n) => (n as usize, None),
            Arity::VarArg(n, v) => (n as usize, Some(v as usize)),
        };
        let closure = Closure::make(self, &code, &[], args, vararg);
        Value::from(closure)
    }

    pub fn make_string(&mut self, s: &str) -> Value {
        let b = Bytes::with(self, s.as_bytes());
        let type_table = self
            .get_module_path(&[static_symbol!("string")], self.global_env)
            .unwrap_or(Value::nil());
        // If the string type table doesn't exist yet, leave it
        // as nil.
        b.set_type_table(type_table);
        Value::from(b)
    }

    pub fn make_symbol(&mut self, s: &str) -> Value {
        Value::symbol(&sym(s))
    }

    pub fn find_overload(&mut self, val: &Value, method: &Symbol) -> Option<Value> {
        let tt_val = self.get_type_table(val).ok()?;
        let tt = tt_val.get_table();
        let m = tt.get(Value::symbol(method));
        if m.is_nil() { None } else { Some(m) }
    }

    pub fn call_function(&mut self, fun: &Value, args: &[Value]) -> Result<bool> {
        self.stack_push(*fun);
        self.ensure_stack(args.len());
        self.stack.pushv(args);
        self.stack.push(Value::integer((args.len() + 1) as i64));
        self.op_call(INSTR_CALL)
    }

    pub fn is_type(&self, val: &Value, t: &Symbol) -> Result<bool> {
        Ok(*t == self.get_type_id(val)?)
    }

    pub fn get_string(&self, value: &Value) -> Result<String> {
        if self.is_type(value, &sym("string"))? {
            let bytes = value.get_bytes();
            str::from_utf8(bytes.get_slice())
                .map_err(|_| self.err("Not an utf-8 string"))
                .map(|s| s.to_owned())
        } else {
            self.error("Not a string value")
        }
    }
}

fn prim_get_path(rt: &mut Runtime) -> Result<()> {
    let path = rt.stack.pop();
    let first = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    ensure_type(&path, ValueRepr::Array)?;
    let arr = path.get_array();
    let syms = arr.values();
    if syms.iter().any(|v| !v.is_symbol()) {
        return rt.error("prim_get_path: All trailing arguments must be symbols");
    }
    let symbols: Vec<_> = syms.iter().map(|v| v.get_symbol()).collect();
    let initial = match first.get_repr() {
        ValueRepr::Symbol => {
            let symbol = first.get_symbol();
            let val = rt.get_global(&symbol);
            if !val.is_table() {
                let new_val = Value::from(Table::make(rt));
                rt.set_global(&symbol, &new_val);
                new_val
            } else {
                val
            }
        }
        ValueRepr::Table => first,
        _ => {
            return rt.error("prim_get_path: First argument must be table or symbol");
        }
    };
    let res = rt.get_or_create_module_path(&symbols, initial)?;
    rt.stack.push(res);
    Ok(())
}

fn prim_print_representation(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    print!("{:?}", val);
    rt.stack_push(Value::nil());
    Ok(())
}

fn prim_project(rt: &mut Runtime) -> Result<()> {
    let projection = rt.stack.pop().get_array();
    let start = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    if let Some(method) = rt.find_overload(&start, &static_symbol!("project")) {
        let mut args = vec![start];
        args.extend_from_slice(projection.values());
        rt.call_function(&method, &args).map(|_| ())?;
    } else {
        let mut current = start;
        for s in projection.values() {
            ensure_type(&current, ValueRepr::Table)?;
            ensure_type(s, ValueRepr::Symbol)?;
            current = current.get_table().get(*s);
        }
        rt.stack_push(current);
    }
    Ok(())
}

fn prim_array_make(rt: &mut Runtime) -> Result<()> {
    let args = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    rt.stack_push(args);
    Ok(())
}

fn prim_array_match(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    if val.is_array() {
        rt.stack.push(val);
    } else {
        rt.stack.push(Value::bool(false));
    }
    Ok(())
}

fn prim_match(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    match val.get_repr() {
        ValueRepr::Closure => {
            rt.stack_push(val);
        }
        ValueRepr::Table => {
            let m = static_symbol!("match");
            let fun = val.get_table().get(Value::symbol(&m));
            if fun.is_closure() {
                rt.stack_push(fun);
            } else {
                return rt.error("_prim_match: custom matcher in table not a function");
            }
        }
        _ => {
            return rt
                .error("_prim_match: custom matcher must be function or table with a match entry");
        }
    }
    Ok(())
}

fn prim_string_print(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    print!("{}", val);
    rt.stack_push(Value::nil());
    Ok(())
}

fn prim_to_string(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    let str = format!("{}", val);
    let out = rt.make_string(&str);
    rt.stack_push(out);
    Ok(())
}

fn prim_load(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    let file = rt.get_string(&val)?;
    rt.load(Path::new(&file), false)?;
    rt.stack_push(Value::nil());
    Ok(())
}

fn prim_symbol_name(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    ensure_type(&val, ValueRepr::Symbol)?;
    let out = rt.make_string(val.get_symbol().as_str());
    rt.stack_push(out);
    Ok(())
}

fn prim_symbol_make(rt: &mut Runtime) -> Result<()> {
    let arg = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    ensure_type(&arg, ValueRepr::Bytes)?;
    let bytes = arg.get_bytes();
    let str = str::from_utf8(bytes.get_slice()).unwrap_or_default();
    let sym = rt.make_symbol(str);
    rt.stack_push(sym);
    Ok(())
}

fn prim_bytes_make(rt: &mut Runtime) -> Result<()> {
    let vals = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    let array = vals.get_array();
    let mut bytes = Vec::new();
    for v in array.values() {
        let n = v.get_integer();
        if n < 0 || n > 255 {
            return rt.error("bytes_make: value out of range for byte");
        }
        bytes.push(n as u8);
    }
    let out = Bytes::with(rt, &bytes);
    rt.stack_push(Value::from(out));
    Ok(())
}

fn prim_table_keys(rt: &mut Runtime) -> Result<()> {
    let arg = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    ensure_type(&arg, ValueRepr::Table)?;
    let table = arg.get_table();
    let keys = table.keys(rt);
    rt.stack_push(keys);
    Ok(())
}

fn prim_table_clear(rt: &mut Runtime) -> Result<()> {
    let arg = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    ensure_type(&arg, ValueRepr::Table)?;
    let mut table = arg.get_table();
    table.clear(rt);
    rt.stack_push(Value::nil());
    Ok(())
}

fn prim_exit(rt: &mut Runtime) -> Result<()> {
    let exitcode = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    Err(crate::common::Error::NibExit {
        exit_code: exitcode.get_integer() as i32,
    })
}

fn prim_panic(rt: &mut Runtime) -> Result<()> {
    let msg = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    let str = rt.get_string(&msg)?;
    Err(crate::common::Error::NibPanic { msg: str })
}

fn prim_string_pack(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    let array = val.get_array();
    let mut packed = String::new();
    for i in array.values() {
        ensure_type(i, ValueRepr::Char)?;
        packed.push(i.get_char());
    }
    let out = rt.make_string(&packed);
    rt.stack_push(out);
    Ok(())
}

fn prim_string_unpack(rt: &mut Runtime) -> Result<()> {
    let msg = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    let str = rt.get_string(&msg)?;
    let arr = Array::make(rt, str.len());
    for (i, ch) in str.chars().enumerate() {
        arr.set(i, Value::char(ch));
    }
    rt.stack_push(Value::from(arr));
    Ok(())
}

fn prim_string_substring(rt: &mut Runtime) -> Result<()> {
    let stop = rt.stack.pop().get_integer();
    let start = rt.stack.pop().get_integer();
    let msg = rt.stack.pop();
    let str = rt.get_string(&msg)?;
    let _ = rt.stack.pop(); // pop closure
    let substring = str
        .chars()
        .skip(start as usize)
        .take((stop - start) as usize);
    let out = rt.make_string(&String::from_iter(substring));
    rt.stack_push(out);
    Ok(())
}

fn prim_to_char(rt: &mut Runtime) -> Result<()> {
    let codepoint = rt.stack.pop().get_integer() as u32;
    let _ = rt.stack.pop(); // pop closure
    if let Some(ch) = char::from_u32(codepoint) {
        rt.stack_push(Value::char(ch));
    } else {
        rt.stack_push(Value::bool(false));
    }
    Ok(())
}

fn prim_to_pointer(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    match val.get_repr() {
        ValueRepr::Integer => {
            let addr = val.get_integer() as usize;
            rt.stack_push(Value::cpointer(addr as *const c_void));
        }
        ValueRepr::Bytes => {
            let ptr = val.get_bytes().get_slice().as_ptr();
            rt.stack_push(Value::cpointer(ptr));
        }
        _ => return rt.error("prim_to_pointer: argument needs to be an integer or a byte array"),
    }
    Ok(())
}

fn prim_apply(rt: &mut Runtime) -> Result<()> {
    let args = rt.stack.pop();
    let fun = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    ensure_type(&args, ValueRepr::Array)?;
    let array = args.get_array();
    let quit = rt.call_function(&fun, array.values())?;
    if quit {
        // TODO: Harmonize prims and intstructions return values
        // Should probably always signal exit with Err.
        Err(crate::common::Error::NibExit { exit_code: 0 })
    } else {
        Ok(())
    }
}

/// Primitives that are implemented as bytecode instructions
/// rather than calling out to a function.
pub fn is_bytecode_primitive(prim: &Symbol) -> Option<u8> {
    static BYTECODE_PRIMS: LazyLock<HashMap<Symbol, u8>> = LazyLock::new(|| {
        let mut prims = HashMap::new();
        prims.insert(static_symbol!("_prim_add"), INSTR_ADD);
        prims.insert(static_symbol!("_prim_sub"), INSTR_SUB);
        prims.insert(static_symbol!("_prim_mul"), INSTR_MUL);
        prims.insert(static_symbol!("_prim_div"), INSTR_DIV);
        prims.insert(static_symbol!("_prim_mod"), INSTR_MOD);
        prims.insert(static_symbol!("_prim_bitand"), INSTR_BITAND);
        prims.insert(static_symbol!("_prim_bitor"), INSTR_BITOR);
        prims.insert(static_symbol!("_prim_bitxor"), INSTR_BITXOR);
        prims.insert(static_symbol!("_prim_bitshift"), INSTR_BITSHIFT);
        prims.insert(static_symbol!("_prim_bitnot"), INSTR_BITNOT);
        prims.insert(static_symbol!("_prim_type"), INSTR_TYPE);
        prims.insert(static_symbol!("_prim_type_set"), INSTR_SET_TYPE);
        prims.insert(static_symbol!("_prim_negate"), INSTR_NEG);
        prims.insert(static_symbol!("_prim_gte"), INSTR_GTE);
        prims.insert(static_symbol!("_prim_gt"), INSTR_GT);
        prims.insert(static_symbol!("_prim_lte"), INSTR_LTE);
        prims.insert(static_symbol!("_prim_lt"), INSTR_LT);
        prims.insert(static_symbol!("_prim_cmp"), INSTR_CMP);
        prims.insert(static_symbol!("_prim_eq"), INSTR_EQ);
        prims.insert(static_symbol!("_prim_sin"), INSTR_SIN);
        prims.insert(static_symbol!("_prim_cos"), INSTR_COS);
        prims.insert(static_symbol!("_prim_tan"), INSTR_TAN);
        prims.insert(static_symbol!("_prim_asin"), INSTR_ASIN);
        prims.insert(static_symbol!("_prim_acos"), INSTR_ACOS);
        prims.insert(static_symbol!("_prim_atan"), INSTR_ATAN);
        prims.insert(static_symbol!("_prim_ceiling"), INSTR_CEILING);
        prims.insert(static_symbol!("_prim_floor"), INSTR_FLOOR);
        prims.insert(static_symbol!("_prim_round"), INSTR_ROUND);
        prims.insert(static_symbol!("_prim_log"), INSTR_LOG);
        prims.insert(static_symbol!("_prim_exp"), INSTR_EXP);
        prims.insert(static_symbol!("_prim_to_int"), INSTR_TOINT);
        prims.insert(static_symbol!("_prim_array_ref"), INSTR_ARRAY_REF);
        prims.insert(static_symbol!("_prim_array_set"), INSTR_ARRAY_SET);
        prims.insert(static_symbol!("_prim_array_create"), INSTR_ALLOC_ARRAY);
        prims.insert(static_symbol!("_prim_array_size"), INSTR_ARRAY_SIZE);
        prims.insert(static_symbol!("_prim_bytes_ref"), INSTR_BYTES_REF);
        prims.insert(static_symbol!("_prim_bytes_set"), INSTR_BYTES_SET);
        prims.insert(static_symbol!("_prim_bytes_create"), INSTR_ALLOC_BYTES);
        prims.insert(static_symbol!("_prim_bytes_size"), INSTR_BYTES_SIZE);
        prims.insert(static_symbol!("_prim_table_create"), INSTR_ALLOC_TABLE);
        prims.insert(static_symbol!("_prim_table_set"), INSTR_TABLE_SET);
        prims.insert(static_symbol!("_prim_table_size"), INSTR_TABLE_SIZE);
        prims.insert(static_symbol!("_prim_table_get"), INSTR_TABLE_GET);
        prims.insert(static_symbol!("_prim_table_delete"), INSTR_TABLE_DELETE);
        prims.insert(static_symbol!("_prim_is_integer"), INSTR_IS_INTEGER);
        prims.insert(static_symbol!("_prim_is_nil"), INSTR_IS_NIL);
        prims.insert(static_symbol!("_prim_is_bool"), INSTR_IS_BOOL);
        prims.insert(static_symbol!("_prim_is_char"), INSTR_IS_CHAR);
        prims.insert(static_symbol!("_prim_is_float"), INSTR_IS_FLOAT);
        prims.insert(static_symbol!("_prim_is_symbol"), INSTR_IS_SYMBOL);
        prims.insert(static_symbol!("_prim_is_array"), INSTR_IS_ARRAY);
        prims.insert(static_symbol!("_prim_is_closure"), INSTR_IS_CLOSURE);
        prims.insert(static_symbol!("_prim_is_bytes"), INSTR_IS_BYTES);
        prims.insert(static_symbol!("_prim_is_table"), INSTR_IS_TABLE);
        prims.insert(static_symbol!("_prim_is_pointer"), INSTR_IS_POINTER);
        prims.insert(static_symbol!("_prim_is_pap"), INSTR_IS_PAP);
        prims
    });
    BYTECODE_PRIMS.get(prim).copied()
}
