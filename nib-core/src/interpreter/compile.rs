use crate::ast::Literal;
use crate::common::{Metadata, Name};
use crate::common::{Result, Symbol};
use crate::core::{Binder, Binding, Expression};
use crate::interpreter::bytecode::{INSTR_ALLOC_FLOAT, INSTR_GET_LOCAL, INSTR_GLOBAL_ENV, INSTR_LOAD_BYTES_IMM, INSTR_LOAD_IMM8, INSTR_LOAD_IMM16, INSTR_LOAD_IMM32, INSTR_LOAD_IMM64, INSTR_SET_TYPE, INSTR_TABLE_GET};
use crate::interpreter::heap::Value;
use std::collections::HashMap;
use std::mem;

pub fn compile(from: crate::core::Module) -> Result<Module> {
    let module = Module::new();
    let mut compilation = Compilation::new(from);
    compilation.compile()?;
    Ok(compilation.module)
}

pub fn compile_expression(expr: crate::core::Expression) -> Result<Module> {
    let binding = crate::core::Binding::binding(0, Binder::Local(Name::str("it")), expr);
    let module = crate::core::Module {
        metadata: Metadata::empty(),
        bindings: vec![binding],
    };
    compile(module)
}

#[derive(Debug, Clone)]
pub struct Module {
    metadata: Option<Metadata>,
    bcode: Vec<u8>,
    local_env_size: usize,
    /// A list of symbol literals that should be put into the local environment.
    // TODO: Remove. Not needed. Can just load the symbol as an imm64
    // Note, this would make the output bound to the process, is this what I want?
    // I can serialize the module if I import the symbols from the environment.
    want_symbols: HashMap<Symbol, usize>,
    /// Global variables used by the module.
    captures: HashMap<Symbol, usize>,
}

impl Module {
    pub fn new() -> Self {
        Module {
            metadata: None,
            bcode: Vec::new(),
            local_env_size: 0,
            want_symbols: HashMap::new(),
            captures: HashMap::new(),
        }
    }
}

/// State held during compilation. Everything that can be discarded when finished
/// goes here.
#[derive(Debug, Clone)]
pub(super) struct Compilation {
    module: Module,
    core_bindings: Vec<crate::core::Binding>,
    next_loc: usize,
    local_vars: HashMap<Symbol, usize>,
    code: Vec<u8>,
}

impl Compilation {
    pub(super) fn new(from: crate::core::Module) -> Self {
        let metadata = from.metadata;
        let bindings = from.bindings;
        let mut compilation = Compilation {
            module: Module::new(),
            core_bindings: bindings,
            next_loc: 0,
            local_vars: HashMap::new(),
            code: Vec::new(),
        };
        compilation.module.metadata = Some(metadata);
        compilation
    }

    pub(super) fn compile(&mut self) -> Result<()> {
        let bindings = mem::replace(&mut self.core_bindings, vec![]);
        for b in bindings {
            self.compile_binding(&b, true)?;
        }
        Ok(())
    }

    fn compile_binding(&mut self, binding: &Binding, top_level:bool) -> Result<()> {
        self.compile_expression(&binding.body)?;
        match &binding.binder {
            Binder::Public(Name::Qualified(path, name)) => todo!(),
            Binder::Public(Name::Plain(name)) => {
                todo!()
            }
            Binder::Local(Name::Qualified(path, name)) => {
                todo!()
            },
            Binder::Local(Name::Plain(name)) => {
                todo!()
            }
            Binder::Unbound => {
                Ok(())
            },
        }
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<()> {
        match expression {
            Expression::Literal(_, literal) => todo!(),
            Expression::Var(_, var) => todo!(),
            Expression::Lambda(_, lambda) => todo!(),
            Expression::Cond(_, cond) => todo!(),
            Expression::App(_, expressions) => todo!(),
            Expression::Where(_, expression, bindings) => todo!(),
        }
    }

    fn compile_literal(&mut self, literal: &Literal) -> Result<()> {
        match literal {
            Literal::Nil => {
                self.code.push(INSTR_LOAD_IMM8);
                self.code.push(0x36);
            },
            Literal::Bool(b) => {
                self.code.push(INSTR_LOAD_IMM8);
                self.code.push(if *b { 0x2e} else {0x26});
            },
            Literal::Integer(n) => {
                self.load_constant_int(*n);
            },
            Literal::Real(f) => {
                self.code.push(INSTR_LOAD_IMM64);
                let bytes = f.to_le_bytes();
                self.code.extend_from_slice(&bytes);
                self.code.push(INSTR_ALLOC_FLOAT);
            },
            Literal::Char(c) => {
                let val = Value::char(*c);
                self.code.push(INSTR_LOAD_IMM32);
                self.code.extend_from_slice(&val.val.to_le_bytes()[0..4]);
            },
            Literal::String(str) => {
                let bytes = str.as_bytes().to_vec();
                self.compile_literal(&Literal::Bytearray(bytes))?;
                self.code.push(INSTR_GLOBAL_ENV);
                self.compile_literal(&Literal::Symbol(Symbol::from("string")))?;
                self.code.push(INSTR_TABLE_GET);
                self.code.push(INSTR_SET_TYPE);
            },
            Literal::Symbol(global_symbol) => {
                let s = self.get_symbol_slot(global_symbol);
                self.load_constant_int(s as i64);
                self.code.push(INSTR_GET_LOCAL);
            },
            Literal::Bytearray(items) => {
                self.code.push(INSTR_LOAD_BYTES_IMM);
                self.code.extend_from_slice(&(items.len() as u32).to_le_bytes());
                self.code.extend_from_slice(&items);
            },
        }
        Ok(())
    }

    fn get_symbol_slot(&mut self, sym:&Symbol) -> usize {
        if let Some(loc) = self.module.want_symbols.get(sym) {
            *loc
        } else {
            let v = self.fresh_env_location();
            self.module.want_symbols.insert(*sym, v);
            v
        }
    }

    fn fresh_env_location(&mut self) -> usize {
        let n = self.next_loc;
        self.next_loc += 1;
        n
    }

    fn load_constant_int(&mut self, n:i64) {
        let v = Value::integer(n);
        let b = v.val.leading_zeros();
        match b {
            56.. => {
                self.code.push(INSTR_LOAD_IMM8);
                self.code.push(v.val.to_le_bytes()[0]);
            }
            48..56 => {
                self.code.push(INSTR_LOAD_IMM16);
                self.code.extend_from_slice(&v.val.to_le_bytes()[0..2]);
            }
            32..48 => {
                self.code.push(INSTR_LOAD_IMM32);
                self.code.extend_from_slice(&v.val.to_le_bytes()[0..4]);
            }
            _ => {
                self.code.push(INSTR_LOAD_IMM64);
                self.code.extend_from_slice(&v.val.to_le_bytes()[0..8]);
            }
        }
    }
}
