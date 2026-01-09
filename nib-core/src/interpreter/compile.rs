use crate::ast::Literal;
use crate::common::{Metadata, Name};
use crate::common::{Result, Symbol};
use crate::core::{Binder, Binding, Cond, Expression, Lambda, free_vars};
use crate::interpreter::bytecode::{
    INSTR_ALLOC_FLOAT, INSTR_CALL, INSTR_CALL_TAIL, INSTR_GET_LOCAL, INSTR_GLOBAL_ENV,
    INSTR_JFALSE, INSTR_JFALSE_IMM8, INSTR_JNEG, INSTR_JNEG_IMM8, INSTR_JNFALSE,
    INSTR_JNFALSE_IMM8, INSTR_JNNEG, INSTR_JNNEG_IMM8, INSTR_JNPOS, INSTR_JNPOS_IMM8, INSTR_JPOS,
    INSTR_JPOS_IMM8, INSTR_JUMP, INSTR_JUMP_IMM8, INSTR_JZ, INSTR_JZ_IMM8, INSTR_LOAD_BYTES_IMM,
    INSTR_LOAD_IMM8, INSTR_LOAD_IMM16, INSTR_LOAD_IMM32, INSTR_LOAD_IMM64, INSTR_SET_TYPE, INSTR_TABLE_GET,
};
use crate::interpreter::heap::Value;
use std::collections::{HashMap, HashSet};
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
    local_vars: Vec<(Symbol, usize)>,
    max_var: usize,
    used_vars: HashSet<usize>,
    free_vars: HashSet<usize>,
    code: Vec<u8>,
    is_tail: bool,
}

impl Compilation {
    pub(super) fn new(from: crate::core::Module) -> Self {
        let metadata = from.metadata;
        let bindings = from.bindings;
        let mut compilation = Compilation {
            module: Module::new(),
            core_bindings: bindings,
            max_var:0,
            local_vars: Vec::new(),
            used_vars: HashSet::new(),
            free_vars: HashSet::new(),
            code: Vec::new(),
            is_tail: true,
        };
        compilation.module.metadata = Some(metadata);
        compilation
    }

    pub(super) fn compile(&mut self) -> Result<()> {
        let bindings = mem::replace(&mut self.core_bindings, vec![]);
        let mut code = Vec::new();

        for b in bindings {
            self.is_tail = true;
            self.compile_binding(&b, true, &mut code)?;
        }
        self.module.bcode = code;
        Ok(())
    }

    fn compile_binding(
        &mut self,
        binding: &Binding,
        top_level: bool,
        code: &mut Vec<u8>,
    ) -> Result<()> {
        self.compile_expression(&binding.body, code)?;
        match &binding.binder {
            Binder::Public(Name::Qualified(path, name)) => todo!(),
            Binder::Public(Name::Plain(name)) => {
                todo!()
            }
            Binder::Local(Name::Qualified(path, name)) => {
                todo!()
            }
            Binder::Local(Name::Plain(name)) => {
                todo!()
            }
            Binder::Unbound => Ok(()),
        }
    }

    fn compile_expression(&mut self, expression: &Expression, code: &mut Vec<u8>) -> Result<()> {
        match expression {
            Expression::Literal(_, literal) => self.compile_literal(literal, code),
            Expression::Var(_, var) => {
                todo!()
            }
            Expression::Lambda(_, lambda) => {
                let mut free = HashSet::new();
                let mut locals = HashMap::new();
                free_vars(expression, &mut free, &mut locals);
                self.compile_lambda(lambda, free, locals, code)
            }
            Expression::Cond(_, cond) => self.compile_cond(cond, code),
            Expression::App(_, expressions) => self.compile_application(&expressions, code),
            Expression::Where(_, expression, bindings) => {
                self.compile_where(&expression, bindings, code)
            }
        }
    }

    fn compile_literal(&mut self, literal: &Literal, code: &mut Vec<u8>) -> Result<()> {
        match literal {
            Literal::Nil => {
                code.push(INSTR_LOAD_IMM8);
                code.push(0x36);
            }
            Literal::Bool(b) => {
                code.push(INSTR_LOAD_IMM8);
                code.push(if *b { 0x2e } else { 0x26 });
            }
            Literal::Integer(n) => {
                load_constant_int(*n, code);
            }
            Literal::Real(f) => {
                code.push(INSTR_LOAD_IMM64);
                let bytes = f.to_le_bytes();
                code.extend_from_slice(&bytes);
                code.push(INSTR_ALLOC_FLOAT);
            }
            Literal::Char(c) => {
                let val = Value::char(*c);
                code.push(INSTR_LOAD_IMM32);
                code.extend_from_slice(&val.val.to_le_bytes()[0..4]);
            }
            Literal::String(str) => {
                let bytes = str.as_bytes().to_vec();
                self.compile_literal(&Literal::Bytearray(bytes), code)?;
                code.push(INSTR_GLOBAL_ENV);
                self.compile_literal(&Literal::Symbol(Symbol::from("string")), code)?;
                code.push(INSTR_TABLE_GET);
                code.push(INSTR_SET_TYPE);
            }
            Literal::Symbol(global_symbol) => {
                let s = self.get_symbol_slot(global_symbol);
                load_constant_int(s as i64, code);
                code.push(INSTR_GET_LOCAL);
            }
            Literal::Bytearray(items) => {
                code.push(INSTR_LOAD_BYTES_IMM);
                code.extend_from_slice(&(items.len() as u32).to_le_bytes());
                code.extend_from_slice(&items);
            }
        }
        Ok(())
    }

    fn compile_lambda(
        &mut self,
        lambda: &Box<Lambda>,
        vars: HashSet<Symbol>,
        locals: HashMap<Symbol, i32>,
        code: &mut Vec<u8>,
    ) -> Result<()> {
        Ok(())
    }

    fn compile_cond(&mut self, cond: &Cond, code: &mut Vec<u8>) -> Result<()> {
        let mut if_true_code = Vec::new();
        let mut if_false_code = Vec::new();
        self.compile_expression(&cond.pred, code)?;
        self.compile_expression(&cond.if_true, &mut if_true_code)?;
        self.compile_expression(&cond.if_false, &mut if_false_code)?;
        optimized_jump(
            INSTR_JUMP,
            if_false_code.len() as i64 + 1,
            &mut if_true_code,
        );
        optimized_jump(INSTR_JFALSE, if_true_code.len() as i64 + 1, code);
        code.extend_from_slice(&if_true_code);
        code.extend_from_slice(&if_false_code);
        Ok(())
    }

    fn compile_application(&mut self, exps: &[Expression], code: &mut Vec<u8>) -> Result<()> {
        let is_tail = self.is_tail;
        self.is_tail = false;
        for e in exps {
            self.compile_expression(e, code)?;
        }
        load_constant_int(exps.len() as i64, code);
        code.push(if is_tail { INSTR_CALL_TAIL } else { INSTR_CALL });
        Ok(())
    }

    fn compile_where(
        &mut self,
        exp: &Expression,
        bindings: &[Binding],
        code: &mut Vec<u8>,
    ) -> Result<()> {
        let is_tail = self.is_tail;
        for b in bindings {
            self.compile_binding(b, false, code)?;
        }
        self.is_tail = is_tail;
        self.compile_expression(exp, code)
    }

    fn get_symbol_slot(&mut self, sym: &Symbol) -> usize {
        if let Some(loc) = self.module.want_symbols.get(sym) {
            *loc
        } else {
            let v = self.fresh_env_location();
            self.module.want_symbols.insert(*sym, v);
            v
        }
    }

    fn lookup_var(&mut self, var: &Symbol) -> usize {
        for (sym, addr) in &self.local_vars {
            if var == sym {
                return *addr;
            }
        }
        if let Some(v) = self.module.captures.get(var) {
            *v
        } else {
            let v = self.fresh_env_location();
            self.module.captures.insert(*var, v);
            v
        }
    }

    fn free_local_var(&mut self, n:usize) {
        self.used_vars.remove(&n);
        self.free_vars.insert(n);
    }

    fn env_location(&mut self) -> usize {
        let val = self.free_vars.iter().next().map(|u| *u);
        if let Some(loc) = val {
            self.free_vars.remove(&loc);
            loc
        } else {
            self.fresh_env_location()
        }
    }

    fn fresh_env_location(&mut self) -> usize {
        let n = self.max_var;
        self.used_vars.insert(n);
        self.max_var += 1;
        n
    }
}

fn load_constant_int(n: i64, code: &mut Vec<u8>) {
    let v = Value::integer(n);
    let b = v.val.leading_zeros();
    match b {
        56.. => {
            code.push(INSTR_LOAD_IMM8);
            code.push(v.val.to_le_bytes()[0]);
        }
        48..56 => {
            code.push(INSTR_LOAD_IMM16);
            code.extend_from_slice(&v.val.to_le_bytes()[0..2]);
        }
        32..48 => {
            code.push(INSTR_LOAD_IMM32);
            code.extend_from_slice(&v.val.to_le_bytes()[0..4]);
        }
        _ => {
            code.push(INSTR_LOAD_IMM64);
            code.extend_from_slice(&v.val.to_le_bytes()[0..8]);
        }
    }
}

fn optimized_jump(op: u8, n: i64, code: &mut Vec<u8>) {
    if let Some(b) = i8::try_from(n).ok() {
        let sop = short_jump_op(op);
        code.push(op);
        code.push(n as u8);
    } else {
        load_constant_int(n, code);
        code.push(op);
    }
}

fn short_jump_op(op: u8) -> u8 {
    match op {
        INSTR_JUMP => INSTR_JUMP_IMM8,
        INSTR_JFALSE => INSTR_JFALSE_IMM8,
        INSTR_JNEG => INSTR_JNEG_IMM8,
        INSTR_JNFALSE => INSTR_JNFALSE_IMM8,
        INSTR_JNNEG => INSTR_JNNEG_IMM8,
        INSTR_JNPOS => INSTR_JNPOS_IMM8,
        INSTR_JPOS => INSTR_JPOS_IMM8,
        INSTR_JZ => INSTR_JZ_IMM8,
        _ => op,
    }
}
