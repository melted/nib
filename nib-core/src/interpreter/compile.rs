use symbol_table::static_symbol;

use crate::ast::Literal;
use crate::common::{Metadata, Name};
use crate::common::{Result, Symbol};
use crate::core::{Binder, Binding, Cond, Expression, Lambda, free_vars};
use crate::interpreter::bytecode::{
    INSTR_ALLOC_ARRAY, INSTR_ALLOC_CLOSURE, INSTR_ALLOC_FLOAT, INSTR_ALLOC_TABLE, INSTR_ARRAY_SET,
    INSTR_CALL, INSTR_CALL_TAIL, INSTR_DROP, INSTR_DUP, INSTR_GET_LOCAL, INSTR_GLOBAL_ENV,
    INSTR_IS_TABLE, INSTR_JFALSE, INSTR_JFALSE_IMM8, INSTR_JNEG, INSTR_JNEG_IMM8, INSTR_JNFALSE,
    INSTR_JNFALSE_IMM8, INSTR_JNNEG, INSTR_JNNEG_IMM8, INSTR_JNPOS, INSTR_JNPOS_IMM8, INSTR_JPOS,
    INSTR_JPOS_IMM8, INSTR_JUMP, INSTR_JUMP_IMM8, INSTR_JZ, INSTR_JZ_IMM8, INSTR_LOAD_BYTES_IMM,
    INSTR_LOAD_IMM8, INSTR_LOAD_IMM16, INSTR_LOAD_IMM32, INSTR_LOAD_IMM64, INSTR_RETURN,
    INSTR_SET_LOCAL, INSTR_SET_TYPE, INSTR_STACK_LOAD, INSTR_TABLE_GET, INSTR_TABLE_SET,
};
use crate::interpreter::heap::Value;
use crate::interpreter::prims::is_bytecode_primitive;
use crate::interpreter::stack_return;
use std::collections::{HashMap, HashSet};
use std::mem;

pub fn compile(from: crate::core::Module) -> Result<Module> {
    let module = Module::new();
    let mut compilation = Compilation::with(from);
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
    /// Possible metadata acquired when parsing and compiling
    /// this module.
    pub metadata: Option<Metadata>,
    /// The bytecode
    pub byte_code: Vec<u8>,
    /// Size of local environment
    pub local_env_size: usize,
    /// A list of symbol literals that should be put into the local environment.
    pub want_symbols: HashMap<Symbol, usize>,
    /// Global variables used by the module.
    pub captures: HashMap<Symbol, usize>,
}

impl Module {
    pub fn new() -> Self {
        Module {
            metadata: None,
            byte_code: Vec::new(),
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
    input: CompilationInput,
    local_vars: Vec<(Symbol, usize)>,
    stack_vars: Vec<(Symbol, usize)>,

    /// Future bindings in this scope, so we can bind to them instead of trying to import a global
    /// of the same name.
    future_bindings: HashSet<Symbol>,
    /// This is requested fixups for bindings not yet in scope when a lambda was defined, the first usize
    /// is where the lambdas environment is in the local environment and the second is the offset in the
    /// lambda's environment
    fixups_needed: HashMap<Symbol, Vec<(usize, usize)>>,
    max_var: usize,
    used_vars: HashSet<usize>,
    free_vars: HashSet<usize>,
    is_tail: bool,
}

#[derive(Debug, Default, Clone)]
pub enum CompilationInput {
    #[default]
    Nothing,
    Bindings(Vec<Binding>),
    Expression(Expression),
}

impl Compilation {
    pub(super) fn new() -> Self {
        Compilation {
            module: Module::new(),
            input: CompilationInput::Nothing,
            stack_vars: Vec::new(),
            max_var: 0,
            local_vars: Vec::new(),
            future_bindings: HashSet::new(),
            fixups_needed: HashMap::new(),
            used_vars: HashSet::new(),
            free_vars: HashSet::new(),
            is_tail: true,
        }
    }

    pub(super) fn with(module: crate::core::Module) -> Self {
        let mut compilation = Compilation::new();
        compilation.input = CompilationInput::Bindings(module.bindings);
        compilation.module.metadata = Some(module.metadata);
        compilation
    }

    pub(super) fn compile(&mut self) -> Result<()> {
        let input = mem::take(&mut self.input);
        let mut code = Vec::new();

        match input {
            CompilationInput::Nothing => {}
            CompilationInput::Bindings(bindings) => {
                self.collect_binding_names(&bindings);
                for b in bindings {
                    self.compile_binding(&b, true, &mut code)?;
                }
                // Return nothing
                push_nil(&mut code);
            }
            CompilationInput::Expression(expression) => {
                self.compile_expression(&expression, &mut code)?;
            }
        }
        code.push(INSTR_RETURN);
        self.module.byte_code = code;
        self.module.local_env_size = self.max_var + 1;
        Ok(())
    }

    fn compile_binding(
        &mut self,
        binding: &Binding,
        top_level: bool,
        code: &mut Vec<u8>,
    ) -> Result<()> {
        self.is_tail = true;
        self.compile_expression(&binding.body, code)?;
        match &binding.binder {
            Binder::Public(Name::Qualified(path, name)) => {
                self.push_symbol(name, code);
                let get_path = static_symbol!("_prim_get_path");
                self.get_global_name(&get_path, code);
                for s in path {
                    self.push_symbol(s, code);
                }
                load_constant_int((path.len() + 1) as i64, code);
                code.push(INSTR_CALL);
                code.push(INSTR_TABLE_SET);
                self.check_fixups(&path[0], code);
            }
            Binder::Public(Name::Plain(name)) => {
                self.push_symbol(name, code);
                code.push(INSTR_GLOBAL_ENV);
                code.push(INSTR_TABLE_SET);
                self.check_fixups(name, code);
            }
            Binder::Local(Name::Qualified(path, name)) => {
                self.push_symbol(name, code);
                let get_path = static_symbol!("_prim_get_path");
                self.get_global_name(&get_path, code);
                let (first, tail) = path.split_at(1);
                self.get_local_table(&first[0], code);
                for s in path {
                    self.push_symbol(s, code);
                }
                load_constant_int((path.len() + 1) as i64, code);
                code.push(INSTR_CALL);
                code.push(INSTR_TABLE_SET);
                self.check_fixups(&path[0], code);
            }
            Binder::Local(Name::Plain(name)) => {
                let h = if top_level {
                    self.local_top_var(&name)
                } else {
                    self.env_location()
                };
                load_constant_int(h as i64, code);
                code.push(INSTR_SET_LOCAL);
                self.check_fixups(name, code);
            }
            Binder::Unbound => {}
        }
        Ok(())
    }

    fn check_fixups(&mut self, name: &Symbol, code: &mut Vec<u8>) {
        if let Some(fixups) = self.fixups_needed.remove(name) {
            self.get_variable(name, code);
            for fix in fixups {
                code.push(INSTR_DUP);
                load_constant_int(fix.0 as i64, code);
                code.push(INSTR_GET_LOCAL);
                code.push(INSTR_ARRAY_SET);
            }
            code.push(INSTR_DROP);
        }
    }

    fn collect_binding_names(&mut self, bindings: &[Binding]) {
        for b in bindings {
            match &b.binder {
                Binder::Public(name) | Binder::Local(name) => {
                    self.future_bindings.insert(name.top());
                }
                Binder::Unbound => {}
            }
        }
    }

    fn compile_expression(&mut self, expression: &Expression, code: &mut Vec<u8>) -> Result<()> {
        match expression {
            Expression::Literal(_, literal) => self.compile_literal(literal, code),
            Expression::Var(_, var) => {
                match self.lookup_var(var) {
                    VarLocation::Stack(loc) => {
                        load_constant_int(loc as i64, code);
                        code.push(INSTR_STACK_LOAD);
                    }
                    VarLocation::Env(loc) => {
                        load_constant_int(loc as i64, code);
                        code.push(INSTR_GET_LOCAL);
                    }
                }
                Ok(())
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
                push_nil(code);
            }
            Literal::Bool(b) => {
                push_bool(*b, code);
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
                push_bytes(&items, code);
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
        let mut fun_compilation = Compilation::new();
        for (i, arg) in lambda.args.iter().enumerate() {
            fun_compilation.stack_vars.push((*arg, i + 1));
        }
        fun_compilation.input = CompilationInput::Expression(lambda.body.clone());
        fun_compilation.compile()?;
        let (arity, vararg) = match lambda.arity {
            crate::core::Arity::Fixed(n) => (Value::integer(n as i64), Value::bool(false)),
            crate::core::Arity::VarArg(n, i) => {
                (Value::integer(n as i64), Value::integer(i as i64))
            }
        };
        load_constant_value(&vararg, code);
        load_constant_value(&arity, code);
        load_constant_int(fun_compilation.module.local_env_size as i64, code);
        code.push(INSTR_ALLOC_ARRAY);

        for (sym, index) in fun_compilation.module.want_symbols {
            code.push(INSTR_DUP);
            let slot = self.get_symbol_slot(&sym);
            load_constant_int(index as i64, code);
            load_constant_int(slot as i64, code);
            code.push(INSTR_GET_LOCAL);
            code.push(INSTR_ARRAY_SET);
        }
        let mut captures_var = None;
        for (var, index) in fun_compilation.module.captures {
            code.push(INSTR_DUP);
            load_constant_int(index as i64, code);
            if self.future_bindings.contains(&var) {
                let v = if let Some(v) = captures_var {
                    v
                } else {
                    let n = self.env_location();
                    captures_var = Some(n);
                    n
                };
                self.fixups_needed
                    .entry(var)
                    .and_modify(|vec| vec.push((v, index)))
                    .or_insert_with(|| vec![(v, index)]);
            } else {
                self.get_variable(&var, code);
                code.push(INSTR_ARRAY_SET);
            }
        }
        push_bytes(&fun_compilation.module.byte_code, code);
        code.push(INSTR_ALLOC_CLOSURE);
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
        let callee = &exps[0];
        let bytecode_prim = if let Expression::Var(_, sym) = callee {
            is_bytecode_primitive(sym)
        } else {
            self.compile_expression(callee, code)?;
            None
        };
        for e in &exps[1..] {
            self.compile_expression(e, code)?;
        }
        match bytecode_prim {
            Some(op) => {
                code.push(op);
                if stack_return(op) == 0 {
                    push_nil(code);
                }
            }
            None => {
                load_constant_int(exps.len() as i64, code);
                code.push(if is_tail { INSTR_CALL_TAIL } else { INSTR_CALL });
            }
        }
        Ok(())
    }

    fn compile_where(
        &mut self,
        exp: &Expression,
        bindings: &[Binding],
        code: &mut Vec<u8>,
    ) -> Result<()> {
        let mut old_fixups = HashMap::new();
        let mut old_future_bindings = HashSet::new();
        let mut old_used_vars = self.used_vars.clone();
        let mut old_free_vars = self.free_vars.clone();
        mem::swap(&mut old_fixups, &mut self.fixups_needed);
        mem::swap(&mut old_future_bindings, &mut self.future_bindings);
        let is_tail = self.is_tail;
        self.collect_binding_names(&bindings);
        for b in bindings {
            self.compile_binding(b, false, code)?;
        }
        self.is_tail = is_tail;
        self.compile_expression(exp, code)?;
        mem::swap(&mut old_fixups, &mut self.fixups_needed);
        mem::swap(&mut old_future_bindings, &mut self.future_bindings);
        mem::swap(&mut old_used_vars, &mut self.used_vars);
        mem::swap(&mut old_free_vars, &mut self.free_vars);
        Ok(())
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

    fn push_symbol(&mut self, sym: &Symbol, code: &mut Vec<u8>) {
        let sym_addr = self.get_symbol_slot(sym);
        load_constant_int(sym_addr as i64, code);
        code.push(INSTR_GET_LOCAL);
    }

    fn get_global_name(&mut self, sym: &Symbol, code: &mut Vec<u8>) {
        self.push_symbol(&sym, code);
        code.push(INSTR_GLOBAL_ENV);
        code.push(INSTR_TABLE_GET);
    }

    fn get_local_table(&mut self, sym: &Symbol, code: &mut Vec<u8>) {
        let h = self.local_top_var(&sym);
        load_constant_int(h as i64, code);
        code.push(INSTR_GET_LOCAL);
        code.push(INSTR_DUP);
        code.push(INSTR_IS_TABLE);
        let mut create_table_code = Vec::new();
        create_table_code.push(INSTR_ALLOC_TABLE);
        load_constant_int(h as i64, &mut create_table_code);
        create_table_code.push(INSTR_SET_LOCAL);
        optimized_jump(INSTR_JFALSE, (create_table_code.len() + 1) as i64, code);
        code.extend_from_slice(&create_table_code);
    }

    fn get_variable(&mut self, var: &Symbol, code: &mut Vec<u8>) {
        let loc = self.lookup_var(&var);
        match loc {
            VarLocation::Stack(s) => {
                load_constant_int(s as i64, code);
                code.push(INSTR_STACK_LOAD);
            }
            VarLocation::Env(i) => {
                load_constant_int(i as i64, code);
                code.push(INSTR_GET_LOCAL);
            }
        }
    }

    fn lookup_var(&mut self, var: &Symbol) -> VarLocation {
        for (sym, loc) in &self.stack_vars {
            if var == sym {
                return VarLocation::Stack(*loc);
            }
        }
        for (sym, loc) in self.local_vars.iter().rev() {
            if var == sym {
                return VarLocation::Env(*loc);
            }
        }
        if let Some(v) = self.module.captures.get(var) {
            VarLocation::Env(*v)
        } else {
            let v = self.fresh_env_location();
            self.module.captures.insert(*var, v);
            VarLocation::Env(v)
        }
    }

    fn local_top_var(&mut self, sym: &Symbol) -> usize {
        for (var, loc) in self.local_vars.iter().rev() {
            if var == sym {
                return *loc;
            }
        }
        let nvar = self.fresh_env_location();
        self.local_vars.push((*sym, nvar));
        nvar
    }

    fn free_local_var(&mut self, n: usize) {
        self.used_vars.remove(&n);
        self.free_vars.insert(n);
    }

    fn env_location(&mut self) -> usize {
        let val = self.free_vars.iter().next().map(|u| *u);
        if let Some(loc) = val {
            self.free_vars.remove(&loc);
            self.used_vars.insert(loc);
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

enum VarLocation {
    Stack(usize),
    Env(usize),
}

fn load_constant_value(v: &Value, code: &mut Vec<u8>) {
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

fn load_constant_int(n: i64, code: &mut Vec<u8>) {
    let v = Value::integer(n);
    load_constant_value(&v, code);
}

fn push_nil(code: &mut Vec<u8>) {
    code.push(INSTR_LOAD_IMM8);
    code.push(0x36);
}

fn push_bool(b: bool, code: &mut Vec<u8>) {
    code.push(INSTR_LOAD_IMM8);
    code.push(if b { 0x2e } else { 0x26 });
}

fn push_bytes(items: &[u8], code: &mut Vec<u8>) {
    code.push(INSTR_LOAD_BYTES_IMM);
    code.extend_from_slice(&(items.len() as u32).to_le_bytes());
    code.extend_from_slice(&items);
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
