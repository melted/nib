use symbol_table::static_symbol;

use crate::ast::Literal;
use crate::common::{Location, Metadata, Name, sym, symbol_id};
use crate::common::{Result, Symbol};
use crate::core::{Binder, Binding, Cond, Expression, Function};
use crate::interpreter::bytecode::{
    INSTR_ALLOC_ARRAY, INSTR_ALLOC_CLOSURE, INSTR_ALLOC_FLOAT, INSTR_ALLOC_TABLE, INSTR_ARRAY_SET,
    INSTR_CALL, INSTR_CALL_TAIL, INSTR_DROP, INSTR_DUP, INSTR_GET_LOCAL, INSTR_GLOBAL_ENV,
    INSTR_IS_TABLE, INSTR_JFALSE, INSTR_JFALSE_IMM8, INSTR_JNEG, INSTR_JNEG_IMM8, INSTR_JNFALSE,
    INSTR_JNFALSE_IMM8, INSTR_JNNEG, INSTR_JNNEG_IMM8, INSTR_JNPOS, INSTR_JNPOS_IMM8, INSTR_JPOS,
    INSTR_JPOS_IMM8, INSTR_JUMP, INSTR_JUMP_IMM8, INSTR_JZ, INSTR_JZ_IMM8, INSTR_LOAD_BYTES_IMM,
    INSTR_LOAD_IMM8, INSTR_LOAD_IMM16, INSTR_LOAD_IMM32, INSTR_LOAD_IMM64, INSTR_MAKE_SYMBOL,
    INSTR_PUSH_FALSE, INSTR_PUSH_LAST_SMALL, INSTR_PUSH_MINUS_ONE, INSTR_PUSH_NIL, INSTR_PUSH_TRUE,
    INSTR_RETURN, INSTR_SET_LOCAL, INSTR_SET_TYPE, INSTR_STACK_LOAD, INSTR_TABLE_GET,
    INSTR_TABLE_SET,
};
use crate::interpreter::heap::{Value, ValueRepr};
use crate::interpreter::prims::is_bytecode_primitive;
use crate::interpreter::stack_return;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::mem;
use std::sync::LazyLock;

pub fn compile(from: crate::core::Module) -> Result<Module> {
    let module = Module::new();
    let mut compilation = Compilation::with(from);
    compilation.compile()?;
    Ok(compilation.module)
}

pub fn compile_expression(expr: crate::core::Expression) -> Result<Module> {
    let mut compilation = Compilation::with_expression(expr);
    compilation.compile()?;
    Ok(compilation.module)
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
    /// Byte arrays to be put on the heap and made available in the local env
    pub data: HashMap<Vec<u8>, usize>,
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

impl Module {
    pub fn new() -> Self {
        Module {
            metadata: None,
            byte_code: Vec::new(),
            local_env_size: 0,
            data: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct Context {
    pub local_vars: Vec<(Symbol, usize)>,
    pub stack_vars: Vec<(Symbol, usize)>,
    pub max_var: usize,
    pub used_locs: HashSet<usize>,
    pub free_locs: HashSet<usize>,
}

impl Context {
    fn new() -> Self {
        Context {
            local_vars: Vec::new(),
            stack_vars: Vec::new(),
            max_var: 0,
            used_locs: HashSet::new(),
            free_locs: HashSet::new(),
        }
    }

    fn lookup_var(&self, var: &Symbol) -> VarLocation {
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
        VarLocation::Global
    }

    fn local_var(&mut self, sym: &Symbol, fresh:bool) -> usize {
        for (var, loc) in self.local_vars.iter().rev() {
            if var == sym {
                return *loc;
            }
        }
        let nvar = if fresh {
            self.fresh_env_location()
        } else {
            self.env_location()
        };
        self.local_vars.push((*sym, nvar));
        nvar
    }

    fn free_location(&mut self, n: usize) {
        self.used_locs.remove(&n);
        self.free_locs.insert(n);
    }

    fn env_location(&mut self) -> usize {
        let val = self.free_locs.iter().next().copied();
        if let Some(loc) = val {
            self.free_locs.remove(&loc);
            self.used_locs.insert(loc);
            loc
        } else {
            self.fresh_env_location()
        }
    }

    fn fresh_env_location(&mut self) -> usize {
        let n = self.max_var;
        self.used_locs.insert(n);
        self.max_var += 1;
        n
    }
}

/// State held during compilation. Everything that can be discarded when finished
/// goes here.
#[derive(Debug, Clone)]
pub(super) struct Compilation {
    module: Module,
    input: CompilationInput,
    contexts: Vec<Context>,

    /// Future bindings in this scope, so we can bind to them instead of trying to import a
    ///  global of the same name.
    future_bindings: HashSet<Symbol>,
    /// This is requested fixups for bindings not yet in scope when a lambda was defined, the
    /// first usize is where the lambdas environment is in the local environment and the
    /// second is the offset in the lambda's environment
    fixups_needed: HashMap<Symbol, Vec<(usize, usize)>>,
    data_symbols: HashMap<Vec<u8>, Symbol>,
    data: HashMap<Symbol, (Vec<u8>, usize)>,
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
            contexts: vec![Context::new()],
            input: CompilationInput::Nothing,
            future_bindings: HashSet::new(),
            fixups_needed: HashMap::new(),
            data_symbols: HashMap::new(),
            data: HashMap::new(),
            is_tail: true,
        }
    }

    pub(super) fn with(module: crate::core::Module) -> Self {
        let mut compilation = Compilation::new();
        compilation.input = CompilationInput::Bindings(module.bindings);
        compilation.module.metadata = Some(module.metadata);
        compilation
    }

    pub(super) fn with_expression(expr: crate::core::Expression) -> Self {
        let mut compilation = Compilation::new();
        compilation.input = CompilationInput::Expression(expr);
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
                if !self.fixups_needed.is_empty() {
                    let f: Vec<Symbol> = self
                        .fixups_needed
                        .iter()
                        .map(|fix| fix.0)
                        .copied()
                        .collect();
                    return self.error(&format!("Missing definition of `{:?}`", f));
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
        self.module.local_env_size = self.current_context().max_var + 1;
        Ok(())
    }

    fn compile_binding(
        &mut self,
        binding: &Binding,
        top_level: bool,
        code: &mut Vec<u8>,
    ) -> Result<()> {
        self.is_tail = true;
        let binding_name = self.get_binding_name(&binding.binder);
        let global = matches!(&binding.binder, Binder::Public(_));
        if let Some(n) = &binding_name
            && !global
        {
            let l = self.current_context().local_var(&n.top(), false);
        }
        self.compile_expression(&binding.body, code)?;
        if let Some(name) = binding_name {
            let top = name.top();
            let path = name.path();
            if path.is_empty() {
                if global {
                    self.get_symbol(&top, code);
                    code.push(INSTR_GLOBAL_ENV);
                    code.push(INSTR_TABLE_SET);
                } else {
                    let loc = self.current_context().local_var(&top, false);
                    set_local(loc, code);
                }
            } else {
                let rest = &path[1..];
                let last = name.base();
                self.get_symbol(&last, code);
                self.compile_get_path(code);
                if global {
                    self.get_symbol(&top, code);
                } else {
                    self.get_local_table(&top, code);
                }
                for s in rest {
                    self.get_symbol(s, code);
                }
                load_constant_int((rest.len() + 2) as i64, code);
                code.push(INSTR_CALL);
                code.push(INSTR_TABLE_SET);
            }
            if !global {
                self.future_bindings.remove(&top);
            }
            self.check_fixups(&top, code);
        } else {
            code.push(INSTR_DROP);
        }
        Ok(())
    }

    fn get_binding_name(&mut self, binder: &Binder) -> Option<Name> {
        match binder {
            Binder::Public(name) => Some(name.clone()),
            Binder::Local(name) => Some(name.clone()),
            Binder::Unbound => None,
        }
    }

    fn compile_get_path(&mut self, code: &mut Vec<u8>) {
        let get_path = static_symbol!("_prim_get_path");
        self.get_global_name(&get_path, code);
    }

    fn check_fixups(&mut self, name: &Symbol, code: &mut Vec<u8>) {
        if let Some(fixups) = self.fixups_needed.remove(name) {
            self.get_variable(name, code);
            for fix in fixups {
                code.push(INSTR_DUP);
                load_constant_int(fix.1 as i64, code);
                get_local(fix.0, code);
                code.push(INSTR_ARRAY_SET);
            }
            code.push(INSTR_DROP);
        }
    }

    fn collect_binding_names(&mut self, bindings: &[Binding]) {
        for b in bindings {
            match &b.binder {
                Binder::Local(name) => {
                    self.future_bindings.insert(name.top());
                }
                _ => {}
            }
        }
    }

    fn compile_expression(&mut self, expression: &Expression, code: &mut Vec<u8>) -> Result<()> {
        match expression {
            Expression::Literal(_, literal) => self.compile_literal(literal, code),
            Expression::Var(_, var) => {
                self.get_variable(var, code);
                Ok(())
            }
            Expression::Cond(_, cond) => self.compile_cond(cond, code),
            Expression::App(_, expressions) => self.compile_application(expressions, code),
            Expression::Function(fun) => self.compile_function(fun, code),
            Expression::Where(_, expression, bindings) => {
                self.compile_where(expression, bindings, code)
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
                load_constant_value(&Value::char(*c), code);
            }
            Literal::String(str) => {
                let bytes = str.as_bytes().to_vec();
                self.compile_literal(&Literal::Bytearray(bytes), code)?;
                code.push(INSTR_DUP);
                self.compile_literal(&Literal::Symbol(Symbol::from("string")), code)?;
                code.push(INSTR_GLOBAL_ENV);
                code.push(INSTR_TABLE_GET);
                code.push(INSTR_SET_TYPE);
            }
            Literal::Symbol(sym) => {
                self.get_symbol(sym, code);
            }
            Literal::Bytearray(data) => {
                let sym = self.get_data_symbol(data);
                self.set_data_slot(&sym, data);
                self.get_variable(&sym, code);
            }
        }
        Ok(())
    }

    fn get_all_captures(&mut self, lambda: &Function) -> Vec<Symbol> {
        let mut captures = Vec::new();
        captures.extend(lambda.captures.clone());
        captures.extend(lambda.code_captures.clone());
        for lit in &lambda.literal_captures {
            captures.push(self.get_literal_symbol(lit));
        }
        captures
    }

    fn compile_function(&mut self, lambda: &Function, code: &mut Vec<u8>) -> Result<()> {
        // Make sure these exist outside the function before capture
        for lit in &lambda.literal_captures {
            let _ = self.get_literal_symbol(lit);
        }
        for c in &lambda.code_captures {
            let _ = self.current_context().local_var(c, true);
        }
        let mut old_future_bindings = HashSet::new();
        self.contexts.push(Context::new());
        mem::swap(&mut old_future_bindings, &mut self.future_bindings);
        for (i, arg) in lambda.args.iter().enumerate() {
            self.current_context().stack_vars.push((*arg, i + 1));
        }
        let captures = self.get_all_captures(lambda);
        let mut addrs = Vec::new();
        for cap in captures.iter() {
            let addr = self.current_context().local_var(cap, true);
            addrs.push((*cap, addr));
        }
        let mut fun_code = Vec::new();
        self.compile_expression(&lambda.body, &mut fun_code)?;
        fun_code.push(INSTR_RETURN);
        let env_size = self.current_context().max_var;
        self.contexts.pop();
        mem::swap(&mut old_future_bindings, &mut self.future_bindings);
        let (arity, vararg) = match lambda.arity {
            crate::core::Arity::Fixed(n) => (Value::integer(n as i64), Value::bool(false)),
            crate::core::Arity::VarArg(n, i) => {
                (Value::integer(n as i64), Value::integer(i as i64))
            }
        };
        load_constant(&vararg, code);
        load_constant_int(arity.get_integer(), code);
        load_constant_int(env_size as i64, code);
        code.push(INSTR_ALLOC_ARRAY);
        let env_local = self.current_context().env_location();
        set_local(env_local, code);

        for (c, i) in &addrs {
            if self.future_bindings.contains(c) {
                let addr = (env_local, *i);
                self.fixups_needed
                    .entry(*c)
                    .and_modify(|v| v.push(addr))
                    .or_insert(vec![addr]);
            } else {
                self.get_variable(c, code);
                load_constant_int(*i as i64, code);
                get_local(env_local, code);
                code.push(INSTR_ARRAY_SET);
            }
        }
        get_local(env_local, code);
        self.set_data_slot(&lambda.code_ref, &fun_code);
        self.get_variable(&lambda.code_ref, code);
        code.push(INSTR_ALLOC_CLOSURE);
        Ok(())
    }

    fn compile_cond(&mut self, cond: &Cond, code: &mut Vec<u8>) -> Result<()> {
        let mut if_true_code = Vec::new();
        let mut if_false_code = Vec::new();
        self.compile_expression(&cond.pred, code)?;
        self.compile_expression(&cond.if_true, &mut if_true_code)?;
        self.compile_expression(&cond.if_false, &mut if_false_code)?;
        optimized_jump(INSTR_JUMP, if_false_code.len() as i64, &mut if_true_code);
        optimized_jump(INSTR_JFALSE, if_true_code.len() as i64, code);
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
            None
        };
        if bytecode_prim.is_none() {
            self.compile_expression(callee, code)?;
        }
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
        let mut old_future_bindings = self.future_bindings.clone();
        let old_used_vars = self.current_context().used_locs.clone();
        mem::swap(&mut old_fixups, &mut self.fixups_needed);
        let is_tail = self.is_tail;
        self.collect_binding_names(bindings);
        for b in bindings {
            self.compile_binding(b, false, code)?;
        }
        self.is_tail = is_tail;
        self.compile_expression(exp, code)?;
        let to_free = self
            .current_context()
            .used_locs
            .difference(&old_used_vars)
            .copied()
            .collect::<Vec<_>>();
        for v in to_free {
            self.current_context().free_location(v);
        }
        if !self.fixups_needed.is_empty() {
            let f: Vec<Symbol> = self
                .fixups_needed
                .iter()
                .map(|fix| fix.0)
                .copied()
                .collect();
            return self.error(&format!("Missing definition of `{:?}`", f));
        }
        mem::swap(&mut old_fixups, &mut self.fixups_needed);
        mem::swap(&mut old_future_bindings, &mut self.future_bindings);
        Ok(())
    }

    fn compile_bytes(&mut self, data: &[u8], code: &mut Vec<u8>) {
        push_bytes(data, code);
    }

    fn set_data_slot(&mut self, sym: &Symbol, data: &[u8]) {
        if let Some(loc) = self.module.data.get(data).copied() {
            self.base_context().local_vars.push((*sym, loc));
        } else {
            let b = self.base_context().local_var(sym, true);
            self.module.data.insert(data.to_vec(), b);
        }
    }

    fn get_data_symbol(&mut self, data: &[u8]) -> Symbol {
        if let Some(sym) = self.data_symbols.get(data) {
            *sym
        } else {
            let sym = next_lit_id();
            self.data_symbols.insert(data.to_vec(), sym);
            sym
        }
    }

    fn get_symbol(&mut self, sym: &Symbol, code: &mut Vec<u8>) {
        load_constant_int(symbol_id(sym) as i64, code);
        code.push(INSTR_MAKE_SYMBOL);
    }

    fn get_literal_symbol(&mut self, lit: &Literal) -> Symbol {
        match lit {
            Literal::Bytearray(c) => self.get_data_symbol(c),
            Literal::String(s) => self.get_data_symbol(s.as_bytes()),
            _ => {
                panic!("Invalid literal in get_literal_symbol");
            }
        }
    }

    fn get_global_name(&mut self, sym: &Symbol, code: &mut Vec<u8>) {
        self.get_symbol(sym, code);
        code.push(INSTR_GLOBAL_ENV);
        code.push(INSTR_TABLE_GET);
    }

    fn get_local_table(&mut self, sym: &Symbol, code: &mut Vec<u8>) {
        let h = self.current_context().local_var(sym, true);
        get_local(h, code);
        code.push(INSTR_DUP);
        code.push(INSTR_IS_TABLE);
        let mut create_table_code = Vec::new();
        create_table_code.push(INSTR_ALLOC_TABLE);
        set_local(h, &mut create_table_code);
        optimized_jump(INSTR_JFALSE, (create_table_code.len() + 1) as i64, code);
        code.extend_from_slice(&create_table_code);
    }

    fn get_variable(&mut self, var: &Symbol, code: &mut Vec<u8>) {
        let loc = self.current_context().lookup_var(var);
        match loc {
            VarLocation::Stack(s) => {
                load_constant_int(s as i64, code);
                code.push(INSTR_STACK_LOAD);
            }
            VarLocation::Env(i) => {
                get_local(i, code);
            }
            VarLocation::Global => {
                self.get_global_name(var, code);
            }
        }
    }

    fn current_context(&mut self) -> &mut Context {
        self.contexts.last_mut().unwrap()
    }

    fn base_context(&mut self) -> &mut Context {
        self.contexts.get_mut(0).unwrap()
    }

    fn error<T>(&self, msg: &str) -> Result<T> {
        Err(crate::common::Error::Runtime {
            msg: msg.to_owned(),
            loc: None,
        })
    }
}

enum VarLocation {
    Stack(usize),
    Env(usize),
    Global,
}

fn set_local(n: usize, code: &mut Vec<u8>) {
    load_constant_int(n as i64, code);
    code.push(INSTR_SET_LOCAL);
}

fn get_local(n: usize, code: &mut Vec<u8>) {
    load_constant_int(n as i64, code);
    code.push(INSTR_GET_LOCAL);
}

fn load_constant(v: &Value, code: &mut Vec<u8>) {
    match v.get_repr() {
        ValueRepr::Nil => push_nil(code),
        ValueRepr::Bool => push_bool(v.get_bool(), code),
        ValueRepr::Integer => load_constant_int(v.get_integer(), code),
        _ => load_constant_value(v, code),
    }
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
    const LAST_SMALL: i64 = INSTR_PUSH_LAST_SMALL as i64;
    match n {
        -1 => code.push(INSTR_PUSH_MINUS_ONE),
        0..=LAST_SMALL => code.push(n as u8),
        _ => {
            let v = Value::integer(n);
            load_constant_value(&v, code);
        }
    }
}

fn push_nil(code: &mut Vec<u8>) {
    code.push(INSTR_PUSH_NIL);
}

fn push_bool(b: bool, code: &mut Vec<u8>) {
    if b {
        code.push(INSTR_PUSH_TRUE);
    } else {
        code.push(INSTR_PUSH_FALSE);
    }
}

fn push_bytes(items: &[u8], code: &mut Vec<u8>) {
    code.push(INSTR_LOAD_BYTES_IMM);
    code.extend_from_slice(&(items.len() as u32).to_le_bytes());
    code.extend_from_slice(items);
}

fn optimized_jump(op: u8, n: i64, code: &mut Vec<u8>) {
    if let Ok(b) = i8::try_from(n) {
        let sop = short_jump_op(op);
        code.push(sop);
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

pub fn next_lit_id() -> Symbol {
    unsafe {
        static mut LOCAL_VAL: LazyLock<u32> = LazyLock::new(|| 0);
        *LOCAL_VAL += 1;
        sym(&format!("$lit{}", *LOCAL_VAL))
    }
}
