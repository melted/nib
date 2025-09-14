use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    os::raw::c_void,
};

use internment::Intern;
use log::info;

use crate::{
    ast::Literal,
    common::{Name, Result},
    core::{Arity, Binder, Binding, Expression, Lambda, Module, free_vars},
    treewalker::{CType, Closure, Code, Runtime, Symbol, Value, new_ref},
};

impl Runtime {
    pub(super) fn evaluate(&mut self, code: &mut Module, env: &mut Environment) -> Result<()> {
        for b in code.bindings.iter_mut() {
            self.evaluate_binding(b, env, false)?;
            if let Some(hs) = self.closures_to_check.get(&b.name) {
                for c in hs.clone() {
                    if let Some(Value::Closure(closure)) = self.lookup(env, &c) {
                        let mut cl = closure.borrow_mut();
                        self.replace_undefined(&mut cl.env, env);
                    }
                }
                self.closures_to_check.remove(&b.name);
            }
        }
        Ok(())
    }

    pub(super) fn evaluate_binding(
        &mut self,
        binding: &Binding,
        env: &mut Environment,
        local: bool,
    ) -> Result<()> {
        info!("Evaluating binding {}", binding);
        let val = self.evaluate_expression(&binding.name, &binding.body, env)?;
        let is_closure = matches!(val, Value::Closure(_));
        match &binding.binder {
            Binder::Public(name) if local => {
                match name {
                    Name::Plain(s) => {
                        env.add(s, &val);
                    }
                    Name::Qualified(_, _) => {
                        return self.error(&format!("Qualified name {} in where clause", name));
                    }
                };
            }
            Binder::Public(name) => {
                self.add_name(name, &val)?;
            }
            Binder::Local(name) => {
                env.add(name, &val);
            }
            Binder::Unbound => {}
        }
        Ok(())
    }

    pub(super) fn evaluate_expression(
        &mut self,
        binding_name: &str,
        expression: &Expression,
        env: &mut Environment,
    ) -> Result<Value> {
        info!("Evaluating expression {}", expression);
        match expression {
            Expression::Var(n, id) => {
                let Some(v) = self.lookup(env, id) else {
                    return self.error(&format!("couldn't find variable {} in environment", id));
                };
                Ok(v)
            }
            Expression::App(n, exps) => self.evaluate_application(binding_name, exps, env),
            Expression::Literal(n, lit) => self.evaluate_literal(lit),
            Expression::Lambda(n, clauses) => {
                let mut free = HashSet::new();
                free_vars(expression, &mut free)?;
                todo!()
            }
            Expression::Cond(_, cond) => {
                todo!()
            }
            Expression::Let(_, bind) => {
                todo!()
            }
            Expression::Where(n, exp, bindings) => {
                env.push();
                let prev_cc = self.closures_to_check.clone();
                self.closures_to_check = HashMap::new();
                for b in bindings.iter() {
                    self.evaluate_binding(b, env, true)?;
                    if let Some(hs) = self.closures_to_check.get(&b.name) {
                        for c in hs.clone() {
                            if let Some(Value::Closure(closure)) = self.lookup(env, &c) {
                                let mut cl = closure.borrow_mut();
                                self.replace_undefined(&mut cl.env, env);
                            }
                        }
                    }
                }
                let val = self.evaluate_expression(binding_name, exp, env)?;
                self.closures_to_check = prev_cc;
                env.pop();
                Ok(val)
            }
        }
    }

    pub(super) fn evaluate_literal(&self, literal: &Literal) -> Result<Value> {
        info!("evaluating literal {}", literal);
        match literal {
            Literal::Nil => Ok(Value::Nil),
            Literal::Bool(b) => Ok(Value::Bool(*b)),
            Literal::Integer(i) => Ok(Value::Integer(*i)),
            Literal::Char(c) => Ok(Value::Char(*c)),
            Literal::Real(r) => Ok(Value::Real(*r)),
            Literal::Bytearray(ba) => Ok(Value::new_bytes(ba.clone())),
            Literal::Symbol(sym) => Ok(Value::Symbol(self.get_symbol(sym))),
            Literal::String(s) => self.make_string(s),
        }
    }

    pub(super) fn evaluate_application(
        &mut self,
        binding_name: &str,
        exps: &Vec<Expression>,
        env: &mut Environment,
    ) -> Result<Value> {
        if exps.len() < 2 {
            return self.error("application requires at least two expressions");
        }
        let mut vals = Vec::new();
        for e in exps {
            vals.push(self.evaluate_expression(binding_name, e, env)?);
        }
        self.apply_values(binding_name, &vals)
    }

    pub(super) fn apply_values(&mut self, binding_name: &str, vals: &[Value]) -> Result<Value> {
        info!("Applying {} to {} arguments", &vals[0], &vals[1..].len());
        match &vals[0] {
            Value::Closure(closure_rc) => {
                let mut env: Environment;
                let (mut args, code, arity) = {
                    let mut args = Vec::new();
                    let mut closure = closure_rc.borrow_mut();

                    args.append(&mut closure.args);
                    args.append(&mut vals[1..].to_vec());

                    if args.len() < closure.arity.min_arity() as usize {
                        return Ok(Value::Closure(new_ref(closure.with_args(&args))));
                    }
                    env = closure.env.clone();
                    (args, closure.code.clone(), closure.arity.clone())
                };

                let mut remaining = match arity {
                    Arity::Fixed(n) => args.split_off(n as usize),
                    Arity::VarArg(_, _) => Vec::new(),
                };

                let mut ret = match code.borrow().deref() {
                    Code::Nib(lam) => {
                        let mut v: Value = Value::Nil;
                        todo!()
                    }
                    Code::ExternSimple(ext) => ext(&args)?,
                    Code::ExternMut(ext) => ext(self, &args)?,
                    Code::Extern(ext) => ext(self, &args)?,
                    Code::Foreign(signature, code) => {
                        let mut cargs = Vec::new();
                        for (a, t) in args.iter().zip(&signature.arg_types) {
                            cargs.push(self.get_arg(a, t)?);
                        }
                        let ret = match signature.ret_type {
                            CType::Void => {
                                unsafe { signature.cif.call::<c_void>(*code, &cargs) };
                                Value::Nil
                            }
                            CType::Float32 => {
                                let n = unsafe { signature.cif.call::<f32>(*code, &cargs) };
                                Value::from(n)
                            }
                            CType::Float64 => {
                                let n = unsafe { signature.cif.call::<f64>(*code, &cargs) };
                                Value::from(n)
                            }
                            CType::Pointer => {
                                let n = unsafe { signature.cif.call::<*mut c_void>(*code, &cargs) };
                                Value::from(n)
                            }
                            _ => {
                                let n = unsafe { signature.cif.call::<u64>(*code, &cargs) };
                                Value::from(n)
                            }
                        };
                        ret
                    }
                };

                if !remaining.is_empty() {
                    remaining.insert(0, ret);
                    ret = self.apply_values(binding_name, &remaining)?;
                }
                Ok(ret)
            }
            _ => self.error(&format!("Not a callable type in application {}", vals[0])),
        }
    }

    pub(super) fn evaluate_lambda(
        &mut self,
        binding_name: &str,
        lam: &Lambda,
        free: &HashSet<String>,
        env: &mut Environment,
    ) -> Result<Value> {
        info!("Evaluating lambda");
        let mut lexical_env = Environment::new();
        lexical_env.push();
        for v in free.iter() {
            let val = self.lookup(env, v).unwrap_or(Value::Undefined);
            if val == Value::Undefined {
                let c = self.closures_to_check.entry(v.to_owned()).or_default();
                c.insert(binding_name.to_owned());
            }
            lexical_env.add(v, &val);
        }
        let arity = lam.arity.clone();
        Ok(Value::Closure(new_ref(Closure {
            code: new_ref(Code::Nib(Box::new(lam.clone()))),
            type_table: None,
            env: lexical_env,
            args: Vec::new(),
            arity,
        })))
    }

    pub(super) fn lookup(&self, env: &Environment, id: &str) -> Option<Value> {
        env.get(id).or_else(|| self.get_global(id))
    }

    pub(super) fn lookup_name(&self, env: &Environment, id: &Name) -> Option<Value> {
        if let Name::Plain(s) = id {
            return self.lookup(env, s);
        }
        self.get_name(id)
    }

    pub fn replace_undefined(&mut self, env: &mut Environment, new_env: &Environment) {
        let udef: Vec<_> = {
            env.envs
                .iter()
                .flat_map(|hm| {
                    hm.iter()
                        .filter(|&(k, v)| v == &Value::Undefined)
                        .map(|(k, v)| k.to_owned())
                })
                .collect()
        };
        for k in udef {
            if let Some(v) = self.lookup(new_env, &k) {
                env.add(&k, &v);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub envs: Vec<HashMap<Symbol, Value>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment { envs: Vec::new() }
    }

    pub fn push(&mut self) {
        self.envs.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.envs.pop();
    }

    pub fn push_env(&mut self, env: HashMap<Symbol, Value>) {
        self.envs.push(env);
    }

    pub fn pop_env(&mut self) -> Option<HashMap<Symbol, Value>> {
        self.envs.pop()
    }

    pub fn get(&self, id: &str) -> Option<Value> {
        for e in self.envs.iter().rev() {
            let v = e.get(&Intern::from_ref(id));
            if v.is_some() {
                return v.cloned();
            }
        }
        None
    }

    pub fn add(&mut self, id: &str, value: &Value) {
        let e = if self.envs.is_empty() {
            self.push();
            &mut self.envs[0]
        } else {
            self.envs.last_mut().unwrap()
        };
        e.insert(Intern::from_ref(id), value.clone());
    }
}
