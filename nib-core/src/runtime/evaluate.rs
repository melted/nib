use core::panic;
use std::{
    cmp::max,
    collections::{HashMap, HashSet},
};

use log::info;

use crate::{
    ast::Literal,
    common::{Name, Result},
    core::{Arity, Binder, Binding, Expression, FunClause, Module, Pattern, free_vars},
    runtime::{Bytes, Closure, Runtime, Value, new_ref},
};

impl Runtime {
    pub(super) fn evaluate(&mut self, code: &mut Module, env: &mut Environment) -> Result<()> {
        for b in code.bindings.iter_mut() {
            self.evaluate_binding(b, env, false)?;
            dbg!(&self.closures_to_check);
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
        let val = match expression {
            Expression::Var(n, id) => {
                let Some(v) = self.lookup(env, id) else {
                    return self.error(&format!("couldn't find variable {} in environment", id));
                };
                v
            }
            Expression::App(n, exps) => self.evaluate_application(binding_name, exps, env)?,
            Expression::Literal(n, lit) => self.evaluate_literal(lit)?,
            Expression::Lambda(n, clauses) => {
                let mut free = HashSet::new();
                free_vars(expression, &mut free)?;
                self.evaluate_lambda(binding_name, clauses, &free, env)?
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
                val
            }
        };
        Ok(val)
    }

    pub(super) fn evaluate_literal(&mut self, literal: &Literal) -> Result<Value> {
        info!("evaluating literal {}", literal);
        let val = match literal {
            Literal::Nil => Value::Nil,
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Integer(i) => Value::Integer(*i),
            Literal::Char(c) => Value::Char(*c),
            Literal::Real(r) => Value::Real(*r),
            Literal::Bytearray(ba) => Value::new_bytes(ba.clone()),
            Literal::Symbol(sym) => {
                let s = self.get_or_add_named_symbol(sym);
                Value::Symbol(s)
            }
            Literal::String(s) => {
                let mut b = Bytes::with(s.clone().as_bytes().to_vec());
                b.type_table = Some(self.get_or_create_module_path(&["string".to_owned()])?);
                Value::Bytes(new_ref(b))
            }
        };
        Ok(val)
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
            Value::Primitive(prim, Arity::Fixed(1)) => self.call_primitive1(prim, &vals[1]),
            Value::Primitive(prim, Arity::Fixed(2)) => {
                self.call_primitive2(prim, &vals[1], &vals[2])
            }
            Value::Primitive(prim, Arity::Fixed(3)) => {
                self.call_primitive3(prim, &vals[1], &vals[2], &vals[3])
            }
            Value::Primitive(prim, Arity::VarArg(_)) => {
                self.call_primitive_vararg(prim, &vals[1..])
            }
            Value::Closure(closure_rc) => {
                let mut env: Environment;
                let (mut args, clauses, arity) = {
                    let mut args = Vec::new();
                    let mut closure = closure_rc.borrow_mut();

                    args.append(&mut closure.args);
                    args.append(&mut vals[1..].to_vec());

                    if args.len() < closure.arity.min_arity() {
                        return Ok(Value::Closure(new_ref(closure.with_args(&args))));
                    }
                    env = closure.env.clone();
                    (args, closure.code.clone(), closure.arity.clone())
                };

                let mut remaining = match arity {
                    Arity::Fixed(n) => args.split_off(n),
                    Arity::VarArg(_) => Vec::new(),
                };

                for clause in clauses.borrow().iter() {
                    if let Some(binds) = self.match_patterns(&args, &clause.args, &env)? {
                        env.push_env(binds);
                        if let Some(guard) = &clause.guard {
                            let v = self.evaluate_expression(binding_name, guard, &mut env)?;
                            if v == Value::Bool(false) {
                                env.pop();
                                continue;
                            }
                        }
                        let mut v =
                            self.evaluate_expression(binding_name, &clause.rhs, &mut env)?;
                        if remaining.len() > 0 {
                            remaining.insert(0, v);
                            v = self.apply_values(binding_name, &remaining)?;
                        }
                        env.pop();
                        return Ok(v);
                    }
                }
                self.error(&format!("No matching pattern for closure"))
            }
            _ => {
                panic!("sss");
                self.error(&format!("Not a callable type in application {}", vals[0]))
            },
        }
    }

    pub(super) fn evaluate_lambda(
        &mut self,
        binding_name: &str,
        clauses: &Vec<FunClause>,
        free: &HashSet<String>,
        env: &mut Environment,
    ) -> Result<Value> {
        info!("Evaluating lambda");
        let mut lexical_env = Environment::new();
        lexical_env.push();
        for v in free.iter() {
            let val = self.lookup(env, v).unwrap_or(Value::Undefined);
            if val == Value::Undefined {
                let c = self
                    .closures_to_check
                    .entry(v.to_owned())
                    .or_insert_with(|| HashSet::new());
                c.insert(binding_name.to_owned());
            }
            lexical_env.add(&v, &val);
        }
        let mut arity = get_arity(&clauses[0].args);
        for c in clauses[1..].iter() {
            let next = get_arity(&c.args);
            match (&arity, &next) {
                (Arity::Fixed(n), Arity::Fixed(m)) if n == m => {}
                (Arity::VarArg(n), Arity::VarArg(m)) => {
                    arity = Arity::VarArg(max(*n, *m));
                }
                _ => {
                    return self.error("Function clauses must have same arity");
                }
            }
        }
        Ok(Value::Closure(new_ref(Closure {
            code: new_ref(clauses.clone()),
            type_table: None,
            env: lexical_env,
            args: Vec::new(),
            arity,
        })))
    }

    pub(super) fn match_patterns(
        &mut self,
        args: &[Value],
        patterns: &[Pattern],
        env: &Environment,
    ) -> Result<Option<HashMap<String, Value>>> {
        let mut current_arg: usize = 0;
        let mut out = HashMap::new();
        for (i, p) in patterns.iter().enumerate() {
            if current_arg == args.len() {
                return Ok(None);
            }
            let res = if let Pattern::Ellipsis(_) = p {
                let trailing = patterns.len() - i - 1;
                let ellipsis = Value::new_array(&args[current_arg..args.len() - trailing]);
                let r = self.match_pattern(&ellipsis, p, env)?;
                current_arg = args.len() - trailing;
                r
            } else {
                let r = self.match_pattern(&args[current_arg], p, env)?;
                current_arg = current_arg + 1;
                r
            };
            if let Some(vars) = res {
                vars.into_iter().for_each(|(k, v)| {
                    out.insert(k, v);
                });
            } else {
                return Ok(None);
            }
        }
        Ok(Some(out))
    }

    pub(super) fn match_pattern(
        &mut self,
        arg: &Value,
        pattern: &Pattern,
        env: &Environment,
    ) -> Result<Option<HashMap<String, Value>>> {
        let mut out = HashMap::new();
        let val = match pattern {
            Pattern::Wildcard => Some(out),
            Pattern::Literal(literal) => {
                let v = self.evaluate_literal(literal)?;
                if &v == arg { Some(out) } else { None }
            }
            Pattern::Ellipsis(Some(name)) | Pattern::Bind(name) => {
                out.insert(name.string(), arg.clone());
                Some(out)
            }
            Pattern::Ellipsis(None) => Some(out),
            Pattern::Custom(name, patterns) => {
                let Some(handler) = self.lookup_name(env, name) else {
                    return self.error(&format!("Failed to find custom pattern handler {}", name));
                };
                let fun = match handler {
                    Value::Closure(_) => handler,
                    Value::Table(t) => {
                        let Some(val @ Value::Closure(_)) = self.get_from_table(t, "match") else {
                            return self.error("No match function in table used in custom pattern");
                        };
                        val
                    }
                    _ => {
                        return self.error("Custom pattern handler must be a function or a table with a match function");
                    }
                };
                let call = vec![fun, arg.clone()];
                let res = self.apply_values(&name.string(), &call)?;
                match res {
                    Value::Array(array) => {
                        let vals = &array.borrow().array;
                        self.match_patterns(vals, patterns, env)?
                    }
                    Value::Bool(false) => None,
                    _ => {
                        return self.error(&format!(
                            "Custom pattern handler {} didn't return an array or false",
                            name.string()
                        ));
                    }
                }
            }
            Pattern::Alias(pattern, name) => {
                let res = self.match_pattern(arg, pattern, env)?;
                res.map(|mut vars| {
                    vars.insert(name.string(), arg.clone());
                    vars
                })
            }
        };
        Ok(val)
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
                .map(|hm| {
                    hm.iter()
                        .filter(|&(k, v)| v == &Value::Undefined)
                        .map(|(k, v)| k.to_owned())
                })
                .flatten()
                .collect()
        };
        dbg!(&udef);
        for k in udef {
            if let Some(v) = self.lookup(new_env, &k) {
                env.add(&k, &v);
            }
        }
    }
}

pub(super) fn get_arity(patterns: &[Pattern]) -> Arity {
    let vararg = patterns.iter().any(|p| p.is_ellipsis());
    let len = patterns.len();
    if vararg {
        Arity::VarArg(len - 1)
    } else {
        Arity::Fixed(len)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub envs: Vec<HashMap<String, Value>>,
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

    pub fn push_env(&mut self, env: HashMap<String, Value>) {
        self.envs.push(env);
    }

    pub fn pop_env(&mut self) -> Option<HashMap<String, Value>> {
        self.envs.pop()
    }

    pub fn get(&self, id: &str) -> Option<Value> {
        for e in self.envs.iter().rev() {
            let v = e.get(id);
            if v.is_some() {
                return v.map(|x| x.clone());
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
        e.insert(id.to_owned(), value.clone());
    }
}
