use crate::common::Symbol;
use crate::{
    ast::Literal,
    common::{Name, Result},
    core::{Arity, Binder, Binding, Expression, Lambda, Module, free_vars},
    treewalker::{CType, Closure, Code, Runtime, Value, new_ref},
};
use log::{info, log};
use std::hash::Hash;
use std::{
    collections::{HashMap, HashSet},
    mem,
    ops::Deref,
    os::raw::c_void,
};
use symbol_table::GlobalSymbol;

type ClosureRefs = HashMap<Symbol, HashSet<Symbol>>;

impl Runtime {
    pub(super) fn evaluate(&mut self, code: &mut Module, env: &mut Environment) -> Result<()> {
        let mut eval_status = EvalStatus::new();
        for b in code.bindings.iter().rev() {
            self.add_binding(&mut eval_status, b);
        }
        self.eval(&mut eval_status, env)?;
        Ok(())
    }

    pub(super) fn evaluate_expression(&mut self, expression: Expression) -> Result<Value> {
        let mut eval_status = EvalStatus::new();
        let mut env = Environment::new();
        eval_status
            .work_stack
            .push(EvalStep::Expression(None, expression));
        self.eval(&mut eval_status, &mut env)?;
        Ok(eval_status.value_stack.pop().unwrap_or(Value::Nil))
    }

    pub(super) fn evaluate_apply(&mut self, vals: &[Value]) -> Result<Value> {
        let size = vals.len();
        let mut eval_status = EvalStatus::new();
        let mut env = Environment::new();
        for v in vals {
            eval_status.value_stack.push(v.clone());
        }
        eval_status.work_stack.push(EvalStep::Apply(None, size));
        self.eval(&mut eval_status, &mut env)?;
        Ok(eval_status.value_stack.pop().unwrap_or(Value::Nil))
    }

    fn update_closures(&mut self, env: &mut Environment, name: &str) {
        if let Some(hs) = self.closures_to_check.get(&Symbol::from(name)) {
            for c in hs.clone() {
                if let Some(Value::Closure(closure)) = self.lookup(env, &c) {
                    let mut cl = closure.borrow_mut();
                    self.replace_undefined(&c, &mut cl.env, env);
                }
            }
        }
    }

    fn evaluate_literal(&self, literal: &Literal) -> Result<Value> {
        info!("evaluating literal {}", literal);
        match literal {
            Literal::Nil => Ok(Value::Nil),
            Literal::Bool(b) => Ok(Value::Bool(*b)),
            Literal::Integer(i) => Ok(Value::Integer(*i)),
            Literal::Char(c) => Ok(Value::Char(*c)),
            Literal::Real(r) => Ok(Value::Real(*r)),
            Literal::Bytearray(ba) => Ok(Value::new_bytes(ba.clone())),
            Literal::Symbol(sym) => Ok(Value::Symbol(*sym)),
            Literal::String(s) => self.make_string(s),
        }
    }

    fn evaluate_lambda(
        &mut self,
        binding_name: &Option<Name>,
        lam: &Lambda,
        free: &HashSet<Symbol>,
        env: &mut Environment,
    ) -> Result<Value> {
        info!("Evaluating lambda");
        let mut lexical_env = Environment::new();
        lexical_env.push();
        for v in free.iter() {
            let val = self.lookup(env, &v).unwrap_or(Value::Undefined);
            if let Some(name) = binding_name {
                if val == Value::Undefined {
                    let c = self.closures_to_check.entry(*v).or_default();
                    c.insert(name.top());
                }
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

    pub(super) fn lookup(&self, env: &Environment, id: &Symbol) -> Option<Value> {
        env.get(id).or_else(|| self.get_global(id))
    }

    fn replace_undefined(&mut self, place: &Symbol, env: &mut Environment, new_env: &Environment) {
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
                if let Some(hs) = self.closures_to_check.get_mut(place) {
                    hs.remove(&k);
                }
            }
        }
    }

    fn add_binding(&mut self, eval_status: &mut EvalStatus, binding: &Binding) {
        eval_status
            .work_stack
            .push(EvalStep::Bind(binding.name.clone(), binding.binder.clone()));
        eval_status.work_stack.push(EvalStep::Expression(
            binding.name.clone(),
            binding.body.clone(),
        ));
    }

    fn eval(&mut self, eval_status: &mut EvalStatus, env: &mut Environment) -> Result<()> {
        while let Some(step) = eval_status.work_stack.pop() {
            self.eval_step(eval_status, step, env)?;
        }
        Ok(())
    }

    fn eval_step(
        &mut self,
        eval_status: &mut EvalStatus,
        step: EvalStep,
        env: &mut Environment,
    ) -> Result<()> {
        info!("taking eval step {:?}", &step);
        match step {
            EvalStep::Expression(binding_name, exp) => {
                self.eval_expression(eval_status, exp, env, &binding_name)?;
            }
            EvalStep::ReplaceClosureRefs(prev_cc) => {
                self.closures_to_check = prev_cc;
            }
            EvalStep::Select(if_true, if_false) => {
                let Some(val) = eval_status.value_stack.pop() else {
                    return self.error("Expected Value in select eval");
                };
                let next = if val == Value::Bool(false) {
                    if_false
                } else {
                    if_true
                };
                eval_status.work_stack.push(*next);
            }
            EvalStep::Apply(name, size) => {
                let from = eval_status.value_stack.len() - size;
                let args = eval_status.value_stack.drain(from..).collect::<Vec<_>>();
                self.eval_apply(eval_status, &args, env, &name)?;
            }
            EvalStep::Bind(name, binder) => {
                let Some(val) = eval_status.value_stack.pop() else {
                    return self.error("no value to bind to");
                };
                match binder {
                    Binder::Public(name) => {
                        self.add_name(&name, &val)?;
                    }
                    Binder::Local(name) => {
                        match name {
                            Name::Plain(s) => {
                                env.add(&s, &val);
                            }
                            Name::Qualified(path, id) => {
                                let start = &path[0];
                                let rest = &path[1..];
                                let first = if let Some(v) = env.get(&start) {
                                    v.get_table()?
                                } else {
                                    let nt = Value::new_table();
                                    env.add(start, &nt);
                                    nt.get_table()?
                                };
                                let tab = self.get_or_create_module_path(rest, first)?;
                                self.add_to_table(tab, &id, &val);
                            }
                        };
                    }
                    Binder::Unbound => {}
                }
                if let Some(n) = &name {
                    self.update_closures(env, n.top().as_str());
                }
            }
            EvalStep::ReplaceEnv(mut new_env) => {
                mem::swap(env, &mut new_env);
            }
            EvalStep::Value(val) => {
                eval_status.value_stack.push(val);
            }
        }
        Ok(())
    }

    fn eval_expression(
        &mut self,
        eval_status: &mut EvalStatus,
        expr: Expression,
        env: &mut Environment,
        binding_name: &Option<Name>,
    ) -> Result<()> {
        match expr {
            Expression::Literal(_, lit) => {
                let val = self.evaluate_literal(&lit)?;
                eval_status.value_stack.push(val);
            }
            Expression::Var(_, var) => {
                let Some(v) = self.lookup(env, &var.name()) else {
                    return self.error(&format!(
                        "couldn't find variable {} in environment",
                        &var.name()
                    ));
                };
                eval_status.value_stack.push(v);
            }
            Expression::Lambda(_, lam) => {
                let mut free = HashSet::new();
                let mut locals = HashMap::new();
                free_vars(&Expression::Lambda(0, lam.clone()), &mut free, &mut locals);
                let closure = self.evaluate_lambda(&binding_name, &lam, &free, env)?;
                eval_status.value_stack.push(closure);
            }
            Expression::Cond(_, cond) => {
                eval_status.work_stack.push(EvalStep::Select(
                    Box::new(EvalStep::Expression(binding_name.clone(), cond.if_true)),
                    Box::new(EvalStep::Expression(binding_name.clone(), cond.if_false)),
                ));
                eval_status
                    .work_stack
                    .push(EvalStep::Expression(binding_name.clone(), cond.pred));
            }
            Expression::App(_, app) => {
                let size = app.len();
                eval_status
                    .work_stack
                    .push(EvalStep::Apply(binding_name.clone(), size));
                for exp in app.into_iter().rev() {
                    eval_status
                        .work_stack
                        .push(EvalStep::Expression(binding_name.clone(), exp));
                }
            }
            Expression::Where(_, exp, binds) => {
                let mut prev_cc = HashMap::new();
                mem::swap(&mut prev_cc, &mut self.closures_to_check);
                eval_status
                    .work_stack
                    .push(EvalStep::ReplaceClosureRefs(prev_cc));
                eval_status
                    .work_stack
                    .push(EvalStep::Expression(binding_name.clone(), *exp));
                for b in binds.into_iter().rev() {
                    self.add_binding(eval_status, &b);
                }
            }
        }
        Ok(())
    }

    pub(super) fn eval_apply(
        &mut self,
        eval_status: &mut EvalStatus,
        vals: &[Value],
        current_env: &mut Environment,
        binding_name: &Option<Name>,
    ) -> Result<()> {
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
                        eval_status
                            .value_stack
                            .push(Value::Closure(new_ref(closure.with_args(&args))));
                        return Ok(());
                    }
                    env = closure.env.clone();
                    (args, closure.code.clone(), closure.arity.clone())
                };

                let mut remaining = match arity {
                    Arity::Fixed(n) => args.split_off(n as usize),
                    Arity::VarArg(_, _) => Vec::new(),
                };
                if !remaining.is_empty() {
                    let size = remaining.len();
                    eval_status
                        .work_stack
                        .push(EvalStep::Apply(binding_name.clone(), size + 1));
                    for r in remaining.into_iter().rev() {
                        eval_status.work_stack.push(EvalStep::Value(r));
                    }
                }
                let ret = match code.borrow().deref() {
                    Code::Nib(lam) => {
                        if let Arity::VarArg(i, n) = lam.arity {
                            let num = args.len() - i as usize;
                            let array = {
                                let vars = args.drain((i as usize)..(i as usize + num));
                                Value::new_array(vars.as_slice())
                            };
                            args.insert(i as usize, array);
                        }
                        for (v, i) in args.iter().zip(lam.args.iter()) {
                            env.add(&i.name(), v);
                        }
                        mem::swap(&mut env, current_env);
                        eval_status.work_stack.push(EvalStep::ReplaceEnv(env));
                        eval_status
                            .work_stack
                            .push(EvalStep::Expression(binding_name.clone(), lam.body.clone()));
                        None
                    }
                    Code::ExternSimple(ext) => Some(ext(&args)?),
                    Code::ExternMut(ext) => Some(ext(self, &args)?),
                    Code::Extern(ext) => Some(ext(self, &args)?),
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
                        Some(ret)
                    }
                };
                if let Some(val) = ret {
                    eval_status.value_stack.push(val);
                }
                Ok(())
            }
            _ => self.error(&format!(
                "Not a callable type in application {} {:?}",
                vals[0], vals
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct EvalStatus {
    work_stack: Vec<EvalStep>,
    value_stack: Vec<Value>,
}

impl EvalStatus {
    fn new() -> Self {
        EvalStatus {
            work_stack: Vec::new(),
            value_stack: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum EvalStep {
    Expression(Option<Name>, Expression),
    ReplaceClosureRefs(ClosureRefs),
    ReplaceEnv(Environment),
    Select(Box<EvalStep>, Box<EvalStep>),
    Value(Value),
    Apply(Option<Name>, usize),
    Bind(Option<Name>, Binder),
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

    pub fn get(&self, id: &Symbol) -> Option<Value> {
        for e in self.envs.iter().rev() {
            let v = e.get(id);
            if v.is_some() {
                return v.cloned();
            }
        }
        None
    }

    pub fn add(&mut self, id: &Symbol, value: &Value) {
        let e = if self.envs.is_empty() {
            self.push();
            &mut self.envs[0]
        } else {
            self.envs.last_mut().unwrap()
        };
        e.insert(*id, value.clone());
    }

    pub fn remove(&mut self, id: &Symbol) {
        for e in self.envs.iter_mut().rev() {
            let v = e.get(id);
            if v.is_some() {
                e.remove(id);
            }
        }
    }
}
