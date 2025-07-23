use std::{collections::{HashMap, HashSet}, rc::Rc};

use log::info;

use crate::{ast::Literal, common::{Error, Name, Result}, core::{free_vars, Binder, Binding, Expression, FunClause, Module}, runtime::{new_ref, prims::Arity, Bytes, Closure, Runtime, Value}};

impl Runtime {
    pub(super) fn evaluate(&mut self, code : &mut Module, env:&mut Environment) -> Result<()> {
        for b in code.bindings.iter_mut() {
            self.evaluate_binding(b, env, false)?;
        }
        Ok(())
    }

    pub(super) fn evaluate_binding(&mut self, binding: &Binding, env:&mut Environment, local:bool) -> Result<()> {
        info!("Evaluating binding {}", binding);
        let val = self.evaluate_expression(&binding.body, env)?;
        match &binding.binder {

            Binder::Public(name) if local => {
                match name {
                    Name::Plain(s) => env.add(s, &val),
                    Name::Qualified(_,_) => return self.error(&format!("Qualified name {} in where clause", name))
                };
            },
            Binder::Public(name) => {
                self.add_name(name, &val)?;
            },
            Binder::Local(name) => {
                env.add(name, &val);
            },
            Binder::Unbound => {}
        }
        Ok(())
    }

    pub(super) fn evaluate_expression(&mut self, expression: &Expression, env:&mut Environment) -> Result<Value> {
        info!("Evaluating expression {}", expression);
        let val = match expression {
            Expression::Var(n, id) => {
                // TODO: fix declaration order
                let Some(v) = self.lookup(env, id) else {
                    return self.error(&format!("couldn't find variable {} in environment", id));
                };
                v
            },
            Expression::App(n, exps) => self.evaluate_application(exps, env)?,
            Expression::Literal(n, lit) => self.evaluate_literal(lit)?,
            Expression::Lambda(n, clauses) => {
                let mut free = HashSet::new();
                free_vars(expression, &mut free);
                self.evaluate_lambda(clauses, &free, env)?
            },
            Expression::Where(n, exp, bindings) => {
                env.push();
                for b in bindings.iter() {
                    self.evaluate_binding(b, env, true)?;
                }
                let val = self.evaluate_expression(exp, env)?;
                env.pop();
                val
            }
        };
        Ok(val)
    }

    pub(super) fn evaluate_literal(&mut self, literal : &Literal) -> Result<Value> {
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
            },
            Literal::String(s) => {
                let mut b = Bytes::with(s.clone().as_bytes().to_vec());
                b.type_table = Some(self.get_or_create_module_path(&["string".to_owned()])?);
                Value::Bytes(new_ref(b))
            }
        };
        Ok(val)
    }

    pub(super) fn evaluate_application(&mut self, exps : &Vec<Expression>, env:&mut Environment) -> Result<Value> {
        if exps.len() < 2 {
            return self.error("application requires at least two expressions");
        }
        let mut vals = Vec::new();
        for e in exps {
            vals.push(self.evaluate_expression(e, env)?);
        }
        match &vals[0] {
            Value::Primitive(prim, Arity::OneArg) => self.call_primitive1(prim, &vals[1]),
            Value::Primitive(prim, Arity::TwoArg) => self.call_primitive2(prim, &vals[1], &vals[2]),
            Value::Primitive(prim, Arity::ThreeArg) => self.call_primitive3(prim, &vals[1], &vals[2], &vals[3]),
            Value::Primitive(prim, Arity::VarArg) => self.call_primitive_vararg(prim, &vals[1..]),
            Value::Closure(closure) => {
                let c = closure.borrow_mut();
                todo!()
            },
            _ => {
                self.error(&format!("Not a callable type in application {}", vals[0]))
            }
        }
    }

    pub(super) fn evaluate_lambda(&mut self, clauses : &Vec<FunClause>, free:&HashSet<String>, env:&mut Environment) -> Result<Value> {
        let mut lexical_env = Environment::new();
        lexical_env.push();

        for v in free.iter() {
            let Some(val) = self.lookup(env, v) else {
                return self.error(&format!("{} not in scope", v));
            };
            lexical_env.add(&v, &val);
        }
        Ok(Value::Closure(new_ref(Closure{ code: new_ref(clauses.clone()), type_table: None, env: lexical_env })))
    }


    pub(super) fn lookup(&mut self, env: &Environment, id: &str) -> Option<Value> {
        env.get(id).or_else(|| self.get_global(id))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub envs: Vec<HashMap<String, Value>>
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

    pub fn push_env(&mut self, env : HashMap<String, Value>) {
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
