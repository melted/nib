use std::{collections::HashMap, rc::Rc};

use crate::{common::{Name, Result}, core::{Binder, Binding, Expression, Module}, runtime::{Runtime, Value}};

impl Runtime {
    pub(super) fn evaluate(&mut self, code : &mut Module) -> Result<()> {
        for b in code.bindings.iter_mut() {
            self.evaluate_binding(b)?;
        }
        Ok(())
    }

    pub(super) fn evaluate_binding(&mut self, binding: &mut Binding) -> Result<()> {
        let val = self.evaluate_expression(&mut binding.body)?;
        match binding.binder {
            Binder::Public(name) => {
                match name {
                    Name::Plain(name) => {

                    },
                    Name::Qualified(path, name) => {

                    }
                 }
            },
            Binder::Local(name) => {
                self.local_environment.add(&name, &val);
            },
            Binder::Unbound => {}
        }
        Ok(())
    }

    pub(super) fn evaluate_expression(&mut self, expression: &mut Expression) -> Result<Value> {

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

    pub fn push_env(&mut self) {
        self.envs.push(HashMap::new());
    }

    pub fn pop_env(&mut self) {
        self.envs.pop();
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
            self.push_env();
            &mut self.envs[0]
        } else {
            self.envs.last_mut().unwrap()
        };
        e.insert(id.to_owned(), value.clone());
    }
}
