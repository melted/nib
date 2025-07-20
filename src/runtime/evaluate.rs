use std::{collections::HashMap, rc::Rc};

use crate::{core::Module, runtime::{Runtime, Value}};

impl Runtime {
    pub(super) fn evaluate(&mut self, code : Module) {
        todo!()
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
