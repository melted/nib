use std::collections::HashMap;

use crate::runtime::{Runtime, Value};


pub enum Primitive {
    NoArg(Box<dyn FnMut() -> Value>),
    OneArg(Box<dyn FnMut(&Value) -> Value>),
    TwoArg(Box<dyn FnMut(&Value, &Value) -> Value>),
    ThreeArg(Box<dyn FnMut(&Value, &Value, &Value) -> Value>),
}

impl Runtime {
    pub fn register_primitives(&mut self) {

    }
}


