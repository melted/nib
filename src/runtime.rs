use std::collections::HashMap;

use crate::{common::Metadata, core, runtime::prims::Primitive};

pub mod heap;
pub mod table;
mod prims;

pub struct Runtime {
    metadata: HashMap<String, Metadata>,
    globals: Environment,
    primitives: HashMap<String, Primitive>
}

impl Runtime {
    pub fn new() -> Self {
        Runtime { metadata: HashMap::new(), globals: Environment::new(), primitives: HashMap::new() }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub vars : HashMap<String, Value>
}

impl Environment {
    fn new() -> Environment {
        Environment { vars: HashMap::new() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Integer(i64),
    Real(f64),
    Char(char),
    Symbol(String),
    Bytearray(Vec<u8>),
    Array(Vec<Value>),
    Table(HashMap<String, Value>),
    Closure(Closure)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub code : core::Expression,
    pub vars : Environment
}