#![allow(unused_variables)]
#![allow(dead_code)]

//! Compile code to bytecode then run it

use std::collections::HashMap;

use crate::common::{Result, Symbol};
use crate::interpreter::heap::{Heap, Table, Value};

pub mod bytecode;
pub mod compile;
pub mod heap;
mod tests;

pub struct Runtime {
    heap: Heap,
    global_env: Value,
    stack: Vec<Value>,
}

const DEFAULT_HEAP_SIZE: usize = 1000000;

impl Runtime {
    pub fn new() -> Self {
        let mut heap = Heap::new(DEFAULT_HEAP_SIZE);
        let global_env = Value::from(Table::make(&mut heap));
        let stack = Vec::new();
        Runtime {
            heap,
            global_env,
            stack,
        }
    }

    pub fn load(&mut self, reload: bool) -> Result<()> {
        todo!()
    }

    pub fn add_code(&mut self, name: &str, code: &str) -> Result<()> {
        todo!()
    }

    pub fn run_expression(&mut self, code: &str) -> Result<Value> {
        todo!()
    }
}
