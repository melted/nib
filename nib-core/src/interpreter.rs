#![allow(unused_variables)]
#![allow(dead_code)]

//! Compile code to bytecode then run it

use std::collections::HashMap;

use crate::common::Result;
use crate::interpreter::heap::{Heap, Table, Value};

pub mod bytecode;
pub mod compile;
pub mod heap;
mod tests;

pub struct Runtime {
    heap: Heap,
    global_env: Value,
    symbol_table: HashMap<String, Value>,
}

const DEFAULT_HEAP_SIZE: usize = 1000000;

impl Runtime {
    pub fn new() -> Self {
        let mut heap = Heap::new(DEFAULT_HEAP_SIZE);
        let global_env = Value::from(Table::make(&mut heap));
        let symbol_table = HashMap::new();
        Runtime {
            heap,
            global_env,
            symbol_table,
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
