//! Compile code to bytecode then run it

use std::collections::HashMap;

use crate::interpreter::heap::{Heap, Table, Value};

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
}
