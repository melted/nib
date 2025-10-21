#![allow(unused_variables)]
#![allow(dead_code)]

//! Compile code to bytecode then run it

use std::collections::HashMap;

use crate::common::{Result, Symbol};
use crate::interpreter::bytecode::Instruction;
use crate::interpreter::heap::{Heap, Table, Value};

pub mod bytecode;
pub mod compile;
pub mod heap;
mod tests;

pub struct Runtime {
    heap: Heap,
    global_env: Value,
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


pub struct VMState {
    pub code : Vec<u8>,
    pub ip : usize,
    pub stack : Vec<Value>,
    pub regs : [Value; 256]
}

impl VMState {
    fn new() -> Self {
        VMState { code: Vec::new(), ip: 0, stack: Vec::new(), regs: [Value::nil(); 256] }
    }

    fn step(&mut self) {
        match self.code[ip] {
             => self.execute_op(self.code[ip]),

        }
    }

    fn execute_op(&mut self, op: u8) {
        
    }
}