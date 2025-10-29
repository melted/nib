#![allow(unused_variables)]
#![allow(dead_code)]

//! Compile code to bytecode then run it

use std::collections::HashMap;

use crate::common::{Result, Symbol};
use crate::interpreter::bytecode::*;
use crate::interpreter::heap::{Heap, Table, Value};

pub mod bytecode;
pub mod compile;
pub mod heap;
mod tests;

pub struct Runtime {
    heap: Heap,
    global_env: Value,
    pub code : Vec<u8>,
    pub ip : usize,
    pub stack : Vec<Value>,
    pub regs : [Value; 256]
}


const DEFAULT_HEAP_SIZE: usize = 1000000;

impl Runtime {
    pub fn new() -> Self {
        let mut heap = Heap::new(DEFAULT_HEAP_SIZE);
        let global_env = Value::from(Table::make(&mut heap));

        Runtime {
            heap,
            global_env,
            code: Vec::new(),
            ip: 0, 
            stack: Vec::new(), 
            regs: [Value::nil(); 256]
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

    fn run(&mut self) -> Result<()> {
        while self.ip < self.code.len() {
            self.step()?;
        }
        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        let instr = self.code[self.ip];
        match instr {
            INSTR_ADD..=INSTR_MOD => self.op_arithmetic(instr),
            _ => todo!()
        }
    }

    fn op_arithmetic(&mut self, op: u8) -> Result<()> {
        let reg_target = self.code[self.ip+1];
        let reg_left = self.code[self.ip+2];
        let reg_right = self.code[self.ip+3];
        let left = self.regs[reg_left as usize];
        let right = self.regs[reg_right as usize];
        let res = if left.is_immediate_integer() && right.is_immediate_integer() {
            match op {
                INSTR_ADD => Value::integer(left.get_integer() + right.get_integer()),
                INSTR_SUB => Value::integer(left.get_integer() - right.get_integer()),
                INSTR_MUL => Value::integer(left.get_integer() * right.get_integer()),
                INSTR_DIV => Value::integer(left.get_integer() / right.get_integer()),
                INSTR_MOD => Value::integer(left.get_integer() % right.get_integer()),
                _ => unreachable!()
            }

        } else if left.is_float() || left.is_immediate_integer() {
            let lf = get_float(left);
            let rf = get_float(right);
            let r = match op {
                INSTR_ADD => lf + rf,
                INSTR_SUB => lf - rf,
                INSTR_MUL => lf * rf,
                INSTR_DIV => lf / rf,
                _ => unreachable!()
            };
            Value::alloc_float(&mut self.heap, r)
        } else {
            todo!()
        };
        self.ip += 4;
        self.regs[reg_target as usize] = res;
        Ok(())
    }

}

fn get_float(val: Value) -> f64 {
    if val.is_immediate_integer() {
        val.get_integer() as f64
    } else {
        val.get_float()
    }
}