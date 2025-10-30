#![allow(unused_variables)]
#![allow(dead_code)]

//! Compile code to bytecode then run it

use std::cmp::Ordering;
use std::collections::HashMap;
use std::ffi::c_void;
use std::ops::BitXor;

use crate::common::{Error, Result, Symbol};
use crate::interpreter::bytecode::*;
use crate::interpreter::heap::{Heap, Table, Value, ValueRepr, TYPE_BYTECODE, TYPE_CORE, TYPE_EXTERN};
use crate::interpreter::prims::PrimFn;

pub mod bytecode;
pub mod compile;
pub mod heap;
mod prims;
mod tests;

pub struct Runtime {
    heap: Heap,
    global_env: Value,
    pub stack : Vec<Value>,
    pub base : usize,
    code : Value,
    ip : usize,
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
            stack: Vec::new(),
            base: 0,
            code: Value::nil(),
            ip: 0,
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

    pub fn add_global(&mut self, sym: &Symbol, value:&Value) -> Result<()> {
        let mut env = self.global_env.get_table();
        env.insert(&mut self.heap, Value::symbol(sym), value.clone());
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        let code_size = self.code.get_bytes().size();
        while self.ip < code_size {
            if self.step()? {
                break;
            }
        }
        Ok(())
    } 

    fn step(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let instr = code[self.ip];
        match instr {
            INSTR_ADD..=INSTR_MOD => self.op_arithmetic(),
            INSTR_NEG => self.op_negate(),
            INSTR_CMP..=INSTR_NEQ => self.op_compare(),
            INSTR_CALL => self.op_call(),
            INSTR_RETURN => self.op_return(),
            INSTR_JUMP..=INSTR_JNFALSE_IMM32 => self.op_jump(),
            _ => {
                self.error(&format!("unimplemented opcode: {}", instr))
            }
        }
    }

    fn op_arithmetic(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let op = code[self.ip];
        let reg_target = code[self.ip+1];
        let reg_left = code[self.ip+2];
        let reg_right = code[self.ip+3];
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
        } else if left.is_pointer() && right.is_immediate_integer() && (op == INSTR_ADD || op == INSTR_SUB) {
            let p = left.get_pointer::<*mut c_void>();
            let sign = if op == INSTR_ADD { 1 } else { -1 };
            unsafe { Value::pointer(p.byte_offset(sign*right.get_integer() as isize)) }
        } else  {
            todo!()
        };
        self.ip += 4;
        self.regs[reg_target as usize] = res;
        Ok(false)
    }

    fn op_negate(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target = code[self.ip+1] as usize;
        let src = code[self.ip+2] as usize;
        let val = self.regs[src];
        let res = if val.is_immediate_integer() {
            const MSB:u64 = 1 << 63;
            Value { val: val.val.bitxor(MSB) }
        } else if val.is_float() {
            let f = -val.get_float();
            Value::alloc_float(&mut self.heap, f)
        } else {
            // crash and burn
            todo!()
        };
        self.regs[target] = res;
        self.ip += 3;
        Ok(false)
    }

    fn op_compare(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let op = code[self.ip];
        let reg_target = code[self.ip+1] as usize;
        let reg_left = code[self.ip+2];
        let reg_right = code[self.ip+3];
        let left = self.regs[reg_left as usize];
        let right = self.regs[reg_right as usize];
        let res = match (left.get_immediate_repr(), right.get_immediate_repr()) {
            (ValueRepr::Integer, ValueRepr::Integer) => {
                let order = left.get_integer().cmp(&right.get_integer());
                ordering_to_int(order)
            },
            (ValueRepr::Pointer, ValueRepr::Pointer) => {
                let order = left.get_cpointer::<*mut c_void>().addr().cmp(&right.get_cpointer::<*mut c_void>().addr());
                ordering_to_int(order)
            },
            (ValueRepr::Float|ValueRepr::Integer, ValueRepr::Float|ValueRepr::Integer) => {
                let lf = get_float(left);
                let rf = get_float(right);
                let order = lf.total_cmp(&rf);
                ordering_to_int(order)
            }
            (ValueRepr::Char, ValueRepr::Char) => {
                let order = left.get_char().cmp(&right.get_char());
                ordering_to_int(order)
            }
            (ValueRepr::Bool, ValueRepr::Bool) => {
                let order = left.get_bool().cmp(&right.get_bool());
                ordering_to_int(order)
            }
            (ValueRepr::Nil, ValueRepr::Nil) => {
                0
            }
            (ValueRepr::Symbol, ValueRepr::Symbol) if op != INSTR_CMP => {
                if left.get_symbol() == right.get_symbol() { 0 } else { 1 }
            },
            (ValueRepr::Object|ValueRepr::Array, ValueRepr::Object|ValueRepr::Array) if op != INSTR_CMP => {
                let order = left.val.cmp(&right.val);
                ordering_to_int(order)
            }
            (_, _) if op != INSTR_CMP => {
                // TODO: look at type table
                -1
            },
            (x, y) => {
                // TODO: look at type table
                return self.error(&format!("Can't compare types {:?} and {:?}", x, y));
            }
        };
        let val = match op {
            INSTR_CMP => Value::integer(res),
            INSTR_EQ => Value::bool(res == 0),
            INSTR_NEQ => Value::bool(res != 0),
            _ => unreachable!()
        };
        self.ip += 4;
        self.regs[reg_target] = val;
        Ok(false)
    }

    fn op_call(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let reg = code[self.ip+1] as usize;
        let val = self.regs[reg];
        ensure_type(&val, ValueRepr::Closure)?;
        let closure = val.get_closure();
        let fun_code = closure.fun();
        self.ip += 2;
        match closure.get_tag() {
            TYPE_BYTECODE => {
                self.base = self.stack.len();
                self.stack.push(self.code.clone());
                self.stack.push(Value::integer(self.ip as i64));
                self.code = fun_code;
                self.ip = 0;
            }
            TYPE_EXTERN => {
                let fun_ptr = fun_code.get_pointer() as *mut PrimFn;
                unsafe {
                    let fun = *fun_ptr;
                    fun(self)?;    
                }
            },
            _ => {
                return self.error("Core code not supported in bytecode interpreter");
            },
        }
        Ok(false)
    }

    fn op_return(&mut self) -> Result<bool> {
        let Some(ip) = self.stack.pop() else {
            return Ok(true);
        };
        let Some(code) = self.stack.pop() else {
            return Ok(true);
        };
        self.code = code;
        self.ip = ip.get_integer() as usize;
        Ok(false)
    }

    fn op_jump(&mut self) -> Result<bool> {
        Ok(false)
    }

    pub fn error<T>(&self, msg: &str) -> Result<T> {
        Err(Error::Runtime {
            msg: msg.to_owned(),
            loc: None,
        })
    }

}

fn get_float(val: Value) -> f64 {
    if val.is_immediate_integer() {
        val.get_integer() as f64
    } else {
        val.get_float()
    }
}

fn ordering_to_int(order:Ordering) -> i64 {
    match order {
        Ordering::Equal => 0,
        Ordering::Greater => 1,
        Ordering::Less => -1
    }
}


pub(super) fn ensure_type(val:&Value, repr:ValueRepr) -> Result<()> {
    if val.get_repr() != repr {
        Err(Error::runtime_error(&format!("Expected {:?}, has {:?}", repr, val.get_repr())))
    } else {
        Ok(())
    }
}