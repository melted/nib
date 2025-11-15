#![allow(unused_variables)]
#![allow(dead_code)]

//! Compile code to bytecode then run it

use std::cmp::Ordering;
use std::collections::HashMap;
use std::ffi::c_void;
use std::ops::BitXor;

use region::page::size;
use crate::capi;
use crate::common::{Error, Name, Result, Symbol};
use crate::interpreter::bytecode::*;
use crate::interpreter::heap::{
    Array, Bytes, Closure, Code, Heap, ObjectHeader, TYPE_BYTECODE, TYPE_CORE, TYPE_EXTERN, Table,
    Value, ValueRepr, set_value,
};
use crate::interpreter::prims::PrimFn;

pub mod bytecode;
pub mod compile;
pub mod heap;
pub mod prims;
mod tests;

pub struct Runtime {
    heap: Heap,
    global_env: Value,
    pub stack: Stack,
    pub base: usize,
    code: Value,
    ip: usize,
    pub regs: [Value; 256],
}

const DEFAULT_HEAP_SIZE: usize = 1000000;
const DEFAULT_STACK_SIZE:usize = 1000;

impl Runtime {
    pub fn new() -> Self {
        let mut heap = Heap::new(DEFAULT_HEAP_SIZE);
        let mut runtime = Runtime {
            heap,
            global_env: Value::nil(),
            stack: Stack::new(Value::nil()), // Dummy stack
            base: 0,
            code: Value::nil(),
            ip: 0,
            regs: [Value::nil(); 256],
        };
        let global_env = Value::from(Table::make(&mut runtime));
        let stack = Value::from(Array::make(&mut runtime, DEFAULT_STACK_SIZE));
        runtime.global_env = global_env;
        runtime.stack = Stack::new(stack);
        runtime
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

    pub fn set_global(&mut self, sym: &Symbol, value: &Value) {
        let mut env = self.global_env.get_table();
        env.insert(self, Value::symbol(sym), value.clone());
    }

    pub fn get_global(&self, sym: &Symbol) -> Value {
        let env = self.global_env.get_table();
        env.get(Value::symbol(sym))
    }

    pub fn add_name(&mut self, name: &Name, val: &Value) -> Result<()> {
        match name {
            Name::Qualified(path, leaf) => {
                let s = Value::from(leaf.clone());
                let t = self.get_or_create_module_path(path, self.global_env)?;
                t.get_table().insert(self, s, *val);
            }
            Name::Plain(n) => {
                self.set_global(n, &val);
            }
        }
        Ok(())
    }

    pub fn get_name(&self, name: &Name) -> Option<Value> {
        let val = match name {
            Name::Qualified(path, leaf) => {
                if let Some(t) = self.get_module_path(path, self.global_env) {
                    let key = Value::from(*leaf);
                    t.get_table().get(key)
                } else {
                    Value::nil()
                }
            }
            Name::Plain(name) => self.get_global(&name),
        };
        if val.is_nil() {
            None
        } else {
            Some(val) 
        }
    }

    pub fn get_module_path(&self, path: &[Symbol], start: Value) -> Option<Value> {
        let mut rest = path;
        let mut table = start;
        while !rest.is_empty() {
            let sym = Value::from(rest[0]);
            table = {
                let t = table.get_table();
                let v = t.get(sym);
                match v.get_repr() {
                    ValueRepr::Table => v,
                    _ => {
                        return None;
                    }
                }
            };
            rest = &rest[1..];
        }
        Some(table)
    }

    pub fn get_or_create_module_path(&mut self, path: &[Symbol], start: Value) -> Result<Value> {
        let mut rest = path;
        let mut table = start;
        ensure_type(&table, ValueRepr::Table)?;
        while !rest.is_empty() {
            let sym = &rest[0];
            table = {
                let mut t = table.get_table();
                let key = Value::from(sym.clone());
                let v = t.get(key);
                match v.get_repr() {
                    ValueRepr::Table => v,
                    ValueRepr::Nil => {
                        let nt = Value::from(Table::make(self));
                        t.insert(self, key, nt);
                        nt
                    }
                    _ => {
                        return self.error(&format!("Illegal module path {:?}", path));
                    }
                }
            };
            rest = &rest[1..];
        }
        Ok(table)
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
            INSTR_NOP => {
                self.ip += 1;
                Ok(false)
            }
            INSTR_ADD..=INSTR_MOD => self.op_arithmetic(),
            INSTR_NEG => self.op_negate(),
            INSTR_CMP..=INSTR_NEQ => self.op_compare(),
            INSTR_CALL..=INSTR_CALL_TAIL => self.op_call(),
            INSTR_RETURN => self.op_return(),
            INSTR_JUMP..=INSTR_JUMP_IMM8 => self.op_jump(),
            INSTR_JZ..=INSTR_JNFALSE_IMM8 => self.op_conditional_jump(),
            INSTR_MOVE => self.op_move(),
            INSTR_LOAD_IMM8..=INSTR_LOAD_IMM64 => self.op_load_imm(),
            INSTR_LOAD_BYTES_IMM => self.op_load_bytes(),
            INSTR_PUSH => self.op_push(),
            INSTR_POP => self.op_pop(),
            INSTR_PUSH_RANGE => self.op_push_range(),
            INSTR_POP_RANGE => self.op_pop_range(),
            INSTR_ALLOC_FLOAT..=INSTR_ALLOC_CLOSURE => self.op_alloc(),
            INSTR_ARRAY_REF => self.op_array_get(),
            INSTR_ARRAY_SET => self.op_array_set(),
            INSTR_ARRAY_SIZE => self.op_array_size(),
            INSTR_BYTES_REF => self.op_bytes_get(),
            INSTR_BYTES_SET => self.op_bytes_set(),
            INSTR_BYTES_SIZE => self.op_bytes_size(),
            INSTR_TABLE_GET => self.op_table_get(),
            INSTR_TABLE_SET => self.op_table_set(),
            INSTR_TABLE_SIZE => self.op_table_size(),
            _ => self.error(&format!("unimplemented opcode: {}", instr)),
        }
    }

    fn op_arithmetic(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let op = code[self.ip];
        let reg_target = code[self.ip + 1];
        let reg_left = code[self.ip + 2];
        let reg_right = code[self.ip + 3];
        let left = self.regs[reg_left as usize];
        let right = self.regs[reg_right as usize];
        let res = if left.is_immediate_integer() && right.is_immediate_integer() {
            match op {
                INSTR_ADD => Value::integer(left.get_integer() + right.get_integer()),
                INSTR_SUB => Value::integer(left.get_integer() - right.get_integer()),
                INSTR_MUL => Value::integer(left.get_integer() * right.get_integer()),
                INSTR_DIV => Value::integer(left.get_integer() / right.get_integer()),
                INSTR_MOD => Value::integer(left.get_integer() % right.get_integer()),
                _ => unreachable!(),
            }
        } else if left.is_float() || left.is_immediate_integer() {
            let lf = get_float(left);
            let rf = get_float(right);
            let r = match op {
                INSTR_ADD => lf + rf,
                INSTR_SUB => lf - rf,
                INSTR_MUL => lf * rf,
                INSTR_DIV => lf / rf,
                _ => unreachable!(),
            };
            Value::alloc_float(self, r)
        } else if left.is_pointer()
            && right.is_immediate_integer()
            && (op == INSTR_ADD || op == INSTR_SUB)
        {
            let p = left.get_pointer::<*mut c_void>();
            let sign = if op == INSTR_ADD { 1 } else { -1 };
            unsafe { Value::pointer(p.byte_offset(sign * right.get_integer() as isize)) }
        } else {
            todo!()
        };
        self.ip += 4;
        self.regs[reg_target as usize] = res;
        Ok(false)
    }

    fn op_negate(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target = code[self.ip + 1] as usize;
        let src = code[self.ip + 2] as usize;
        let val = self.regs[src];
        let res = if val.is_immediate_integer() {
            const MSB: u64 = 1 << 63;
            Value {
                val: val.val.bitxor(MSB),
            }
        } else if val.is_float() {
            let f = -val.get_float();
            Value::alloc_float(self, f)
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
        let reg_target = code[self.ip + 1] as usize;
        let reg_left = code[self.ip + 2];
        let reg_right = code[self.ip + 3];
        let left = self.regs[reg_left as usize];
        let right = self.regs[reg_right as usize];
        let res = match (left.get_immediate_repr(), right.get_immediate_repr()) {
            (ValueRepr::Integer, ValueRepr::Integer) => {
                let order = left.get_integer().cmp(&right.get_integer());
                ordering_to_int(order)
            }
            (ValueRepr::Pointer, ValueRepr::Pointer) => {
                let order = left
                    .get_cpointer::<*mut c_void>()
                    .addr()
                    .cmp(&right.get_cpointer::<*mut c_void>().addr());
                ordering_to_int(order)
            }
            (ValueRepr::Float | ValueRepr::Integer, ValueRepr::Float | ValueRepr::Integer) => {
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
            (ValueRepr::Nil, ValueRepr::Nil) => 0,
            (ValueRepr::Symbol, ValueRepr::Symbol) if op != INSTR_CMP => {
                if left.get_symbol() == right.get_symbol() {
                    0
                } else {
                    1
                }
            }
            (ValueRepr::Object | ValueRepr::Array, ValueRepr::Object | ValueRepr::Array)
                if op != INSTR_CMP =>
            {
                let order = left.val.cmp(&right.val);
                ordering_to_int(order)
            }
            (_, _) if op != INSTR_CMP => {
                // TODO: look at type table
                -1
            }
            (x, y) => {
                // TODO: look at type table
                return self.error(&format!("Can't compare types {:?} and {:?}", x, y));
            }
        };
        let val = match op {
            INSTR_CMP => Value::integer(res),
            INSTR_EQ => Value::bool(res == 0),
            INSTR_NEQ => Value::bool(res != 0),
            _ => unreachable!(),
        };
        self.ip += 4;
        self.regs[reg_target] = val;
        Ok(false)
    }

    fn op_call(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let op = code[self.ip];
        let reg = code[self.ip + 1] as usize;
        let args = code[self.ip + 2] as usize;
        let val = self.regs[reg];
        ensure_type(&val, ValueRepr::Closure)?;
        let closure = val.get_closure();
        self.make_call(op, args, closure)?;
        Ok(false)
    }

    fn make_call(&mut self, op: u8, args: usize, closure: Closure) -> Result<()> {
        let fun_code = closure.fun();
        if args < closure.num_args() {
            // underapplication, create new closure
    
        }
        let remaining = if closure.is_vararg() {
            0
        } else {
            args - closure.num_args()
        };
        self.ip += 2;
        match closure.get_tag() {
            TYPE_BYTECODE => {
                if op == INSTR_CALL || remaining > 0 {
                    self.base = self.stack.len();
                    self.stack_push(self.code.clone());
                    self.stack_push(Value::integer(self.ip as i64));
                    if remaining > 0 {
                        for r in (args+1)..(args+1+remaining) {
                            let over_arg = self.regs[r];
                            self.stack_push(over_arg);
                        }
                    }
                    self.stack_push(Value::integer(remaining as i64));
                }
                self.code = fun_code;
                self.ip = 0;
            }
            TYPE_EXTERN => {
                let fun_ptr = fun_code.get_pointer() as *mut PrimFn;
                unsafe {
                    let fun = *fun_ptr;
                    fun(self)?;
                }
            }
            _ => {
                return self.error("Core code not supported in bytecode interpreter");
            }
        };
        Ok(())
    }
    
    fn op_return(&mut self) -> Result<bool> {
        // TODO: if stack is empty return true
        let remaining = self.stack.pop();
        let mut args = vec![];
        for r in 2..(remaining.get_integer()+2) {
            let val = self.stack.pop();
            args.push(val);
        } 
        let ip = self.stack.pop();
        let code = self.stack.pop();
        self.code = code;
        self.ip = ip.get_integer() as usize;
        Ok(false)
    }

    fn op_jump(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let op = code[self.ip];
        let arg = code[self.ip + 1];
        let dist = if op == INSTR_JUMP {
            let val = self.regs[arg as usize];
            ensure_type(&val, ValueRepr::Integer)?;
            val.get_integer() as i64
        } else {
            (arg as i64 - 128)
        };
        let target = (self.ip as i64 + dist) as usize;
        if target < code.len() {
            self.ip = target;
            Ok(false)
        } else {
            self.error("Jump outside of code")
        }
    }

    fn op_conditional_jump(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let op = code[self.ip];
        let check = code[self.ip + 1];
        let val = self.regs[check as usize];
        let do_jump = match op {
            INSTR_JZ | INSTR_JZ_IMM8 => val.get_integer() == 0,
            INSTR_JPOS | INSTR_JPOS_IMM8 => val.get_integer() > 0,
            INSTR_JNEG | INSTR_JNEG_IMM8 => val.get_integer() < 0,
            INSTR_JNPOS | INSTR_JNPOS_IMM8 => !(val.get_integer() > 0),
            INSTR_JNNEG | INSTR_JNNEG_IMM8 => !(val.get_integer() < 0),
            INSTR_JFALSE | INSTR_JFALSE_IMM8 => val == Value::bool(false),
            INSTR_JNFALSE | INSTR_JNFALSE_IMM8 => val != Value::bool(false),
            _ => unreachable!(),
        };
        if do_jump {
            let arg = code[self.ip + 1];
            let dist = match op {
                INSTR_JPOS | INSTR_JNEG | INSTR_JNPOS | INSTR_JNNEG | INSTR_JFALSE
                | INSTR_JNFALSE => {
                    let val = self.regs[arg as usize];
                    ensure_type(&val, ValueRepr::Integer)?;
                    val.get_integer() as i64
                }
                _ => arg as i64 - 128,
            };
            let target = (self.ip as i64 + dist) as usize;
            if target < code.len() {
                self.ip = target;
                Ok(false)
            } else {
                self.error("Jump outside of code")
            }
        } else {
            self.ip += 3;
            Ok(false)
        }
    }

    fn op_load_imm(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let op = code[self.ip];
        let target_reg = code[self.ip + 1];
        let rest = &code[self.ip + 2..];
        let (val, skip) = match op {
            INSTR_LOAD_IMM8 => (code[self.ip + 2] as u64, 3),
            INSTR_LOAD_IMM16 => {
                let v = rest.first_chunk::<2>().unwrap();
                (u16::from_ne_bytes(*v) as u64, 4)
            }
            INSTR_LOAD_IMM32 => {
                let v = rest.first_chunk::<4>().unwrap();
                (u32::from_ne_bytes(*v) as u64, 6)
            }
            INSTR_LOAD_IMM64 => {
                let v = rest.first_chunk::<8>().unwrap();
                (u64::from_ne_bytes(*v), 10)
            }
            _ => unreachable!(),
        };
        self.regs[target_reg as usize] = Value { val };
        self.ip += skip;
        Ok(false)
    }

    fn op_load_bytes(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let op = code[self.ip];
        let target_reg = code[self.ip + 1];
        let rest = &code[self.ip + 2..];
        let v = rest.first_chunk::<4>().unwrap();
        let size = u32::from_ne_bytes(*v) as usize;
        let bytes = &rest[4..4 + size];
        let res = Value::from(Bytes::with(self, bytes));
        self.regs[target_reg as usize] = res;
        self.ip += 5 + size;
        Ok(false)
    }

    fn op_move(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target_reg = code[self.ip + 1] as usize;
        let source_reg = code[self.ip + 2] as usize;
        self.regs[target_reg] = self.regs[source_reg];
        self.ip += 3;
        Ok(false)
    }

    fn op_push(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let source_reg = code[self.ip + 1];
        self.stack_push(self.regs[source_reg as usize]);
        self.ip += 2;
        Ok(false)
    }

    fn op_pop(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target_reg = code[self.ip + 1];
        let val = self.stack.pop();
        self.regs[target_reg as usize] = val;
        Ok(false)
    }

    fn op_push_range(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let source_reg_from = code[self.ip + 1] as usize;
        let source_reg_to = code[self.ip + 2] as usize;
        for r in source_reg_from..=source_reg_to {
            self.stack_push(self.regs[r]);
        }
        self.ip += 3;
        Ok(false)
    }

    fn op_pop_range(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target_reg_from = code[self.ip + 1] as usize;
        let target_reg_to = code[self.ip + 2] as usize;
        for r in (target_reg_from..=target_reg_to).rev() {
            let val = self.stack.pop();
            self.regs[r] = val;
        }
        self.ip += 3;
        Ok(false)
    }

    fn op_type(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target = code[self.ip + 1] as usize;
        let reg = code[self.ip + 2] as usize;
        let val = self.regs[reg];
        let typ = match val.get_repr() {
            ValueRepr::Nil => self.get_global(&Symbol::from("nil_type")),
            ValueRepr::Undefined => Value::nil(),
            ValueRepr::Bool => self.get_global(&Symbol::from("bool")),
            ValueRepr::Integer => self.get_global(&Symbol::from("int")),
            ValueRepr::Pointer => self.get_global(&Symbol::from("pointer")),
            ValueRepr::Char => self.get_global(&Symbol::from("char")),
            ValueRepr::Float => self.get_global(&Symbol::from("float")),
            ValueRepr::BoxedInteger => todo!(),
            ValueRepr::Symbol => self.get_global(&Symbol::from("symbol")),
            ValueRepr::Array => {
                let arr = val.get_array();
                let mut type_table = arr.type_table();
                if type_table == Value::nil() {
                    type_table = self.get_global(&Symbol::from("array"));
                }
                type_table
            }
            ValueRepr::Bytes => {
                let bytes = val.get_bytes();
                let mut type_table = bytes.type_table();
                if type_table == Value::nil() {
                    type_table = self.get_global(&Symbol::from("bytes"));
                }
                type_table
            }
            ValueRepr::Table => {
                let table = val.get_table();
                let mut type_table = table.type_table();
                if type_table == Value::nil() {
                    type_table = self.get_global(&Symbol::from("table"));
                }
                type_table
            }
            ValueRepr::Closure => {
                let closure = val.get_closure();
                let mut type_table = closure.type_table();
                if type_table == Value::nil() {
                    type_table = self.get_global(&Symbol::from("closure"));
                }
                type_table
            }
            ValueRepr::Object => todo!(),
        };
        self.regs[target] = typ;
        self.ip += 3;
        Ok(false)
    }

    fn op_set_type(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let obj_reg = code[self.ip + 1] as usize;
        let typ_reg = code[self.ip + 2] as usize;
        let val = self.regs[obj_reg];
        let typ = self.regs[typ_reg];
        ensure_type(&typ, ValueRepr::Table)?;
        match val.get_repr() {
            ValueRepr::Array | ValueRepr::Bytes | ValueRepr::Table | ValueRepr::Closure => {
                let obj = val.get_object();
                set_value(obj, 0, typ);
            }
            _ => {
                return self.error("settype op on illegal value");
            }
        }
        self.ip += 3;
        Ok(false)
    }

    fn op_alloc(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let op = code[self.ip];
        let target_reg = code[self.ip + 1] as usize;
        let (val, dist) = match op {
            INSTR_ALLOC_ARRAY => {
                let size_reg = code[self.ip + 2] as usize;
                let size = self.regs[size_reg];
                ensure_type(&size, ValueRepr::Integer)?;
                let arr = Array::make(self, size.get_integer() as usize);
                (Value::from(arr), 3)
            }
            INSTR_ALLOC_BYTES => {
                let size_reg = code[self.ip + 2] as usize;
                let fill = code[self.ip + 3];
                let size = self.regs[size_reg];
                ensure_type(&size, ValueRepr::Integer)?;
                let bytes = Bytes::make(self, size.get_integer() as usize, fill);
                (Value::from(bytes), 4)
            }
            INSTR_ALLOC_FLOAT => {
                let bytes_reg = code[self.ip + 2] as usize;
                let val = self.regs[bytes_reg];
                ensure_type(&val, ValueRepr::Bytes)?;
                let b = val.get_bytes();
                let x = b.get_slice().first_chunk::<8>().unwrap();
                let f = f64::from_ne_bytes(*x);
                let v = Value::alloc_float(self, f);
                (v, 3)
            }
            INSTR_ALLOC_TABLE => (Value::from(Table::make(self)), 2),
            INSTR_ALLOC_CLOSURE => {
                let code_reg = code[self.ip + 2] as usize;
                let capture_reg = code[self.ip + 3] as usize;
                let arity_reg = code[self.ip + 4] as usize;
                let vararg_reg = code[self.ip + 5] as usize;
                let code = self.regs[code_reg];
                ensure_type(&code, ValueRepr::Bytes)?;
                let captures = self.regs[capture_reg];
                let arity = self.regs[arity_reg];
                ensure_type(&arity, ValueRepr::Integer)?;
                let vararg = self.regs[vararg_reg];
                let code_bytes = code.get_bytes();

                let closure =
                    Closure::make_low(self, &code_bytes, captures, arity, vararg);
                (Value::from(closure), 6)
            }
            _ => unreachable!(),
        };
        self.regs[target_reg] = val;
        self.ip += dist;
        Ok(false)
    }

    fn op_array_get(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target_reg = code[self.ip + 1] as usize;
        let obj_reg = code[self.ip + 2] as usize;
        let pos_reg = code[self.ip + 3] as usize;
        let obj = self.regs[obj_reg];
        let pos = self.regs[pos_reg];
        let val = obj.get_array().at(pos.get_integer() as usize);
        self.regs[target_reg] = val;
        Ok(false)
    }

    fn op_array_set(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let obj_reg = code[self.ip + 1] as usize;
        let pos_reg = code[self.ip + 2] as usize;
        let val_reg = code[self.ip + 3] as usize;
        let obj = self.regs[obj_reg];
        let pos = self.regs[pos_reg];
        let val = self.regs[val_reg];
        obj.get_array().set(pos.get_integer() as usize, val);
        Ok(false)
    }

    fn op_array_size(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target_reg = code[self.ip + 1] as usize;
        let obj_reg = code[self.ip + 2] as usize;
        let obj = self.regs[obj_reg];
        let val = Value::integer(obj.get_array().size() as i64);
        self.regs[target_reg] = val;
        Ok(false)
    }

    fn op_bytes_get(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target_reg = code[self.ip + 1] as usize;
        let obj_reg = code[self.ip + 2] as usize;
        let pos_reg = code[self.ip + 3] as usize;
        let obj = self.regs[obj_reg];
        let pos = self.regs[pos_reg];
        let val = obj.get_bytes().at(pos.get_integer() as usize);
        self.regs[target_reg] = Value::integer(val as i64);
        Ok(false)
    }

    fn op_bytes_set(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let obj_reg = code[self.ip + 1] as usize;
        let pos_reg = code[self.ip + 2] as usize;
        let val_reg = code[self.ip + 3] as usize;
        let obj = self.regs[obj_reg];
        let pos = self.regs[pos_reg];
        let val = self.regs[val_reg].get_integer() as u8;
        obj.get_bytes().set(pos.get_integer() as usize, val);
        Ok(false)
    }

    fn op_bytes_size(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target_reg = code[self.ip + 1] as usize;
        let obj_reg = code[self.ip + 2] as usize;
        let obj = self.regs[obj_reg];
        let val = Value::integer(obj.get_bytes().size() as i64);
        self.regs[target_reg] = val;
        Ok(false)
    }

    fn op_table_get(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target_reg = code[self.ip + 1] as usize;
        let obj_reg = code[self.ip + 2] as usize;
        let sym_reg = code[self.ip + 3] as usize;
        let obj = self.regs[obj_reg];
        let sym = self.regs[sym_reg];
        let val = obj.get_table().get(sym);
        self.regs[target_reg] = val;
        Ok(false)
    }

    fn op_table_set(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let obj_reg = code[self.ip + 1] as usize;
        let sym_reg = code[self.ip + 2] as usize;
        let val_reg = code[self.ip + 3] as usize;
        let obj = self.regs[obj_reg];
        let sym = self.regs[sym_reg];
        let val = self.regs[val_reg];
        obj.get_table().insert(self, sym, val);
        Ok(false)
    }

    fn op_table_size(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let target_reg = code[self.ip + 1] as usize;
        let obj_reg = code[self.ip + 2] as usize;
        let obj = self.regs[obj_reg];
        let val = Value::integer(obj.get_table().size() as i64);
        self.regs[target_reg] = val;
        Ok(false)
    }

    fn op_halt(&mut self) -> Result<bool> {
        Ok(true)
    }

    pub fn error<T>(&self, msg: &str) -> Result<T> {
        Err(Error::Runtime {
            msg: msg.to_owned(),
            loc: None,
        })
    }

    // Because we want to allocate a new stack instead of overflowing,
    // wrap stack pushes.
    pub(super) fn stack_push(&mut self, val:Value) {
        if self.stack.top == self.stack.stack.get_array().size() {
            self.stack_expand();
        }
        self.stack.push(val);
    }

    fn stack_expand(&mut self) {
        let old_array = self.stack.stack.get_array();
        let size = old_array.size();
        let mut new_array = Array::make(self, size*2);
        let values = old_array.values();
        new_array.fill(values, 0, values.len());
        self.stack.stack = Value::from(new_array);
    }
}

pub struct Stack {
    pub stack: Value,
    top: usize,
    base: usize
}

impl Stack {
    pub(super) fn new(stack: Value) -> Self {
        Self { stack, top: 0, base: 0 }
    }

    pub(super) fn push(&mut self, val : Value) {
        if self.top == self.stack.get_array().size() {
            panic!("stack overflow");
        }
        self.stack.get_array().set(self.top, val);
        self.top += 1;
    }

    pub(super) fn pop(&mut self) -> Value {
        if self.top == 0 {
            panic!("stack underflow");
        }
        self.top -= 1;
        let elem = self.stack.get_array().at(self.top);
        self.stack.get_array().set(self.top, Value::nil());
        elem
    }

    pub(super) fn len(&self) -> usize {
        self.top
    }

    pub(super) fn set_base(&mut self) {
        self.base = self.top;
    }

    pub(super) fn current_frame(&self) -> usize {
        self.top - self.base
    }
}

fn get_float(val: Value) -> f64 {
    if val.is_immediate_integer() {
        val.get_integer() as f64
    } else {
        val.get_float()
    }
}

fn ordering_to_int(order: Ordering) -> i64 {
    match order {
        Ordering::Equal => 0,
        Ordering::Greater => 1,
        Ordering::Less => -1,
    }
}

pub(super) fn ensure_type(val: &Value, repr: ValueRepr) -> Result<()> {
    if val.get_repr() != repr {
        Err(Error::runtime_error(&format!(
            "Expected {:?}, has {:?}",
            repr,
            val.get_repr()
        )))
    } else {
        Ok(())
    }
}
