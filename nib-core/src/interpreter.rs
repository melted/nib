#![allow(unused_variables)]
#![allow(dead_code)]

//! Compile code to bytecode then run it

use std::cmp::Ordering;
use std::ffi::c_void;
use std::ops::BitXor;

use crate::common::{Error, Name, Result, Symbol};
use crate::interpreter::bytecode::*;
use crate::interpreter::heap::{
    Array, Bytes, Closure, Heap, TYPE_BYTECODE, TYPE_EXTERN, Table,
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
    local_env: Value,
    stack: Stack,
    call_stack: Stack,
    closure: Value,
    code: Value,
    ip: usize
}

const DEFAULT_HEAP_SIZE: usize = 1000000;
const DEFAULT_STACK_SIZE:usize = 10000;
const DEFAULT_CALL_STACK_SIZE:usize = 10000;

impl Runtime {
    pub fn new() -> Self {
        let heap = Heap::new(DEFAULT_HEAP_SIZE);
        let mut runtime = Runtime {
            heap,
            global_env: Value::nil(),
            local_env:Value::nil(),
            stack: Stack::new(Value::nil()), // Dummy stack
            call_stack: Stack::new(Value::nil()), // Dummy stack
            code: Value::nil(),
            closure: Value::nil(),
            ip: 0
        };
        let global_env = Value::from(Table::make(&mut runtime));
        let stack = Value::from(Array::make(&mut runtime, DEFAULT_STACK_SIZE));
        let call_stack = Value::from(Array::make(&mut runtime, DEFAULT_CALL_STACK_SIZE));
        runtime.global_env = global_env;
        runtime.stack = Stack::new(stack);
        runtime.call_stack = Stack::new(call_stack);
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
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        while self.ip < code_size {
            if self.step(code)? {
                break;
            }
        }
        Ok(())
    }

    fn step(&mut self, code:&[u8]) -> Result<bool> {
        let instr = code[self.ip];
        self.ip += 1;
        match instr {
            INSTR_NOP => {
                Ok(false)
            }
            INSTR_ADD..=INSTR_MOD => self.op_arithmetic(instr),
            INSTR_NEG => self.op_negate(),
            INSTR_CMP..=INSTR_NEQ => self.op_compare(instr),
            INSTR_CALL..=INSTR_CALL_TAIL => self.op_call(instr),
            INSTR_RETURN => self.op_return(),
            INSTR_JUMP..=INSTR_JUMP_IMM8 => self.op_jump(instr),
            INSTR_JZ..=INSTR_JNFALSE_IMM8 => self.op_conditional_jump(instr),
            INSTR_STACK_LOAD => self.op_pick(),
            INSTR_STACK_STORE => self.op_put(),
            INSTR_LOAD_IMM8..=INSTR_LOAD_IMM64 => self.op_load_imm(instr),
            INSTR_LOAD_BYTES_IMM => self.op_load_bytes(),
            INSTR_DUP => self.op_dup(),
            INSTR_SWAP => self.op_swap(),
            INSTR_DROP => self.op_drop(),
            INSTR_DROP_FRAME => self.op_drop_frame(),
            INSTR_STACK_LIFT => self.op_stack_lift(),
            INSTR_ALLOC_FLOAT..=INSTR_ALLOC_CLOSURE => self.op_alloc(instr),
            INSTR_ARRAY_REF => self.op_array_get(),
            INSTR_ARRAY_SET => self.op_array_set(),
            INSTR_ARRAY_SIZE => self.op_array_size(),
            INSTR_BYTES_REF => self.op_bytes_get(),
            INSTR_BYTES_SET => self.op_bytes_set(),
            INSTR_BYTES_SIZE => self.op_bytes_size(),
            INSTR_TABLE_GET => self.op_table_get(),
            INSTR_TABLE_SET => self.op_table_set(),
            INSTR_TABLE_SIZE => self.op_table_size(),
            INSTR_GET_LOCAL => self.op_get_local(),
            INSTR_SET_LOCAL => self.op_set_local(),
            INSTR_GLOBAL_ENV => self.op_global_env(),
            _ => self.error(&format!("unimplemented opcode: {}", instr)),
        }
    }

    fn op_arithmetic(&mut self, op:u8) -> Result<bool> {
        let left = self.stack.pop();
        let right = self.stack.pop();
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
        self.stack_push(res);
        Ok(false)
    }

    fn op_negate(&mut self) -> Result<bool> {
        let val = self.stack.pop();
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
        self.stack_push(res);
        Ok(false)
    }

    fn op_compare(&mut self, op:u8) -> Result<bool> {
        let left = self.stack.pop();
        let right = self.stack.pop();
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
        self.stack_push(val);
        Ok(false)
    }

    fn op_call(&mut self, op:u8) -> Result<bool> {
        let count = self.stack.pop();
        let args = match count.get_immediate_repr() {
            ValueRepr::Integer => count.get_integer() as usize,
            ValueRepr::CallContinuation => count.get_cc_args(),
            _ => return self.error("call arg size must be integer or call continuation")
        };
        let mut argv = self.stack.take(args);
        match argv[0].get_repr() {
            ValueRepr::Closure => {
            }
            ValueRepr::PartialApplication => {
                let pap = argv[0].get_array();
                let closure = pap.at(0).get_closure();
                argv.extend_from_slice(&pap.values()[1..]);
                argv[0] = Value::from(closure)
            }
            _ => {
                // Other callables, implement later
                todo!()
            }
        };
        self.make_call(op, &argv)?;
        Ok(false)
    }

    fn make_call(&mut self, op: u8, argv: &[Value]) -> Result<()> {
        let closure = argv[0].get_closure();
        let fun_code = closure.code_value();
        let args = argv.len() - 1;
        let env = closure.env().get_array();
        if args < closure.num_args() {
            // Underapplication, create a partial application
            let pap = Array::with(self, argv);
            self.stack.push(Value::partial_application(pap));
            return Ok(())
        }
        let mut extra_args = args - closure.num_args();
        let mut new_args = Vec::new();
        if let Some(i) = closure.vararg() {
            let pos = i - 1; 
            let var_arg = Array::with(self, &argv[pos..pos+extra_args]);
            new_args.extend_from_slice(&argv[1..pos]);
            new_args.push(Value::from(var_arg));
            new_args.extend_from_slice(&argv[pos+extra_args..args]);
            extra_args = 0;
        } else {
            new_args.extend_from_slice(&argv[1..closure.num_args()+1]);
        };
        match closure.get_tag() {
            TYPE_BYTECODE => {
                if op == INSTR_CALL || extra_args > 0 {
                    // Not a tail call, set up a new frame
                    self.ensure_call_stack(3);
                    let frame = vec![self.closure.clone(), Value::integer(self.ip as i64), Value::integer(self.stack.base as i64)];
                    self.call_stack.pushv(&frame);
                    self.stack.base = self.stack.top();
                }
                if extra_args > 0 {
                    for i in argv[closure.num_args()+1..argv.len()].iter().rev() {
                        self.stack_push(*i);
                    }
                    self.stack_push(Value::call_continuation(extra_args));
                }
                for i in new_args.iter().rev() {
                    self.stack_push(*i);
                }
                self.local_env = self.closure.get_closure().env();
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
        let cc = self.stack.peek(1);
        if cc.is_call_continuation() {
            self.op_call(INSTR_CALL_TAIL)
        } else {
            let old_base = self.call_stack.pop().get_integer() as usize;
            let ip = self.call_stack.pop().get_integer() as usize;
            let closure = self.call_stack.pop();
            self.closure = closure;
            self.code = closure.get_closure().code_value();
            self.local_env = closure.get_closure().env();
            self.ip = ip;
            Ok(false)
        }
    }

    fn op_jump(&mut self, op:u8) -> Result<bool> {
        let dist = self.stack.pop().get_integer();
        let target = (self.ip as i64 + dist) as usize;
        if target < self.code.get_bytes().size() {
            self.ip = target;
            Ok(false)
        } else {
            self.error("Jump outside of code")
        }
    }

    fn op_conditional_jump(&mut self, op:u8) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let dist = if is_immediate_jump(op) {
            let next = code[self.ip] as i64;
            self.ip += 1;
            next
        } else {
            self.stack.pop().get_integer()
        };
        let val = self.stack.pop();
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
            let target = (self.ip as i64 + dist) as usize;
            if target < code.len() {
                self.ip = target;
            } else {
                return self.error("Jump outside of code");
            }
        }
        Ok(false)
    }

    fn op_load_imm(&mut self, op:u8) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let rest = &code[self.ip..];
        let (val, skip) = match op {
            INSTR_LOAD_IMM8 => (code[self.ip] as u64, 1),
            INSTR_LOAD_IMM16 => {
                let v = rest.first_chunk::<2>().unwrap();
                (u16::from_ne_bytes(*v) as u64, 2)
            }
            INSTR_LOAD_IMM32 => {
                let v = rest.first_chunk::<4>().unwrap();
                (u32::from_ne_bytes(*v) as u64, 4)
            }
            INSTR_LOAD_IMM64 => {
                let v = rest.first_chunk::<8>().unwrap();
                (u64::from_ne_bytes(*v), 8)
            }
            _ => unreachable!(),
        };
        self.stack_push(Value { val });
        self.ip += skip;
        Ok(false)
    }

    fn op_load_bytes(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let rest = &code[self.ip..];
        let v = rest.first_chunk::<4>().unwrap();
        let size = u32::from_ne_bytes(*v) as usize;
        let bytes = &rest[4..4 + size];
        let res = Value::from(Bytes::with(self, bytes));
        self.stack_push(res);
        self.ip += 4 + size;
        Ok(false)
    }

    fn op_pick(&mut self) -> Result<bool> {
        let depth = self.stack.pop().get_integer() as usize;
        self.stack.pick(depth);
        Ok(false)
    }

    fn op_put(&mut self) -> Result<bool> {
        let val = self.stack.pop();
        let depth = self.stack.pop().get_integer() as usize;
        self.stack.put(depth, val);
        Ok(false)
    }

    fn op_dup(&mut self) -> Result<bool> {
        self.stack.pick(0);
        Ok(false)
    }

    fn op_swap(&mut self) -> Result<bool> {
        let top = self.stack.array.at(self.stack.top());
        let next = self.stack.array.at(self.stack.top()-1);
        self.stack.array.set(self.stack.top(), next);
        self.stack.array.set(self.stack.top()-1, top);
        Ok(false)
    }

    fn op_drop(&mut self) -> Result<bool> {
        let _ = self.stack.pop();
        Ok(false)
    }

    fn op_drop_frame(&mut self) -> Result<bool> {
        self.stack.set_top(self.stack.base);
        Ok(false)
    }

    fn op_stack_lift(&mut self) -> Result<bool> {
        let n = self.stack.pop().get_integer() as usize;
        let d = self.stack.pop().get_integer() as usize;
        self.stack.lift(n, d);
        Ok(false)
    }

    fn op_type(&mut self) -> Result<bool> {
        let val = self.stack.pop();
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
            ValueRepr::CallContinuation => self.get_global(&Symbol::from("call_continuation")),
            ValueRepr::PartialApplication => self.get_global(&Symbol::from("partial_application")),
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
                    type_table = self.get_global(&Symbol::from("function"));
                }
                type_table
            }
            ValueRepr::Object => todo!(),
        };
        self.stack_push(typ);
        Ok(false)
    }

    fn op_set_type(&mut self) -> Result<bool> {
        let val = self.stack.pop();
        let typ = self.stack.pop();
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
        Ok(false)
    }

    fn op_alloc(&mut self, op:u8) -> Result<bool> {
        let val = match op {
            INSTR_ALLOC_ARRAY => {
                let size = self.stack.pop();
                ensure_type(&size, ValueRepr::Integer)?;
                let arr = Array::make(self, size.get_integer() as usize);
                Value::from(arr)
            }
            INSTR_ALLOC_BYTES => {
                let size = self.stack.pop();
                let fill = self.stack.pop().get_integer() as u8;
                ensure_type(&size, ValueRepr::Integer)?;
                let bytes = Bytes::make(self, size.get_integer() as usize, fill);
                Value::from(bytes)
            }
            INSTR_ALLOC_FLOAT => {
                let val = self.stack.pop();
                ensure_type(&val, ValueRepr::Bytes)?;
                let b = val.get_bytes();
                let x = b.get_slice().first_chunk::<8>().unwrap();
                let f = f64::from_ne_bytes(*x);
                let v = Value::alloc_float(self, f);
                v
            }
            INSTR_ALLOC_TABLE => Value::from(Table::make(self)),
            INSTR_ALLOC_CLOSURE => {
                let code = self.stack.pop();
                ensure_type(&code, ValueRepr::Bytes)?;
                let captures = self.stack.pop();
                let arity = self.stack.pop();
                ensure_type(&arity, ValueRepr::Integer)?;
                let vararg = self.stack.pop();
                let code_bytes = code.get_bytes();

                let closure =
                    Closure::make_low(self, &code_bytes, captures, arity, vararg);
                Value::from(closure)
            }
            _ => unreachable!(),
        };
        self.stack_push(val);
        Ok(false)
    }

    fn op_array_get(&mut self) -> Result<bool> {
        let obj = self.stack.pop();
        let pos = self.stack.pop();
        let val = obj.get_array().at(pos.get_integer() as usize);
        self.stack_push(val);
        Ok(false)
    }

    fn op_array_set(&mut self) -> Result<bool> {
        let obj = self.stack.pop();
        let pos = self.stack.pop();
        let val = self.stack.pop();
        obj.get_array().set(pos.get_integer() as usize, val);
        Ok(false)
    }

    fn op_array_size(&mut self) -> Result<bool> {
        let obj = self.stack.pop();
        let val = Value::integer(obj.get_array().size() as i64);
        self.stack_push(val);
        Ok(false)
    }

    fn op_bytes_get(&mut self) -> Result<bool> {
        let obj = self.stack.pop();
        let pos = self.stack.pop();
        let val = obj.get_bytes().at(pos.get_integer() as usize);
        self.stack_push(Value::integer(val as i64));
        Ok(false)
    }

    fn op_bytes_set(&mut self) -> Result<bool> {
        let obj = self.stack.pop();
        let pos = self.stack.pop();
        let byte = self.stack.pop();
        let val = byte.get_integer() as u8;
        obj.get_bytes().set(pos.get_integer() as usize, val);
        Ok(false)
    }

    fn op_bytes_size(&mut self) -> Result<bool> {
        let obj = self.stack.pop();
        let val = Value::integer(obj.get_bytes().size() as i64);
        self.stack_push(val);
        Ok(false)
    }

    fn op_table_get(&mut self) -> Result<bool> {
        let obj = self.stack.pop();
        let sym = self.stack.pop();
        let val = obj.get_table().get(sym);
        self.stack_push(val);
        Ok(false)
    }

    fn op_table_set(&mut self) -> Result<bool> {
        let obj = self.stack.pop();
        let sym = self.stack.pop();
        let val = self.stack.pop();
        obj.get_table().insert(self, sym, val);
        Ok(false)
    }

    fn op_table_size(&mut self) -> Result<bool> {
        let obj = self.stack.pop();
        let val = Value::integer(obj.get_table().size() as i64);
        self.stack_push(val);
        Ok(false)
    }

    fn op_halt(&mut self) -> Result<bool> {
        Ok(true)
    }

    fn op_get_local(&mut self) -> Result<bool> {
        let index = self.stack.pop().get_integer() as usize;
        let val = self.local_env.get_array().at(index);
        self.stack_push(val);
        Ok(false)
    }

    fn op_set_local(&mut self) -> Result<bool> {
        let index = self.stack.pop().get_integer() as usize;
        let val = self.stack.pop();
        self.local_env.get_array().set(index, val);
        Ok(false)
    }

    fn op_global_env(&mut self) -> Result<bool> {
        self.stack_push(self.global_env);
        Ok(false)
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
        if self.stack.top == self.stack.array.size() {
            self.stack = self.stack_expand(self.stack);
        }
        self.stack.push(val);
    }

    fn stack_expand(&mut self, mut stack: Stack) -> Stack {
        let old_array = stack.array;
        let size = old_array.size();
        let mut new_array = Array::make(self, size*2);
        let values = old_array.values();
        new_array.fill(values, 0, values.len());
        stack.array = new_array;
        stack
    }

    fn ensure_stack(&mut self, extra:usize) {
        if self.stack.top + extra == self.stack.array.size() {
            self.stack = self.stack_expand(self.stack);
        }
    }

    fn ensure_call_stack(&mut self, extra:usize) {
        if self.stack.top + extra == self.call_stack.array.size() {
            self.call_stack = self.stack_expand(self.call_stack);
        }
    }

    fn make_underapplied_closure(&mut self, inner:Value, args: &[Value], arity:usize) -> Value {
        // Todo: see if we can get the instructions from the bytecode compiler
        let instrs = Vec::new();
        for i in 0..args.len() {

        }
        let code = heap::Code::Bytecode(instrs);
        let closure = Closure::make(self, &code, args, arity, None);
        Value::from(closure)
    }
}


#[derive(Debug, Clone, Copy)]
pub struct Stack {
    pub array: Array,
    top: usize,
    base: usize
}

impl Stack {
    pub(super) fn new(stack: Value) -> Self {
        Self { array: stack.get_array(), top: 0, base: 0 }
    }

    pub(super) fn push(&mut self, val : Value) {
        if self.top == self.array.size() {
            panic!("stack overflow");
        }
        self.array.set(self.top, val);
        self.top += 1;
    }

    pub(super) fn pop(&mut self) -> Value {
        if self.top == 0 {
            panic!("stack underflow");
        }
        self.top -= 1;
        let elem = self.array.at(self.top);
        self.array.set(self.top, Value::nil());
        elem
    }

    pub(super) fn take(&mut self, n:usize) -> Vec<Value> {
        let mut v = Vec::new();
        let slice = &self.array.values()[self.top-n..self.top];
        v.clone_from_slice(slice);
        self.top -= n;
        v
    }

    pub(super) fn pushv(&mut self, vals:&[Value]) {
        let n = vals.len();
        if self.top + n >= self.array.size() {
            panic!("stack overflow");
        }
        self.array.fill(vals, self.top+1, self.top+n);
        self.top += n;
    }

    pub(super) fn pick(&mut self, i:usize) {
        let elem = self.array.at(self.top - i);
        self.push(elem);
    }

    pub(super) fn put(&mut self, i:usize, val:Value) {
        self.array.set(self.top - i, val);
    }

    pub(super) fn peek(&self, i:usize) -> Value {
        self.array.at(self.top - i)
    }

    pub(super) fn top(&self) -> usize {
        self.top
    }

    pub(super) fn set_top(&mut self, new_top:usize) {
        self.top = new_top;
        if self.base > self.top {
            self.base = self.top;
        }
    }

    pub(super) fn set_base(&mut self) {
        self.base = self.top;
    }

    pub(super) fn current_frame(&self) -> usize {
        self.top - self.base
    }

    pub(super) fn to_value(&self) -> Value {
        Value::from(self.array)
    }

    pub(super) fn lift(&mut self, elems:usize, dist:usize) {
        for i in 0..elems {
            let from = self.array.at(self.top-i);
            self.array.set(self.top-i+dist, from);
        }
        for i in elems..elems+dist {
            self.array.set(self.top - i, Value::nil());
        }
        self.top += dist;
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
