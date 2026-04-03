use std::{cmp::Ordering, ffi::c_void, mem, ops::{Shl, Shr}};

use libffi::low::CodePtr;
use symbol_table::static_symbol;

use crate::{common::{Error, Result, get_symbol, symbol_id}, interpreter::{Runtime, bytecode::*, ensure_type, foreign::call_foreign_function, heap::{Array, Bytes, Closure, TYPE_BYTECODE, TYPE_EXTERN, TYPE_EXTERN_CAPI, TYPE_FOREIGN, Table, Value, ValueRepr}, prims::{CapiFn, PrimFn}, set_value}};


impl Runtime {

   pub(super) fn run(&mut self) -> Result<()> {
        loop {
            let code = self.code;
            let instr = code.get_bytes().get_slice()[self.ip];
            if self.options.trace {
                dbg!(instr, self.ip);
                dbg!(&self.stack, &self.call_stack);
                //            dbg!(&self.local_env);
            }
            self.ip += 1;
            let exit = match instr {
                INSTR_PUSH_ZERO..=INSTR_PUSH_LAST_SMALL => self.op_push_small(instr),
                INSTR_NOP => Ok(false),
                INSTR_HALT => Ok(true),
                INSTR_GT..=INSTR_LTE => self.op_compare(instr),
                INSTR_BITAND..=INSTR_BITSHIFT => self.op_bitops(instr),
                INSTR_BITNOT => self.op_bitnot(),
                INSTR_ADD..=INSTR_MOD => self.op_arithmetic(instr),
                INSTR_NEG => self.op_negate(),
                INSTR_CMP..=INSTR_NEQ => self.op_compare(instr),
                INSTR_SIN..=INSTR_EXP => self.op_float(instr),
                INSTR_TOINT => self.op_toint(),
                INSTR_CALL..=INSTR_CALL_TAIL => self.op_call(instr),
                INSTR_RETURN => self.op_return(),
                INSTR_JUMP | INSTR_JUMP_IMM8 => self.op_jump(instr),
                INSTR_JZ..=INSTR_JNFALSE | INSTR_JZ_IMM8..=INSTR_JNFALSE_IMM8 => {
                    self.op_conditional_jump(instr)
                },
                INSTR_STACK_LOAD => self.op_stack_load(),
                INSTR_STACK_STORE => self.op_put(),
                INSTR_LOAD_IMM8..=INSTR_LOAD_IMM64 => self.op_load_imm(instr),
                INSTR_LOAD_BYTES_IMM => self.op_load_bytes(),
                INSTR_LOAD_BYTES8 => self.op_load_8bytes(),
                INSTR_DUP => self.op_dup(),
                INSTR_SWAP => self.op_swap(),
                INSTR_DROP => self.op_drop(),
                INSTR_ROT => self.op_rot(),
                INSTR_DROP_FRAME => self.op_drop_frame(),
                INSTR_STACK_LIFT => self.op_stack_lift(),
                INSTR_MAKE_SYMBOL => self.op_make_symbol(),
                INSTR_TYPE => self.op_type(),
                INSTR_SET_TYPE => self.op_set_type(),
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
                INSTR_TABLE_DELETE => self.op_table_delete(),
                INSTR_GET_LOCAL => self.op_get_local(),
                INSTR_SET_LOCAL => self.op_set_local(),
                INSTR_GLOBAL_ENV => self.op_global_env(),
                INSTR_IS_INTEGER..=INSTR_IS_IMMEDIATE => self.op_type_pred(instr),
                INSTR_GET_ARG => self.op_get_arg(),
                INSTR_STACK_ARRAY => self.op_stack_array(),
                INSTR_STACK_FRAME => self.op_stack_frame(),
                INSTR_ARG_COUNT => self.op_arg_count(),
                INSTR_PUSH_MINUS_ONE..=INSTR_PUSH_TRUE => self.op_push_const(instr),
                _ => self.error(&format!("unimplemented opcode: {}", instr)),
            }?;
            if exit || self.ip >= self.code.get_bytes().size() {
                return Ok(());
            }
        }
    }

    fn op_bitops(&mut self, op: u8) -> Result<bool> {
        let rhs = self.stack.pop().get_integer();
        let lhs = self.stack.pop().get_integer();
        let res = match op {
            INSTR_BITAND => lhs & rhs,
            INSTR_BITOR => lhs | rhs,
            INSTR_BITXOR => lhs ^ rhs,
            INSTR_BITSHIFT => {
                if rhs < 0 {
                    lhs.shl(rhs.abs())
                } else {
                    lhs.shr(rhs)
                }
            }
            _ => unreachable!(),
        };
        self.stack.push(Value::integer(res));
        Ok(false)
    }

    fn op_bitnot(&mut self) -> Result<bool> {
        let val = self.stack.pop().get_integer();
        self.stack.push(Value::integer(!val));
        Ok(false)
    }

    fn op_arithmetic(&mut self, op: u8) -> Result<bool> {
        let right = self.stack.pop();
        let left = self.stack.pop();
        let res = match (left.get_immediate_repr(), right.get_immediate_repr()) {
            (ValueRepr::Integer, ValueRepr::Integer) => {
                let l = left.val as i64;
                let r = right.val as i64;
                let res = match op {
                    INSTR_ADD => l + r,
                    INSTR_SUB => l - r,
                    INSTR_MUL => (l >> 3) * r,
                    INSTR_DIV => (l / r) << 3,
                    INSTR_MOD => l % r,
                    _ => unreachable!(),
                };
                Value { val: res as u64 }
            }
            (ValueRepr::Float | ValueRepr::Integer, ValueRepr::Float | ValueRepr::Integer) => {
                let lf = get_float(left);
                let rf = get_float(right);
                let r = match op {
                    INSTR_ADD => lf + rf,
                    INSTR_SUB => lf - rf,
                    INSTR_MUL => lf * rf,
                    INSTR_DIV => lf / rf,
                    INSTR_MOD => lf % rf,
                    _ => unreachable!(),
                };
                Value::alloc_float(self, r)
            }
            (ValueRepr::Pointer, ValueRepr::Integer) => {
                let p = left.get_pointer::<*mut c_void>();
                let sign = match op {
                    INSTR_ADD => 1,
                    INSTR_SUB => -1,
                    _ => return self.error("Illegal op on pointer"),
                };
                unsafe { Value::pointer(p.byte_offset(sign * right.get_integer() as isize)) }
            }
            _ => {
                let symbol = match op {
                    INSTR_ADD => static_symbol!("__add"),
                    INSTR_SUB => static_symbol!("__sub"),
                    INSTR_MUL => static_symbol!("__mul"),
                    INSTR_DIV => static_symbol!("__div"),
                    INSTR_MOD => static_symbol!("__mod"),
                    _ => unreachable!(),
                };
                let overload = self.find_overload(&left, &symbol);
                if let Some(method) = overload {
                    self.stack_push(method);
                    self.stack_push(left);
                    self.stack_push(right);
                    self.stack_push(Value::integer(3));
                    return self.op_call(INSTR_CALL);
                } else {
                    return self.error(&format!(
                        "op_arithmetic: Type {} doesn't have an overload for op: {}",
                        self.get_type_id(&left)?,
                        op
                    ));
                }
            }
        };
        self.stack_push(res);
        Ok(false)
    }

    fn op_negate(&mut self) -> Result<bool> {
        let val = self.stack.pop();
        let res = if val.is_immediate_integer() {
            Value::integer(-val.get_integer())
        } else if val.is_float() {
            let f = -val.get_float();
            Value::alloc_float(self, f)
        } else {
            let overload = self.find_overload(&val, &static_symbol!("__neg"));
            if let Some(method) = overload {
                self.stack_push(method);
                self.stack_push(val);
                self.stack_push(Value::integer(2));
                return self.op_call(INSTR_CALL);
            } else {
                return self.error(&format!(
                    "op_negate: No implementations of negate for {}",
                    self.get_type_id(&val)?
                ));
            }
        };
        self.stack_push(res);
        Ok(false)
    }

    fn op_compare(&mut self, op: u8) -> Result<bool> {
        let right = self.stack.pop();
        let left = self.stack.pop();
        let equalcheck = op == INSTR_EQ || op == INSTR_NEQ;
        let res = match (left.get_immediate_repr(), right.get_immediate_repr()) {
            (ValueRepr::Integer, ValueRepr::Integer) => {
                let l = left.val as i64;
                let r = right.val as i64;
                let order = l.cmp(&r);
                ordering_to_int(order)
            }
            (ValueRepr::Pointer, ValueRepr::Pointer) => {
                let order = left.val.cmp(&right.val);
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
            (ValueRepr::Bool, _) if !equalcheck => 0,
            (_, ValueRepr::Bool) if !equalcheck => 0,
            (ValueRepr::Nil, ValueRepr::Nil) => 0,
            (ValueRepr::Symbol, ValueRepr::Symbol) if equalcheck => {
                if left.get_symbol() == right.get_symbol() {
                    0
                } else {
                    1
                }
            }
            (x, y) => {
                let symbol = match op {
                    INSTR_GTE => static_symbol!("__gte"),
                    INSTR_GT => static_symbol!("__gt"),
                    INSTR_LTE => static_symbol!("__lte"),
                    INSTR_LT => static_symbol!("__lt"),
                    INSTR_EQ => static_symbol!("__eq"),
                    INSTR_NEQ => static_symbol!("__neq"),
                    _ => unreachable!(),
                };
                let overload = self.find_overload(&left, &symbol);
                if let Some(method) = overload {
                    self.stack_push(method);
                    self.stack_push(left);
                    self.stack_push(right);
                    self.stack_push(Value::integer(3));
                    return self.op_call(INSTR_CALL);
                }
                if equalcheck {
                    // Object identity
                    if left.val == right.val { 0 } else { -1 }
                } else {
                    return self.error(&format!("Can't compare types {:?} and {:?}", x, y));
                }
            }
        };
        let val = match op {
            INSTR_GT => {
                if res > 0 {
                    right
                } else {
                    Value::bool(false)
                }
            }
            INSTR_GTE => {
                if res >= 0 {
                    right
                } else {
                    Value::bool(false)
                }
            }
            INSTR_LT => {
                if res < 0 {
                    right
                } else {
                    Value::bool(false)
                }
            }
            INSTR_LTE => {
                if res <= 0 {
                    right
                } else {
                    Value::bool(false)
                }
            }
            INSTR_CMP => Value::integer(res),
            INSTR_EQ => Value::bool(res == 0),
            INSTR_NEQ => Value::bool(res != 0),
            _ => unreachable!(),
        };
        self.stack_push(val);
        Ok(false)
    }

    fn op_float(&mut self, op: u8) -> Result<bool> {
        let val = self.stack.pop().get_float();
        let res = match op {
            INSTR_SIN => val.sin(),
            INSTR_COS => val.cos(),
            INSTR_TAN => val.tan(),
            INSTR_ASIN => val.asin(),
            INSTR_ACOS => val.acos(),
            INSTR_ATAN => val.atan(),
            INSTR_CEILING => val.ceil(),
            INSTR_FLOOR => val.floor(),
            INSTR_ROUND => val.round(),
            INSTR_LOG => val.ln(),
            INSTR_EXP => val.exp(),
            _ => unreachable!(),
        };
        let res_val = Value::alloc_float(self, res);
        self.stack.push(res_val);
        Ok(false)
    }

    fn op_toint(&mut self) -> Result<bool> {
        let val = self.stack.pop();
        let res = match val.get_immediate_repr() {
            ValueRepr::Integer => val,
            ValueRepr::Bool => {
                if val.get_bool() {
                    Value::integer(1)
                } else {
                    Value::integer(0)
                }
            }
            ValueRepr::Char => Value::integer(u32::from(val.get_char()) as i64),
            ValueRepr::Pointer => Value::integer(val.get_cpointer::<*const c_void>().addr() as i64),
            ValueRepr::Symbol => Value::integer(symbol_id(&val.get_symbol()) as i64),
            ValueRepr::Float => Value::integer(val.get_float() as i64),
            _ => {
                if let Some(method) = self.find_overload(&val, &static_symbol!("__toint")) {
                    self.stack_push(method);
                    self.stack_push(val);
                    self.stack_push(Value::integer(2));
                    return self.op_call(INSTR_CALL);
                } else {
                    return self.error("op_toint: type not convertible to int");
                }
            }
        };
        self.stack.push(res);
        Ok(false)
    }

    pub(super) fn push_frame(&mut self) {
        self.ensure_call_stack(4);
        let frame = vec![
            self.code,
            Value::integer(self.ip as i64),
            Value::integer(self.stack.base as i64),
            self.local_env,
        ];
        self.call_stack.base = self.call_stack.top;
        self.call_stack.pushv(&frame);
    }

    pub(super) fn op_call(&mut self, op: u8) -> Result<bool> {
        let count = self.stack.pop();
        let mut args = match count.get_immediate_repr() {
            ValueRepr::Integer => count.get_integer() as usize,
            ValueRepr::CallContinuation => count.get_cc_args(),
            _ => return self.error("call arg size must be integer or call continuation"),
        };
        let fun = self.stack.peek(args);
        match fun.get_repr() {
            ValueRepr::Closure => {}
            ValueRepr::PartialApplication => {
                let pap_array = fun.get_array();
                let pap = pap_array.values();
                self.stack.lift(args - 1, pap.len() - 1);
                let room = self.stack.slice_mut(pap.len(), args - 1);
                room.copy_from_slice(pap);
                args += pap.len() - 1;
            }
            _ => {
                if let Some(method) = self.find_overload(&fun, &static_symbol!("__call")) {
                    self.stack.put(args, method);
                } else {
                    return self.error("op_call: type can't be called");
                }
            }
        };
        self.make_call(op, args)?;
        Ok(false)
    }

    fn make_call(&mut self, op: u8, args: usize) -> Result<()> {
        let closure = self.stack.peek(args).get_closure();
        let params = args - 1;
        let first = self.stack.top - args;
        if params < closure.min_args() {
            // Underapplication, create a partial application
            let cargs = self.stack.take(args);
            let pap = Array::with(self, &cargs);
            self.stack.push(Value::partial_application(pap));
            return Ok(());
        }
        let extra_args = if closure.is_vararg() {
            0
        } else {
            params - closure.min_args()
        };
        let overapplication = extra_args > 0;
        if overapplication {
            let cc = Value::call_continuation(extra_args + 1);
            self.stack_push(cc);
            let elems = self.stack.slice_mut(args + 1, 0);
            elems.rotate_right(extra_args + 1);
        }
        let old_frame_args = self.frame_args;
        self.frame_args = args as i64;
        match closure.get_tag() {
            TYPE_BYTECODE => {
                if op == INSTR_CALL {
                    // Not a tail call, set up a new frame
                    self.push_frame();
                    self.stack.base = self.stack.top() - (args - extra_args);
                } else {
                    let stack_array = self.stack.array.values();
                    let this_call = stack_array[first..self.stack.top].to_vec();
                    self.stack.set_top(self.stack.base);
                    self.stack.pushv(&this_call);
                }
                self.code = closure.code_value();
                self.ip = 0;
                self.local_env = closure.env();
            }

            TYPE_EXTERN => {
                let fun_ptr: *const () = closure.code_value().get_cpointer();
                unsafe {
                    let fun: PrimFn = mem::transmute(fun_ptr);
                    fun(self)?;
                }
                self.frame_args = old_frame_args;
            }
            TYPE_EXTERN_CAPI => {
                let fun_ptr: *const () = closure.code_value().get_cpointer();
                unsafe {
                    let fun: CapiFn = mem::transmute(fun_ptr);
                    let err = fun(self);
                    if err != 0 {
                        return self.error(&format!("Error in C API prim: err={}", err));
                    }
                }
                self.frame_args = old_frame_args;
            }
            TYPE_FOREIGN => {
                let ffi_array = closure.code_value().get_array();
                let code_ptr = ffi_array.at(0).get_pointer::<c_void>();
                let idx = ffi_array.at(1).get_integer() as usize;
                let signature = &self.ffi_signatures[idx].clone(); // TODO: fix clone
                let argv = self.stack.take(args);
                let ret = call_foreign_function(self, &CodePtr(code_ptr), signature, &argv[1..])?;
                self.stack_push(ret);
                self.frame_args = old_frame_args;
            }
            _ => {
                return self.error("Core code not supported in bytecode interpreter");
            }
        };
        Ok(())
    }

    fn op_return(&mut self) -> Result<bool> {
        if self.call_stack.is_empty() {
            return Ok(true);
        }
        let ret = self.stack.pop();
        self.stack.set_top(self.stack.base);

        if self.stack.top > 0 && self.stack.peek(1).is_call_continuation() {
            let cc = self.stack.pop();
            let args = cc.get_cc_args();
            let argv = self.stack.take(args - 1);
            self.ensure_stack(args + 2);
            self.stack_push(ret);
            self.stack.pushv(&argv);
            self.stack_push(cc);
            return self.op_call(INSTR_CALL_TAIL);
        } else {
            let old_env = self.call_stack.pop();
            let old_base = self.call_stack.pop().get_integer() as usize;
            let ip = self.call_stack.pop().get_integer() as usize;
            let code = self.call_stack.pop();
            if self.call_stack.top > 3 {
                self.call_stack.base -= 4;
            }

            self.code = code;
            self.ip = ip;
            self.local_env = old_env;
            self.frame_args = self.stack.base as i64 - old_base as i64;
            self.stack.base = old_base;
        }
        self.stack_push(ret);
        Ok(false)
    }

    fn op_jump(&mut self, op: u8) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let dist = if is_immediate_jump(op) {
            let next = (code[self.ip] as i8) as i64;
            self.ip += 1;
            next
        } else {
            self.stack.pop().get_integer()
        };
        let target = (self.ip as i64 + dist) as usize;
        if target < self.code.get_bytes().size() {
            self.ip = target;
            Ok(false)
        } else {
            self.error("Jump outside of code")
        }
    }

    fn op_conditional_jump(&mut self, op: u8) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let dist = if is_immediate_jump(op) {
            let next = (code[self.ip] as i8) as i64;
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
            INSTR_JNPOS | INSTR_JNPOS_IMM8 => val.get_integer() <= 0,
            INSTR_JNNEG | INSTR_JNNEG_IMM8 => val.get_integer() >= 0,
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

    fn op_push_small(&mut self, op: u8) -> Result<bool> {
        let val = op as i64;
        self.stack_push(Value::integer(val));
        Ok(false)
    }

    fn op_push_const(&mut self, op: u8) -> Result<bool> {
        let val = match op {
            INSTR_PUSH_MINUS_ONE => Value::integer(-1),
            INSTR_PUSH_NIL => Value::nil(),
            INSTR_PUSH_TRUE => Value::bool(true),
            INSTR_PUSH_FALSE => Value::bool(false),
            _ => Value::nil(),
        };
        self.stack.push(val);
        Ok(false)
    }

    fn op_load_imm(&mut self, op: u8) -> Result<bool> {
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
        let size = u32::from_le_bytes(*v) as usize;
        let bytes = &rest[4..4 + size];
        let res = Value::from(Bytes::with(self, bytes));
        self.stack_push(res);
        self.ip += 4 + size;
        Ok(false)
    }

    fn op_load_8bytes(&mut self) -> Result<bool> {
        let bytes = self.code.get_bytes();
        let code = bytes.get_slice();
        let qword = &code[self.ip..self.ip + 8];
        let res = Value::from(Bytes::with(self, qword));
        self.stack_push(res);
        self.ip += 8;
        Ok(false)
    }

    fn op_stack_load(&mut self) -> Result<bool> {
        let slot = self.stack.pop().get_integer() as usize;
        self.stack.load_arg(slot);
        Ok(false)
    }

    fn op_put(&mut self) -> Result<bool> {
        let depth = self.stack.pop().get_integer() as usize;
        let val = self.stack.pop();
        self.stack.put(depth, val);
        Ok(false)
    }

    fn op_dup(&mut self) -> Result<bool> {
        self.stack.pick(1);
        Ok(false)
    }

    fn op_swap(&mut self) -> Result<bool> {
        let top = self.stack.array.at(self.stack.top() - 1);
        let next = self.stack.array.at(self.stack.top() - 2);
        self.stack.array.set(self.stack.top() - 1, next);
        self.stack.array.set(self.stack.top() - 2, top);
        Ok(false)
    }

    fn op_drop(&mut self) -> Result<bool> {
        let _ = self.stack.pop();
        Ok(false)
    }

    fn op_rot(&mut self) -> Result<bool> {
        let a = self.stack.pop();
        let b = self.stack.pop();
        let c = self.stack.pop();
        self.stack.push(a);
        self.stack.push(c);
        self.stack.push(b);
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
        let typ = self.get_type_table(&val)?;
        self.stack_push(typ);
        Ok(false)
    }

    fn op_set_type(&mut self) -> Result<bool> {
        let typ = self.stack.pop();
        let val = self.stack.pop();
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

    fn op_alloc(&mut self, op: u8) -> Result<bool> {
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

                Value::alloc_float(self, f)
            }
            INSTR_ALLOC_TABLE => {
                Value::from(Table::make(self))
            },
            INSTR_ALLOC_CLOSURE => {
                let code = self.stack.pop();
                ensure_type(&code, ValueRepr::Bytes)?;
                let captures = self.stack.pop();
                let arity = self.stack.pop();
                ensure_type(&arity, ValueRepr::Integer)?;
                let vararg = self.stack.pop();
                let code_bytes = code.get_bytes();
                let closure = Closure::make_low(self, &code_bytes, captures, arity, vararg);
                Value::from(closure)
            }
            _ => unreachable!(),
        };
        self.stack_push(val);
        Ok(false)
    }

    fn op_type_pred(&mut self, op: u8) -> Result<bool> {
        let val = self.stack.pop();
        let res = match val.get_repr() {
            ValueRepr::Nil => op == INSTR_IS_NIL,
            ValueRepr::Undefined => false,
            ValueRepr::Bool => op == INSTR_IS_BOOL,
            ValueRepr::Integer => op == INSTR_IS_INTEGER,
            ValueRepr::Pointer => op == INSTR_IS_POINTER,
            ValueRepr::Char => op == INSTR_IS_CHAR,
            ValueRepr::Float => op == INSTR_IS_FLOAT,
            ValueRepr::BoxedInteger => false,
            ValueRepr::Symbol => op == INSTR_IS_SYMBOL,
            ValueRepr::Array => op == INSTR_IS_ARRAY,
            ValueRepr::Bytes => op == INSTR_IS_BYTES,
            ValueRepr::Table => op == INSTR_IS_TABLE,
            ValueRepr::Closure => op == INSTR_IS_CLOSURE,
            ValueRepr::Object => op == INSTR_IS_OBJECT,
            ValueRepr::PartialApplication => op == INSTR_IS_PAP,
            ValueRepr::CallContinuation => op == INSTR_IS_TABLE,
        };
        self.stack.push(Value::bool(res));
        Ok(false)
    }

    fn op_make_symbol(&mut self) -> Result<bool> {
        let id = self.stack.pop().get_integer() as u32;
        let val = Value::symbol(&get_symbol(id));
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
        if self.options.log_missing_keys && val.is_nil() {
            return self.error(&format!("op_table_get: no `{}` key", sym));
        }
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

    fn op_table_delete(&mut self) -> Result<bool> {
        let sym = self.stack.pop();
        let obj = self.stack.pop();
        obj.get_table().delete(sym);
        Ok(false)
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

    fn op_get_arg(&mut self) -> Result<bool> {
        let arg = self.stack.pop().get_integer();
        if arg < 0 {
            self.stack.load_arg((self.frame_args + arg) as usize);
        } else {
            self.stack.load_arg(arg as usize);
        };
        Ok(false)
    }

    fn op_stack_frame(&mut self) -> Result<bool> {
        let index = Value::integer(self.stack.base as i64);
        self.stack_push(index);
        Ok(false)
    }

    fn op_arg_count(&mut self) -> Result<bool> {
        let index = Value::integer(self.frame_args);
        self.stack_push(index);
        Ok(false)
    }

    fn op_stack_array(&mut self) -> Result<bool> {
        self.stack_push(self.stack.as_value());
        Ok(false)
    }

    fn op_global_env(&mut self) -> Result<bool> {
        self.stack_push(self.global_env);
        Ok(false)
    }

    pub fn error<T>(&self, msg: &str) -> Result<T> {
        Err(self.err(msg))
    }

    pub fn err(&self, msg: &str) -> Error {
        Error::Runtime {
            msg: msg.to_owned(),
            loc: None,
        }
    }

    // Because we want to allocate a new stack instead of overflowing,
    // wrap stack pushes.
    pub(super) fn stack_push(&mut self, val: Value) {
        if self.stack.top == self.stack.array.size() {
            self.stack = self.stack_expand(self.stack);
        }
        self.stack.push(val);
    }

    fn stack_expand(&mut self, mut stack: Stack) -> Stack {
        let old_array = stack.array;
        let size = old_array.size();
        let mut new_array = Array::make(self, size * 2);
        let values = old_array.values();
        new_array.fill(values, 0, values.len());
        stack.array = new_array;
        stack
    }

    pub(super) fn ensure_stack(&mut self, extra: usize) {
        if self.stack.top + extra >= self.stack.array.size() {
            self.stack = self.stack_expand(self.stack);
        }
    }

    pub(super) fn ensure_call_stack(&mut self, extra: usize) {
        if self.call_stack.top + extra >= self.call_stack.array.size() {
            self.call_stack = self.stack_expand(self.call_stack);
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Stack {
    pub array: Array,
    top: usize,
    base: usize,
}


impl Stack {
    pub(super) fn new(stack: Value) -> Self {
        Self {
            array: stack.get_array(),
            top: 0,
            base: 0,
        }
    }

    pub(super) fn is_empty(&self) -> bool {
        self.top == 0
    }

    pub(super) fn push(&mut self, val: Value) {
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

    pub(super) fn take(&mut self, n: usize) -> Vec<Value> {
        let mut v = Vec::new();
        let slice = &self.array.values()[self.top - n..self.top];
        v.extend_from_slice(slice);
        for v in &mut self.array.values_mut()[self.top - n..self.top] {
            *v = Value::nil();
        }
        self.top -= n;
        v
    }

    pub(super) fn slice(&self, n: usize, d: usize) -> &[Value] {
        let from = self.top - n - d;
        let to = self.top - d;
        &self.array.values()[from..to]
    }

    pub(super) fn slice_mut(&mut self, n: usize, d: usize) -> &mut [Value] {
        let from = self.top - n - d;
        let to = self.top - d;
        &mut self.array.values_mut()[from..to]
    }

    pub(super) fn pushv(&mut self, vals: &[Value]) {
        let n = vals.len();
        if self.top + n >= self.array.size() {
            panic!("stack overflow");
        }
        self.array.fill(vals, self.top, self.top + n);
        self.top += n;
    }

    pub(super) fn pick(&mut self, i: usize) {
        let elem = self.array.at(self.top - i);
        self.push(elem);
    }

    pub(super) fn load_arg(&mut self, i: usize) {
        let elem = self.array.at(self.base + i);
        self.push(elem);
    }

    pub(super) fn put(&mut self, i: usize, val: Value) {
        self.array.set(self.top - i, val);
    }

    pub(super) fn dip(&mut self, i: usize) {
        let val = self.pop();
        self.lift(i, 1);
        self.array.set(self.top - i, val);
    }

    pub(super) fn peek(&self, i: usize) -> Value {
        if self.top < i {
            Value::nil()
        } else {
            self.array.at(self.top - i)
        }
    }

    pub(super) fn top(&self) -> usize {
        self.top
    }

    pub(super) fn set_top(&mut self, new_top: usize) {
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

    pub(super) fn as_value(&self) -> Value {
        Value::from(self.array)
    }

    pub(super) fn lift(&mut self, elems: usize, dist: usize) {
        let v = self.take(elems);
        for _ in 0..dist {
            self.push(Value::nil());
        }
        self.pushv(&v);
    }

    pub(super) fn sink(&mut self, elems: usize, dist: usize) {
        let v = self.take(elems);
        for _ in 0..dist {
            self.pop();
        }
        self.pushv(&v);
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