use std::collections::HashMap;
use std::ops::Deref;

use crate::common::Result;
use crate::runtime::{Runtime, Value};


impl Runtime {
    pub(super) fn call_primitive0(&mut self, prim : Primitive) -> Result<Value> {
        match prim {
            _ => self.error("Boom!")
        }
    }

    pub(super) fn call_primitive1(&mut self, prim : Primitive, arg: &Value) -> Result<Value> {
        match prim {
            Primitive::Print => printer(arg),
            _ => self.error("Boom!")
        }
    }

    pub(super) fn call_primitive2(&mut self, prim : Primitive, arg: &Value, arg2: &Value) -> Result<Value> {
        match prim {
            _ => self.error("Boom!")
        }
    }

    pub(super) fn call_primitive3(&mut self, prim : Primitive, arg: &Value, arg2: &Value, arg3: &Value) -> Result<Value> {
        match prim {
            _ => self.error("Boom!")
        }
    }

    pub(super) fn call_primitive_vararg(&mut self, prim : Primitive, args: &[Value]) -> Result<Value> {
        match prim {
            _ => self.error("Boom!")
        }
    }

    pub(super) fn register_primitives(&mut self) {
        self.add_global("print", Value::Primitive(Primitive::Print, Arity::OneArg));
    }

}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Arity {
    NoArg,
    OneArg,
    TwoArg,
    ThreeArg,
    VarArg
}


#[derive(Debug, Clone, PartialEq)]
pub(super) enum Primitive {
    Print

}

fn printer(arg: &Value) -> Result<Value> {
    print!("{:?}", arg);
    Ok(Value::Nil)
}
