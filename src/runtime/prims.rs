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
            Primitive::Print => Self::printer(arg),
            Primitive::Type => self.type_query(arg),
            _ => self.error("Boom!")
        }
    }

    pub(super) fn call_primitive2(&mut self, prim : Primitive, arg: &Value, arg2: &Value) -> Result<Value> {
        match prim {
            Primitive::AddInt => {
                if let (Value::Integer(a), Value::Integer(b)) = (arg, arg2) {
                    Ok(Value::Integer(a + b))
                } else {
                    self.error("_add_int requires two integers")
                }
            },
            Primitive::MulInt => {
                if let (Value::Integer(a), Value::Integer(b)) = (arg, arg2) {
                    Ok(Value::Integer(a * b))
                } else {
                    self.error("_mul_int requires two integers")
                }
            },
            Primitive::SubInt => {
                if let (Value::Integer(a), Value::Integer(b)) = (arg, arg2) {
                    Ok(Value::Integer(a - b))
                } else {
                    self.error("_sub_int requires two integers")
                }
            },
            Primitive::DivInt => {
                if let (Value::Integer(a), Value::Integer(b)) = (arg, arg2) {
                    Ok(Value::Integer(a / b))
                } else {
                    self.error("_div_int requires two integers")
                }
            }, 
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
            Primitive::Project => self.project(args),
            _ => self.error("Boom!")
        }
    }

    pub(super) fn register_primitives(&mut self) {
        self.add_global("global", Value::Table(self.globals.clone()));
        self.add_global("print", Value::Primitive(Primitive::Print, Arity::OneArg));
        self.add_global("project", Value::Primitive(Primitive::Project, Arity::VarArg));
        self.add_global("type", Value::Primitive(Primitive::Type, Arity::OneArg));
        self.add_global("_prim_addi", Value::Primitive(Primitive::AddInt, Arity::TwoArg));
        self.add_global("_prim_subi", Value::Primitive(Primitive::SubInt, Arity::TwoArg));
        self.add_global("_prim_muli", Value::Primitive(Primitive::MulInt, Arity::TwoArg));
        self.add_global("_prim_divi", Value::Primitive(Primitive::DivInt, Arity::TwoArg));
    }

}

#[derive(Debug, Clone, PartialEq)]
pub enum Arity {
    NoArg,
    OneArg,
    TwoArg,
    ThreeArg,
    VarArg
}


#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Print,
    Project,
    Type,

    // Integer Math
    AddInt,
    SubInt,
    MulInt,
    DivInt,
}

impl Runtime {
    fn printer(arg: &Value) -> Result<Value> {
        print!("{:?}", arg);
        Ok(Value::Nil)
    }

    fn type_query(&self, arg: &Value) -> Result<Value> {
        todo!()
    }

    fn project(&self, args:&[Value]) -> Result<Value> {
        todo!()
    }
}