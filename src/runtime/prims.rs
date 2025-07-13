use std::collections::HashMap;

use crate::common::Result;
use crate::runtime::{Runtime, Value};


impl Runtime {
    pub(super) fn call_primitive(&mut self, prim : &str, args: &Vec<Value>) -> Result<Value> {
        match prim {
            "print" => printer(args),
            _ => self.error("Boom!")
        }
    }

    pub(super) fn register_primitives(&mut self) {
        self.globals.vars.insert("print".to_owned(), Value::Primitive);
    }

}

fn printer(args: &Vec<Value>) -> Result<Value> {
    for a in args {
        print!("{:?}", a);
    }
    Ok(Value::Nil)
}
