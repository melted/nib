use std::{mem, path::PathBuf};

use minidl::Library;

use crate::{common::Result, core::Arity, runtime::{Runtime, Value}};

impl Runtime {
    pub(super) fn register_foreign_interface(&mut self) -> Result<()> {
        self.add_global("_prim_load_library", Value::new_extern_mut_fun(Runtime::prim_load_library, &Arity::Fixed(1)));
        Ok(())
    }

    fn prim_load_library(&mut self, args: &[Value]) -> Result<Value> {
        let arg = args[0].clone();
        if !self.is_type(&arg, "string") {
            return self.error(&format!("_prim_load_library takes a string argument, got {}", arg));
        }
        let str = self.format_string(&arg)?;
        match Library::load(PathBuf::from(str)) {
            Ok(lib) => {
                // TODO: Better representation
                unsafe {
                    let ptr:usize = mem::transmute(lib.as_ptr());
                    Ok(Value::Pointer(ptr))
                }
            }
            Err(err) => {
                // TODO: Return a better error value
                Ok(Value::Bool(false))
            }
        }
    }
}