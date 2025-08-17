use std::{mem, os::raw::c_void, path::PathBuf};

use libffi::low::ffi_type;
use minidl::Library;

use crate::{
    common::Result,
    core::Arity,
    runtime::{Runtime, Value},
};

impl Runtime {
    pub(super) fn register_foreign_interface(&mut self) -> Result<()> {
        self.add_global(
            "_prim_load_library",
            Value::new_extern_mut_fun(Runtime::prim_load_library, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_foreign_sym",
            Value::new_extern_mut_fun(Runtime::prim_foreign_sym, &Arity::Fixed(2)),
        );
        Ok(())
    }

    fn prim_load_library(&mut self, args: &[Value]) -> Result<Value> {
        let arg = args[0].clone();
        if !self.is_type(&arg, "string") {
            return self.error(&format!(
                "_prim_load_library takes a string argument, got {}",
                arg
            ));
        }
        let str = self.format_string(&arg)?;
        match Library::load(PathBuf::from(str)) {
            Ok(lib) => Ok(Value::Pointer(lib.as_ptr())),
            Err(err) => Ok(Value::Bool(false)),
        }
    }

    fn prim_foreign_sym(&mut self, args: &[Value]) -> Result<Value> {
        let Value::Pointer(lib_ptr) = args[0].clone() else {
            return self
                .error("The first argument to _prim_foreign_sym must be a pointer to a library");
        };
        if !self.is_type(&args[1], "string") {
            return self.error(&format!(
                "The second argument to _prim_foreign_sym must be a string, got {}",
                &args[1]
            ));
        }
        let mut str = self.format_string(&args[1])?;
        str.push('\0');
        unsafe {
            let Some(lib) = Library::from_ptr(mem::transmute(lib_ptr)) else {
                return self.error("Failed to get library");
            };
            let Some(ptr): Option<*mut c_void> = lib.sym_opt(str) else {
                return Ok(Value::Bool(false));
            };
            Ok(Value::Pointer(ptr))
        }
    }

    pub(super) fn to_ffi_type(&self, val: &Value, ftype: ffi_type) -> Result<*mut c_void> {
        todo!()
    }

    pub(super) fn from_ffi_type(&self, val: *mut c_void, ftype: ffi_type) -> Result<Value> {
        todo!()
    }
}
