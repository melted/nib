use std::{os::raw::c_void, path::PathBuf, ptr};

use libffi::middle::{arg, Arg, Cif, CodePtr, Type};
use minidl::Library;

use crate::{
    common::Result,
    core::Arity,
    runtime::{CType, Closure, Runtime, Signature, Value},
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
        self.add_global(
            "_prim_peek",
            Value::new_extern_fun(Runtime::prim_peek, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_poke",
            Value::new_extern_fun(Runtime::prim_poke, &Arity::Fixed(3)),
        );
        self.add_global("_prim_foreign_import", Value::new_extern_mut_fun(Runtime::prim_foreign_import, &Arity::Fixed(4)));
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
        let str = self.format_string(&args[1])?;
        let ptr = self.get_foreign_symbol(&str, lib_ptr)?;
        if ptr.is_null() {
            Ok(Value::Nil)
        } else {
            Ok(Value::Pointer(ptr))
        }
    }

    fn get_foreign_symbol(&mut self, name:&str, lib_ptr: *mut c_void) -> Result<*mut c_void> {
        let mut str = name.to_owned();
        str.push('\0');
        unsafe {
            let Some(lib) = Library::from_ptr(lib_ptr) else {
                return self.error("Failed to get library");
            };
            let Some(ptr): Option<*mut c_void> = lib.sym_opt(str) else {
                return Ok(ptr::null_mut());
            };
            Ok(ptr)
        }
    }
    
    fn prim_foreign_import(&mut self, args: &[Value]) -> Result<Value> {
        let lib_ptr = args[0].get_pointer()?;
        let fun = &args[1];

        let code = match fun {
            Value::Pointer(p) => *p,
            Value::Bytes(b) if self.is_type(fun, "string") => {
                let name = self.format_string(fun)?;
                let ptr = self.get_foreign_symbol(&name, lib_ptr)?;
                if ptr.is_null() {
                    return self.error("Function pointer in prim_foreign_import is null");
                }
                ptr
            },
            _ => return self.error("The first argument to prim_foreign_import must be a string or a pointer")
        };
        let funargs = Value::get_array(&args[2])?;
        let ret = &args[3];
        let sign = self.make_signature(&funargs.borrow().array, ret)?;
        let closure = Value::new_foreign_fun( &sign, CodePtr::from_ptr(code));
        Ok(closure)
    }

    fn make_signature(&mut self, args: &[Value], ret: &Value) -> Result<Signature> {
        let mut args_ffi = Vec::new();
        let mut ctypes = Vec::new();
        for a in args {
            let (t, ct) = self.get_ffi_type(a)?;
            args_ffi.push(t);
            ctypes.push(ct);
        } 
        let (ret, ct_ret) = self.get_ffi_type(ret)?;
        let signature = Signature {
            cif:Cif::new(args_ffi, ret),
            arg_types:ctypes,
            ret_type:ct_ret
        };
        Ok(signature)
    }

    fn prim_peek(&self, args: &[Value]) -> Result<Value> {
        let (_, t) = self.get_ffi_type(&args[0])?;
        let ptr = &args[1].get_pointer()?;
        unsafe {
            match t {
                CType::Int8 => Ok(Value::from((*ptr as *mut i8).read())),
                CType::Int16 => Ok(Value::from((*ptr as *mut i16).read())),
                CType::Int32 => Ok(Value::from((*ptr as *mut i32).read())),
                CType::Int64 => Ok(Value::from((*ptr as *mut i64).read())),
                CType::UInt8 => Ok(Value::from((*ptr as *mut u8).read())),
                CType::UInt16 => Ok(Value::from((*ptr as *mut u16).read())),
                CType::UInt32 => Ok(Value::from((*ptr as *mut u32).read())),
                CType::UInt64 => Ok(Value::from((*ptr as *mut u64).read())),
                CType::Float32 => Ok(Value::from((*ptr as *mut f32).read())),
                CType::Float64 => Ok(Value::from((*ptr as *mut f64).read())),
                CType::Pointer => Ok(Value::from((*ptr as *mut *mut c_void).read())),
                CType::Void => self.error("_prim_peek needs a concrete type, got void")
            }
        }
    }

    fn prim_poke(&self, args: &[Value]) -> Result<Value> {
        let (_, t) = self.get_ffi_type(&args[0])?;
        let ptr = &args[1].get_pointer()?;
        let value = &args[2];
        unsafe {
            match t {
                CType::Int8 => (*ptr as *mut i8).write(i8::try_from(value)?),
                CType::Int16 => (*ptr as *mut i16).write(i16::try_from(value)?),
                CType::Int32 => (*ptr as *mut i32).write(i32::try_from(value)?),
                CType::Int64 => (*ptr as *mut i64).write(i64::try_from(value)?),
                CType::UInt8 => (*ptr as *mut u8).write(u8::try_from(value)?),
                CType::UInt16 => (*ptr as *mut u16).write(u16::try_from(value)?),
                CType::UInt32 => (*ptr as *mut u32).write(u32::try_from(value)?),
                CType::UInt64 => (*ptr as *mut u64).write(u64::try_from(value)?),
                CType::Float32 => (*ptr as *mut f32).write(f32::try_from(value)?),
                CType::Float64 => (*ptr as *mut f64).write(f64::try_from(value)?),
                CType::Pointer => (*ptr as *mut *mut c_void).write(value.get_pointer()?),
                CType::Void => return self.error("prim_poke needs a concrete type, got void")
            }
        }
        Ok(Value::Nil)
    }

    fn get_ffi_type(&self, arg: &Value) -> Result<(Type, CType)> {
        // TODO: Handle passing struct by value
        let sym = if let Value::Array(arr) = arg {
            Value::get_symbol(&arr.borrow().array[0])?
        } else {
            Value::get_symbol(arg)?
        };
        match sym.name().as_str() {
            "cint8" => Ok((Type::i8(), CType::Int8)),
            "cint16" => Ok((Type::i16(), CType::Int16)), 
            "cint32" => Ok((Type::i32(), CType::Int32)),
            "cint64" => Ok((Type::i64(), CType::Int64)),
            "cuint8" => Ok((Type::u8(), CType::UInt8)),
            "cuint16" => Ok((Type::u16(), CType::UInt16)),
            "cuint32" => Ok((Type::u32(), CType::UInt32)),
            "cuint64" => Ok((Type::u64(), CType::UInt64)),
            "cfloat" => Ok((Type::f32(), CType::Float32)),
            "cdouble" => Ok((Type::f64(), CType::Float32)),
            "cpointer" => Ok((Type::pointer(), CType::Pointer)),
            "cvoid" => Ok((Type::void(), CType::Void)),
            _ => self.error(&format!("Invalid ffi type {}", sym))
        }
    }

    pub fn get_arg(&mut self, val:&Value, ctype:&CType) -> Result<Arg> {
        match ctype {
            CType::Int8 => Ok(arg(&i8::try_from(val)?)),
            CType::Int16 => Ok(arg(&i16::try_from(val)?)),
            CType::Int32 => Ok(arg(&i32::try_from(val)?)),
            CType::Int64 => Ok(arg(&i64::try_from(val)?)),
            CType::UInt8 => Ok(arg(&u8::try_from(val)?)),
            CType::UInt16 => Ok(arg(&u16::try_from(val)?)),
            CType::UInt32 => Ok(arg(&u32::try_from(val)?)),
            CType::UInt64 => Ok(arg(&u64::try_from(val)?)),
            CType::Float32 => Ok(arg(&f32::try_from(val)?)),
            CType::Float64 => Ok(arg(&f64::try_from(val)?)),
            CType::Pointer => Ok(arg(&val.get_pointer()?)),
            CType::Void => self.error("Can't use void type in argument list")
        }
    }


}
