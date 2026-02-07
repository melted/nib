// Foreign primitives for the bytecode interpreter

use std::{ffi::c_void, path::PathBuf, ptr};

use libffi::{
    low::CodePtr,
    middle::{Arg, Cif, Type, arg},
};
use minidl::Library;

use crate::{
    common::{CType, Result, Signature, sym},
    core::Arity,
    interpreter::{
        Runtime, ensure_type,
        heap::{Closure, Code, Foreign, Value, ValueRepr},
    },
};

impl Runtime {
    pub(super) fn register_foreign_interface(&mut self) -> Result<()> {
        self.register_primitive("_prim_load_library", prim_load_library, Arity::Fixed(1));
        self.register_primitive("_prim_foreign_sym", prim_foreign_sym, Arity::Fixed(2));
        self.register_primitive("_prim_foreign_import", prim_foreign_import, Arity::Fixed(4));
        self.register_primitive("_prim_peek", prim_peek, Arity::Fixed(2));
        self.register_primitive("_prim_poke", prim_peek, Arity::Fixed(3));
        Ok(())
    }

    pub fn register_signature(&mut self, signature: Signature) -> usize {
        self.ffi_signatures.push(signature);
        self.ffi_signatures.len() - 1
    }
}

fn prim_load_library(rt: &mut Runtime) -> Result<()> {
    let arg = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    if !rt.is_type(&arg, &sym("string"))? {
        return rt.error(&format!(
            "_prim_load_library takes a string argument, got {}",
            arg
        ));
    }
    let str = rt.get_string(&arg)?;
    let out = match Library::load(PathBuf::from(str)) {
        Ok(lib) => Value::cpointer(lib.as_ptr()),
        Err(err) => Value::bool(false),
    };
    rt.stack_push(out);
    Ok(())
}

fn prim_foreign_sym(rt: &mut Runtime) -> Result<()> {
    let name = rt.stack.pop();
    let lib_ptr = rt.stack.pop().get_pointer();
    let _ = rt.stack.pop(); // pop closure
    if !rt.is_type(&name, &sym("string"))? {
        return rt.error(&format!(
            "The second argument to _prim_foreign_sym must be a string, got {}",
            name
        ));
    }
    let str = rt.get_string(&name)?;
    let ptr = get_foreign_symbol(rt, &str, lib_ptr)?;
    let out = if ptr.is_null() {
        Value::nil()
    } else {
        Value::pointer(ptr)
    };
    rt.stack_push(out);
    Ok(())
}

fn get_foreign_symbol(rt: &mut Runtime, name: &str, lib_ptr: *mut c_void) -> Result<*mut c_void> {
    let mut str = name.to_owned();
    str.push('\0');
    unsafe {
        let Some(lib) = Library::from_ptr(lib_ptr) else {
            return rt.error("Failed to get library");
        };
        let Some(ptr): Option<*mut c_void> = lib.sym_opt(str) else {
            return Ok(ptr::null_mut());
        };
        Ok(ptr)
    }
}

fn prim_foreign_import(rt: &mut Runtime) -> Result<()> {
    let ret_type = rt.stack.pop();
    let arg_types = rt.stack.pop();
    let fun_spec = rt.stack.pop();
    let lib_ptr = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    ensure_type(&lib_ptr, ValueRepr::Pointer)?;

    let code = match fun_spec.get_repr() {
        ValueRepr::Pointer => fun_spec.get_pointer(),
        ValueRepr::Bytes if rt.is_type(&fun_spec, &sym("string"))? => {
            let name = rt.get_string(&fun_spec)?;
            let ptr = get_foreign_symbol(rt, &name, lib_ptr.get_pointer())?;
            if ptr.is_null() {
                return rt.error("Function pointer in prim_foreign_import is null");
            }
            ptr
        }
        _ => {
            return rt
                .error("The first argument to prim_foreign_import must be a string or a pointer");
        }
    };
    let arity = arg_types.get_array().size();
    let sign = make_signature(rt, arg_types.get_array().values(), &ret_type)?;
    let signature_handle = rt.register_signature(sign);
    let code_object = Code::Foreign(Foreign {
        code,
        signature_handle,
    });
    let closure = Closure::make(rt, &code_object, &[], arity, None);
    rt.stack_push(Value::from(closure));
    Ok(())
}

fn make_signature(rt: &mut Runtime, args: &[Value], ret: &Value) -> Result<Signature> {
    let mut args_ffi = Vec::new();
    let mut ctypes = Vec::new();
    for a in args {
        let (t, ct) = get_ffi_type(rt, a)?;
        args_ffi.push(t);
        ctypes.push(ct);
    }
    let (ret, ct_ret) = get_ffi_type(rt, ret)?;
    let signature = Signature {
        cif: Cif::new(args_ffi, ret),
        arg_types: ctypes,
        ret_type: ct_ret,
    };
    Ok(signature)
}

fn prim_peek(rt: &mut Runtime) -> Result<()> {
    let ptr = rt.stack.pop().get_pointer::<c_void>();
    let ct = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    let (_, t) = get_ffi_type(rt, &ct)?;
    unsafe {
        let out = match t {
            CType::Int8 => Value::integer((ptr as *mut i8).read() as i64),
            CType::Int16 => Value::integer((ptr as *mut i16).read() as i64),
            CType::Int32 => Value::integer((ptr as *mut i32).read() as i64),
            CType::Int64 => Value::integer((ptr as *mut i64).read() as i64),
            CType::UInt8 => Value::integer((ptr as *mut u8).read() as i64),
            CType::UInt16 => Value::integer((ptr as *mut u16).read() as i64),
            CType::UInt32 => Value::integer((ptr as *mut u32).read() as i64),
            CType::UInt64 => Value::integer((ptr as *mut u64).read() as i64),
            CType::Float32 => Value::alloc_float(rt, (ptr as *mut f32).read() as f64),
            CType::Float64 => Value::alloc_float(rt, (ptr as *mut f64).read()),
            CType::Pointer => Value::pointer((ptr as *mut *mut c_void).read()),
            CType::Void => return rt.error("_prim_peek needs a concrete type, got void"),
        };
        rt.stack_push(out);
        Ok(())
    }
}

fn prim_poke(rt: &mut Runtime) -> Result<()> {
    let value = rt.stack.pop();
    let ptr = rt.stack.pop().get_pointer::<c_void>();
    let ct = rt.stack.pop();
    let _ = rt.stack.pop(); // pop closure
    let (_, t) = get_ffi_type(rt, &ct)?;
    unsafe {
        match t {
            CType::Int8 => (ptr as *mut i8).write(i8::try_from(value.get_integer())?),
            CType::Int16 => (ptr as *mut i16).write(i16::try_from(value.get_integer())?),
            CType::Int32 => (ptr as *mut i32).write(i32::try_from(value.get_integer())?),
            CType::Int64 => (ptr as *mut i64).write(value.get_integer()),
            CType::UInt8 => (ptr as *mut u8).write(u8::try_from(value.get_integer())?),
            CType::UInt16 => (ptr as *mut u16).write(u16::try_from(value.get_integer())?),
            CType::UInt32 => (ptr as *mut u32).write(u32::try_from(value.get_integer())?),
            CType::UInt64 => (ptr as *mut u64).write(u64::try_from(value.get_integer())?),
            CType::Float32 => (ptr as *mut f32).write(value.get_float() as f32),
            CType::Float64 => (ptr as *mut f64).write(value.get_float()),
            CType::Pointer => (ptr as *mut *mut c_void).write(value.get_pointer()),
            CType::Void => return rt.error("prim_poke needs a concrete type, got void"),
        }
    }
    rt.stack_push(Value::nil());
    Ok(())
}

fn get_ffi_type(rt: &Runtime, arg: &Value) -> Result<(Type, CType)> {
    // TODO: Handle passing struct by value
    let sym = if arg.is_array() {
        arg.get_array().at(0).get_symbol()
    } else {
        arg.get_symbol()
    };
    match sym.as_str() {
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
        _ => rt.error(&format!("Invalid ffi type {}", sym)),
    }
}

pub fn get_arg(rt: &Runtime, val: &Value, ctype: &CType) -> Result<Arg> {
    match ctype {
        CType::Int8 => Ok(arg(&i8::try_from(val.get_integer())?)),
        CType::Int16 => Ok(arg(&i16::try_from(val.get_integer())?)),
        CType::Int32 => Ok(arg(&i32::try_from(val.get_integer())?)),
        CType::Int64 => Ok(arg(&val.get_integer())),
        CType::UInt8 => Ok(arg(&u8::try_from(val.get_integer())?)),
        CType::UInt16 => Ok(arg(&u16::try_from(val.get_integer())?)),
        CType::UInt32 => Ok(arg(&u32::try_from(val.get_integer())?)),
        CType::UInt64 => Ok(arg(&u64::try_from(val.get_integer())?)),
        CType::Float32 => Ok(arg(&(val.get_float() as f32))),
        CType::Float64 => Ok(arg(&val.get_float())),
        CType::Pointer => {
            let p = val.get_pointer::<c_void>();
            Ok(arg(&p))
        }
        CType::Void => rt.error("Can't use void type in argument list"),
    }
}

pub fn call_foreign_function(
    rt: &mut Runtime,
    code: &CodePtr,
    signature: &Signature,
    args: &[Value],
) -> Result<Value> {
    let mut cargs = Vec::new();
    for (a, t) in args.iter().zip(&signature.arg_types) {
        cargs.push(get_arg(rt, a, t)?);
    }
    let ret = match signature.ret_type {
        CType::Void => {
            unsafe { signature.cif.call::<c_void>(*code, &cargs) };
            Value::nil()
        }
        CType::Float32 => {
            let n = unsafe { signature.cif.call::<f32>(*code, &cargs) };
            Value::alloc_float(rt, n as f64)
        }
        CType::Float64 => {
            let n = unsafe { signature.cif.call::<f64>(*code, &cargs) };
            Value::alloc_float(rt, n)
        }
        CType::Pointer => {
            let n = unsafe { signature.cif.call::<*mut c_void>(*code, &cargs) };
            Value::pointer(n)
        }
        _ => {
            let n = unsafe { signature.cif.call::<i64>(*code, &cargs) };
            Value::integer(n)
        }
    };
    Ok(ret)
}
