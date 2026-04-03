// This file defines the C API for Nib, it should be kept in sync with the corresponding
// C header file.

// We're in C land, tell clippy to calm down
#![allow(clippy::not_unsafe_ptr_arg_deref)]

use log::error;
use std::ffi::{CStr, c_char, c_int};

use crate::{
    ast::Module,
    common::{Error, Symbol, sym},
    interpreter::{Runtime, heap::Value},
    parser::parse_declarations,
};

pub const NIB_SUCCESS: c_int = 0;
pub const NIB_ERROR: c_int = 1;

type CValue = u64;

// Parsing

#[unsafe(no_mangle)]
pub extern "C" fn nib_parse(source: *const c_char, mod_ptr: *mut *mut Module) -> c_int {
    let Ok(c_str) = unsafe { CStr::from_ptr(source) }.to_str() else {
        return NIB_ERROR;
    };
    let mut module = Module::new(None, c_str);
    let res = parse_declarations(&mut module);
    let mod_box = Box::new(module);
    unsafe {
        *mod_ptr = Box::into_raw(mod_box);
    }
    match res {
        Ok(()) => NIB_SUCCESS,
        Err(_) => NIB_ERROR,
    }
}

// Nib runtime execution

#[unsafe(no_mangle)]
pub extern "C" fn nib_init() -> *mut Runtime {
    let rt = Box::new(Runtime::new());
    Box::into_raw(rt)
}

#[unsafe(no_mangle)]
pub extern "C" fn nib_execute(
    rt: *mut Runtime,
    name: *const c_char,
    source: *const c_char,
) -> c_int {
    let Some(runtime) = (unsafe { rt.as_mut() }) else {
        error!("nib_execute: Invalid runtime pointer");
        return NIB_ERROR;
    };
    let Ok(name_str) = unsafe { CStr::from_ptr(name) }.to_str() else {
        error!("nib_execute: Invalid name string");
        return NIB_ERROR;
    };
    let Ok(code) = unsafe { CStr::from_ptr(source) }.to_str() else {
        error!("nib_execute: Invalid source string");
        return NIB_ERROR;
    };
    let res = runtime.execute_code(name_str, code);
    if let Err(e) = res {
        if let Error::NibExit { exit_code } = e {
            return exit_code as c_int;
        }
        error!("nib_execute: {}", e);
        NIB_ERROR
    } else {
        NIB_SUCCESS
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn nib_free(rt: *mut Runtime) {
    let runtime = unsafe { Box::from_raw(rt) };
    drop(runtime); // Not really needed, but make it clear.
}

// Runtime environment

#[unsafe(no_mangle)]
pub extern "C" fn nib_get_global(rt: *mut Runtime, id: *const c_char) -> CValue {
    let Some(name) = symbol_from_cstr(id) else {
        return Value::nil().val;
    };
    unsafe { (*rt).get_global(&name).val }
}

// nib_load_prelude

// Nib data types

// Symbol
#[unsafe(no_mangle)]
pub extern "C" fn nib_symbol(id: *const c_char) -> CValue {
    let Some(name) = symbol_from_cstr(id) else {
        return Value::nil().val;
    };
    Value::symbol(&name).val
}

#[unsafe(no_mangle)]
pub extern "C" fn nib_is_symbol(sym: CValue) -> bool {
    value(sym).is_symbol()
}

#[unsafe(no_mangle)]
pub extern "C" fn nib_symbol_str(sym: CValue, len: *mut c_int) -> *const c_char {
    let v = value(sym);
    let s = v.get_symbol().as_str();
    unsafe { *len = s.len() as c_int };
    s.as_ptr() as *const c_char
}

// Helpers

const fn value(cv: CValue) -> Value {
    Value { val: cv }
}

fn symbol_from_cstr(s: *const c_char) -> Option<Symbol> {
    let Ok(name) = unsafe { CStr::from_ptr(s) }.to_str() else {
        return None;
    };
    Some(sym(name))
}
