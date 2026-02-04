// This file defines the C API for Nib, it should be kept in sync with the corresponding
// C header file.

use log::error;
use std::ffi::{CStr, c_char, c_int};

use crate::{ast::Module, parser::parse_declarations, runtime::Interpreter, treewalker::Runtime};

const NIB_SUCCESS: c_int = 0;
const NIB_ERROR: c_int = 1;

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
    let res = runtime.add_code(name_str, code);
    if let Err(e) = res {
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
