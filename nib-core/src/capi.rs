// This file defines the C API for Nib, it should be kept in sync with the corresponding 
// C header file.

use std::ffi::{c_char, c_int, CStr};

use crate::{ast::Module, parser::parse_declarations, treewalker::Runtime};

const NIB_SUCCESS:c_int = 0;
const NIB_ERROR:c_int = 1;

#[unsafe(no_mangle)]
pub extern "C" fn nib_parse(source: *const c_char, mod_ptr: *mut *mut Module)  -> c_int {
    let c_str = unsafe { CStr::from_ptr(source) };
    let res = parse_declarations(None, c_str.to_str().unwrap());
    match res {
        Ok(module) => {
            let mod_box = Box::new(module);
            unsafe { *mod_ptr = Box::into_raw(mod_box); }
            NIB_SUCCESS
        },
        Err(_) => {
            NIB_ERROR
        },
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn nib_init() -> *mut Runtime {
    let rt = Box::new(Runtime::new());
    Box::into_raw(rt)
}

#[unsafe(no_mangle)]
pub extern "C" fn nib_free(rt: *mut Runtime) {
    let runtime = unsafe { Box::from_raw(rt) };
    drop(runtime);
}

