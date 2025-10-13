use std::ops::{Shl, Shr};
use std::os::raw::c_void;

use crate::common::{Error, Name, Result, Symbol};
use crate::core::Arity;
use crate::treewalker::{Runtime, Value};

impl Runtime {
    pub(super) fn register_primitives(&mut self) -> Result<()> {
        self.add_global("global", Value::Table(self.globals.clone()));
        self.add_global(
            "_prim_print_representation",
            Value::new_extern_fun(Runtime::value_printer, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_project",
            Value::new_extern_mut_fun(Runtime::project, &Arity::VarArg(2, 1)),
        );
        self.add_global(
            "_prim_array_make",
            Value::new_extern_mut_fun(|_rt, args| Ok(Value::new_array(args)), &Arity::VarArg(1, 0)),
        );
        self.add_global(
            "_prim_type",
            Value::new_extern_fun(Runtime::type_prim, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_type_set",
            Value::new_extern_mut_fun(Runtime::set_type, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_add",
            Value::new_extern_fun(Runtime::prim_add, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_sub",
            Value::new_extern_fun(Runtime::prim_sub, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_mul",
            Value::new_extern_fun(Runtime::prim_mul, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_div",
            Value::new_extern_fun(Runtime::prim_div, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_mod",
            Value::new_extern_fun(Runtime::prim_mod, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_negate",
            Value::new_extern_fun(Runtime::prim_negate, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_bitand",
            Value::new_extern_fun(Runtime::prim_bitand, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bitor",
            Value::new_extern_fun(Runtime::prim_bitor, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bitxor",
            Value::new_extern_fun(Runtime::prim_bitxor, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bitnot",
            Value::new_extern_fun(Runtime::prim_bitnot, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_bitshift",
            Value::new_extern_fun(Runtime::prim_bitshift, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_gte",
            Value::new_extern_fun(Runtime::prim_gte, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_gt",
            Value::new_extern_fun(Runtime::prim_gt, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_lte",
            Value::new_extern_fun(Runtime::prim_lte, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_lt",
            Value::new_extern_fun(Runtime::prim_lt, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_eq",
            Value::new_extern_fun(Runtime::prim_eq, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_array_ref",
            Value::new_extern_fun(Runtime::prim_array_ref, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_array_set",
            Value::new_extern_fun(Runtime::prim_array_set, &Arity::Fixed(3)),
        );
        self.add_global(
            "_prim_array_create",
            Value::new_extern_fun(Runtime::array_create, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_array_size",
            Value::new_extern_fun(Runtime::prim_array_size, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_array_match",
            Value::new_extern_fun(Runtime::prim_array_match, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_match",
            Value::new_extern_fun(Runtime::prim_match, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_string_print",
            Value::new_extern_fun(Runtime::print_string, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_to_string",
            Value::new_extern_fun(Runtime::to_string, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_load",
            Value::new_extern_mut_fun(Runtime::load_prim, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_symbol_name",
            Value::new_extern_fun(Runtime::prim_symbol_name, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_symbol_make",
            Value::new_extern_fun(Runtime::prim_symbol_make, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_apply",
            Value::new_extern_mut_fun(Runtime::prim_apply, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bytes_ref",
            Value::new_extern_fun(Runtime::prim_bytes_ref, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bytes_set",
            Value::new_extern_fun(Runtime::prim_bytes_set, &Arity::Fixed(3)),
        );
        self.add_global(
            "_prim_bytes_make",
            Value::new_extern_mut_fun(Runtime::bytes_make, &Arity::VarArg(1, 0)),
        );
        self.add_global(
            "_prim_bytes_create",
            Value::new_extern_fun(Runtime::prim_bytes_create, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bytes_size",
            Value::new_extern_fun(Runtime::prim_bytes_size, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_table_create",
            Value::new_extern_fun(|_, _| Ok(Value::new_table()), &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_table_set",
            Value::new_extern_fun(Runtime::prim_table_set, &Arity::Fixed(3)),
        );
        self.add_global(
            "_prim_table_size",
            Value::new_extern_fun(Runtime::prim_table_size, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_table_keys",
            Value::new_extern_fun(Runtime::table_keys, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_table_delete",
            Value::new_extern_fun(Runtime::prim_table_delete, &Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_exit",
            Value::new_extern_fun(Runtime::nib_exit, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_panic",
            Value::new_extern_fun(Runtime::nib_panic, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_string_pack",
            Value::new_extern_fun(Runtime::prim_string_pack, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_string_unpack",
            Value::new_extern_fun(Runtime::prim_string_unpack, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_string_substring",
            Value::new_extern_fun(Runtime::prim_string_sub, &Arity::Fixed(3)),
        );
        self.add_global(
            "_prim_to_char",
            Value::new_extern_fun(Runtime::prim_to_char, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_ceiling",
            Value::new_extern_fun(Runtime::prim_ceiling, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_floor",
            Value::new_extern_fun(Runtime::prim_floor, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_round",
            Value::new_extern_fun(Runtime::prim_round, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_sin",
            Value::new_extern_fun(Runtime::prim_sin, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_cos",
            Value::new_extern_fun(Runtime::prim_cos, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_tan",
            Value::new_extern_fun(Runtime::prim_tan, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_asin",
            Value::new_extern_fun(Runtime::prim_asin, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_acos",
            Value::new_extern_fun(Runtime::prim_acos, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_atan",
            Value::new_extern_fun(Runtime::prim_atan, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_log",
            Value::new_extern_fun(Runtime::prim_log, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_exp",
            Value::new_extern_fun(Runtime::prim_exp, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_to_int",
            Value::new_extern_fun(Runtime::prim_to_int, &Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_to_pointer",
            Value::new_extern_fun(Runtime::prim_to_pointer, &Arity::Fixed(1)),
        );
        Ok(())
    }

    pub(super) fn register_system_constants(&mut self) -> Result<()> {
        self.add_name(
            &Name::str("system.os"),
            &self.make_string(std::env::consts::OS)?,
        )?;
        self.add_name(
            &Name::str("system.os_family"),
            &self.make_string(std::env::consts::FAMILY)?,
        )?;
        self.add_name(
            &Name::str("system.arch"),
            &self.make_string(std::env::consts::ARCH)?,
        )?;
        self.add_name(
            &Name::str("system.dll_extension"),
            &self.make_string(std::env::consts::DLL_EXTENSION)?,
        )?;
        self.add_name(
            &Name::str("system.dll_prefix"),
            &self.make_string(std::env::consts::DLL_PREFIX)?,
        )?;
        self.add_name(
            &Name::str("system.exe_extension"),
            &self.make_string(std::env::consts::EXE_EXTENSION)?,
        )?;
        Ok(())
    }

    pub(super) fn register_type_tables(&mut self) {
        // Since strings created without the string table present will
        // not have the string type, create that first.
        self.register_type("string", "string");
        self.register_type("nil_type", "nil");
        self.register_type("bool", "bool");
        self.register_type("int", "int");
        self.register_type("float", "float");
        self.register_type("char", "char");
        self.register_type("pointer", "pointer");
        self.register_type("symbol", "symbol");
        self.register_type("bytes", "bytes");
        self.register_type("array", "array");
        self.register_type("table", "table");
        self.register_type("function", "function");
    }

    fn register_type(&mut self, table_name: &str, type_name: &str) {
        self.add_global(table_name, Value::new_table());
        let tname = self.make_string(type_name).unwrap();
        self.add_name(&Name::str(&format!("{}.type_id", table_name)), &tname)
            .unwrap();
    }

    pub(super) fn prim_to_int(&self, args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Real(f) => Ok(Value::Integer(*f as i64)),
            Value::Bool(b) => Ok(Value::Integer(if *b { 1 } else { 0 })),
            Value::Char(c) => Ok(Value::Integer(*c as i64)),
            Value::Pointer(p) => Ok(Value::Integer(p.addr() as i64)),
            _ => self.error("The argument to _prim_to_int must be a float or bool"),
        }
    }

    pub(super) fn prim_to_pointer(&self, args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Bytes(b) => {
                let ptr = b.borrow().bytes.as_ptr();
                Ok(Value::from(ptr))
            }
            Value::Integer(n) => {
                let u = *n as usize;
                Ok(Value::Pointer(u as *mut c_void))
            }
            _ => self.error("The argument to _prim_to_pointer must be an integer or bytes array"),
        }
    }

    pub(super) fn prim_to_char(&self, args: &[Value]) -> Result<Value> {
        let cp = u32::try_from(&args[0])?;
        if let Some(c) = char::from_u32(cp) {
            Ok(Value::Char(c))
        } else {
            Ok(Value::Bool(false))
        }
    }

    pub(super) fn prim_table_size(&self, args: &[Value]) -> Result<Value> {
        let table = &args[0].get_table()?;
        Ok(Value::from(table.borrow().table.len() as i64))
    }

    pub(super) fn prim_bitnot(&self, args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Integer(a) => Ok(Value::Integer(!a)),
            _ => self.error("The argument to _prim_bitnot must be an integer or pointer"),
        }
    }

    pub(super) fn prim_array_size(&self, args: &[Value]) -> Result<Value> {
        let array = &args[0].get_array()?;
        Ok(Value::from(array.borrow().array.len() as i64))
    }

    pub(super) fn prim_bytes_size(&self, args: &[Value]) -> Result<Value> {
        let bytes = &args[0].get_bytes()?;
        Ok(Value::from(bytes.borrow().bytes.len() as i64))
    }

    pub(super) fn prim_symbol_name(&self, args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Symbol(sym) => {
                let name = sym.as_str();
                if !name.is_empty() {
                    Ok(self.make_string(name)?)
                } else {
                    Ok(Value::Bool(false))
                }
            }
            _ => self.error("The argument to _prim_symbol_name must be a symbol"),
        }
    }

    pub(super) fn prim_symbol_make(&self, args: &[Value]) -> Result<Value> {
        let str = String::try_from(&args[0])?;
        Ok(Value::Symbol(self.get_symbol(&str)))
    }

    pub(super) fn prim_ceiling(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.ceil()))
    }

    pub(super) fn prim_floor(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.floor()))
    }

    pub(super) fn prim_round(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.round()))
    }

    pub(super) fn prim_sin(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.sin()))
    }

    pub(super) fn prim_cos(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.cos()))
    }

    pub(super) fn prim_tan(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.tan()))
    }

    pub(super) fn prim_asin(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.asin()))
    }

    pub(super) fn prim_acos(&self, args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Real(f) => Ok(Value::Real(f.acos())),
            _ => self.error("The argument to _prim_acos must be a float"),
        }
    }

    pub(super) fn prim_atan(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.acos()))
    }

    pub(super) fn prim_log(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.ln()))
    }

    pub(super) fn prim_exp(&self, args: &[Value]) -> Result<Value> {
        let f = f64::try_from(&args[0])?;
        Ok(Value::from(f.exp()))
    }

    pub(super) fn prim_negate(&self, args: &[Value]) -> Result<Value> {
        match &args[0] {
            Value::Integer(n) => Ok(Value::Integer(-n)),
            Value::Real(f) => Ok(Value::Real(-f)),
            _ => self.error("The argument to _prim_negate must be an int or a float"),
        }
    }

    pub(super) fn prim_string_pack(&self, args: &[Value]) -> Result<Value> {
        let a = &args[0].get_array()?;
        let mut chars = Vec::new();
        for v in &a.borrow().array {
            chars.push(char::try_from(v)?)
        }
        let str = String::from_iter(chars.iter());
        self.make_string(&str)
    }

    pub(super) fn prim_string_unpack(&self, args: &[Value]) -> Result<Value> {
        if self.is_type(&args[0], "string") {
            let str = self.format_string(&args[0])?;
            let vals: Vec<Value> = str.chars().map(Value::Char).collect();
            Ok(Value::new_array(&vals))
        } else {
            self.error("The argument to _prim_string_unpack must be a string")
        }
    }

    pub(super) fn prim_add(&self, args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a + b)),
            (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 + b)),
            (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a + *b as f64)),
            (Value::Pointer(p), Value::Integer(n)) => unsafe {
                Ok(Value::Pointer(p.byte_offset(*n as isize)))
            },
            (arg, arg2) => self.error(&format!("Can't add {} and {}", arg, arg2)),
        }
    }

    pub(super) fn prim_sub(&self, args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a - b)),
            (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 - b)),
            (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a - *b as f64)),
            (Value::Pointer(p), Value::Integer(n)) => unsafe {
                Ok(Value::Pointer(p.byte_offset((-*n) as isize)))
            },
            (arg, arg2) => self.error(&format!("Can't subtract {} from {}", arg2, arg)),
        }
    }

    pub(super) fn prim_mul(&self, args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
            (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
            (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
            (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
            (arg, arg2) => self.error(&format!("Can't multiply {} and {}", arg, arg2)),
        }
    }

    pub(super) fn prim_div(&self, args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a / b)),
            (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a / b)),
            (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 / b)),
            (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a / *b as f64)),
            (arg, arg2) => self.error(&format!("Can't divide {} by {}", arg, arg2)),
        }
    }

    pub(super) fn prim_mod(&self, args: &[Value]) -> Result<Value> {
        let a = i64::try_from(&args[0])?;
        let b = i64::try_from(&args[1])?;
        Ok(Value::from(a % b))
    }

    pub(super) fn prim_gte(&self, args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(if a >= b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Real(a), Value::Real(b)) => Ok(if a >= b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Integer(a), Value::Real(b)) => Ok(if *a as f64 >= *b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Real(a), Value::Integer(b)) => Ok(if *a >= *b as f64 {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Char(a), Value::Char(b)) => Ok(if a >= b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Bool(b), _) => Ok(Value::Bool(false)),
            (_, Value::Bool(b)) => Ok(Value::Bool(false)),
            (arg, arg2) => self.error(&format!("Can't compare {} and {}", arg, arg2)),
        }
    }

    pub(super) fn prim_gt(&self, args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(if a > b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Real(a), Value::Real(b)) => Ok(if a > b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Integer(a), Value::Real(b)) => Ok(if *a as f64 > *b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Real(a), Value::Integer(b)) => Ok(if *a > *b as f64 {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Char(a), Value::Char(b)) => Ok(if a > b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Bool(b), _) => Ok(Value::Bool(false)),
            (_, Value::Bool(b)) => Ok(Value::Bool(false)),
            (arg, arg2) => self.error(&format!("Can't compare {} and {}", arg, arg2)),
        }
    }

    pub(super) fn prim_lte(&self, args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(if a <= b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Real(a), Value::Real(b)) => Ok(if a <= b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Integer(a), Value::Real(b)) => Ok(if *a as f64 <= *b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Real(a), Value::Integer(b)) => Ok(if *a <= *b as f64 {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Char(a), Value::Char(b)) => Ok(if a <= b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Bool(b), _) => Ok(Value::Bool(false)),
            (_, Value::Bool(b)) => Ok(Value::Bool(false)),
            (arg, arg2) => self.error(&format!("Can't compare {} and {}", arg, arg2)),
        }
    }

    pub(super) fn prim_lt(&self, args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(if a < b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Real(a), Value::Real(b)) => Ok(if a < b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Integer(a), Value::Real(b)) => Ok(if (*a as f64) < *b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Real(a), Value::Integer(b)) => Ok(if *a < *b as f64 {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Char(a), Value::Char(b)) => Ok(if a < b {
                args[1].clone()
            } else {
                Value::Bool(false)
            }),
            (Value::Bool(b), _) => Ok(Value::Bool(false)),
            (_, Value::Bool(b)) => Ok(Value::Bool(false)),
            (arg, arg2) => self.error(&format!("Can't compare {} and {}", arg, arg2)),
        }
    }

    pub(super) fn prim_eq(&self, args: &[Value]) -> Result<Value> {
        match (&args[0], &args[1]) {
            (x, y) if std::mem::discriminant(x) == std::mem::discriminant(y) => {
                Ok(Value::Bool(x == y))
            }
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a == b)),
            (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a == b)),
            (arg, arg2) => Ok(Value::Bool(false)),
        }
    }

    pub(super) fn prim_array_ref(&self, args: &[Value]) -> Result<Value> {
        let arr = args[0].get_array()?;
        let array = &arr.borrow().array;
        let n = usize::try_from(&args[1])?;
        if n < array.len() {
            Ok(array[n].clone())
        } else {
            self.error("prim_array_ref index out of bounds")
        }
    }

    pub(super) fn prim_bitand(&self, args: &[Value]) -> Result<Value> {
        let a = i64::try_from(&args[0])?;
        let b = i64::try_from(&args[1])?;
        Ok(Value::from(a & b))
    }

    pub(super) fn prim_bitor(&self, args: &[Value]) -> Result<Value> {
        let a = i64::try_from(&args[0])?;
        let b = i64::try_from(&args[1])?;
        Ok(Value::from(a | b))
    }

    pub(super) fn prim_bitxor(&self, args: &[Value]) -> Result<Value> {
        let a = i64::try_from(&args[0])?;
        let b = i64::try_from(&args[1])?;
        Ok(Value::from(a | b))
    }

    pub(super) fn prim_bitshift(&self, args: &[Value]) -> Result<Value> {
        let a = i64::try_from(&args[0])?;
        let b = i64::try_from(&args[1])?;
        let val = if b < 0 { a.shl(b.abs()) } else { a.shr(b) };
        Ok(Value::from(val))
    }

    pub(super) fn prim_bytes_create(&self, args: &[Value]) -> Result<Value> {
        let n = i64::try_from(&args[0])?;
        let v = u8::try_from(&args[1])?;
        let mut b = Vec::with_capacity(n as usize);
        b.resize(n as usize, v);
        Ok(Value::new_bytes(b))
    }

    pub(super) fn prim_bytes_ref(&self, args: &[Value]) -> Result<Value> {
        let b = &args[0].get_bytes()?;
        let n = i64::try_from(&args[1])? as usize;
        let bytes = &mut b.borrow_mut().bytes;
        if let Some(byte) = bytes.get(n) {
            Ok(Value::from(*byte))
        } else {
            self.error("_prim_bytes_ref index out of bounds")
        }
    }

    pub(super) fn prim_table_delete(&self, args: &[Value]) -> Result<Value> {
        let tab = &args[0].get_table()?;
        let sym = &args[1].get_symbol()?;
        let table = &mut tab.borrow_mut().table;
        let res = table.remove(sym).is_some();
        Ok(Value::from(res))
    }

    pub(super) fn prim_array_set(&self, args: &[Value]) -> Result<Value> {
        let arr = &args[0].get_array()?;
        let n = i64::try_from(&args[1])? as usize;
        let array = &mut arr.borrow_mut().array;
        if n < array.len() {
            array[n] = args[2].clone();
            Ok(Value::Nil)
        } else {
            self.error("index out of bounds in _prim_array set")
        }
    }

    pub(super) fn prim_array_match(&self, args: &[Value]) -> Result<Value> {
        let arr = &args[0];
        if matches!(arr, &Value::Array(_)) {
            Ok(arr.clone())
        } else {
            Ok(Value::Bool(false))
        }
    }

    pub(super) fn prim_match(&self, args: &[Value]) -> Result<Value> {
        let matcher = &args[0];
        match matcher {
            Value::Closure(_) => Ok(matcher.clone()),
            Value::Table(t) => {
                let table = &t.borrow().table;
                let sym = Symbol::from_ref("match");
                if let Some(m) = table.get(&sym) {
                    if matches!(m, Value::Closure(_)) {
                        Ok(m.clone())
                    } else {
                        self.error("table matcher used in custom pattern must be a function")
                    }
                } else {
                    self.error("table used in custom pattern must have a `match` function")
                }
            }
            _ => self
                .error("custom matcher must be either a function or a table with a `match` entry"),
        }
    }

    pub(super) fn prim_bytes_set(&self, args: &[Value]) -> Result<Value> {
        let b = &args[0].get_bytes()?;
        let n = i64::try_from(&args[1])? as usize;
        let v = u8::try_from(&args[2])?;
        let bytes = &mut b.borrow_mut().bytes;
        if n < bytes.len() {
            bytes[n] = v;
            Ok(Value::Nil)
        } else {
            self.error("index out of bounds in _prim_bytes set")
        }
    }

    pub(super) fn prim_table_set(&self, args: &[Value]) -> Result<Value> {
        let tab = &args[0].get_table()?;
        let sym = &args[1].get_symbol()?;
        let table = &mut tab.borrow_mut().table;
        table.insert(sym.clone(), args[2].clone());
        Ok(Value::Nil)
    }

    pub(super) fn prim_string_sub(&self, args: &[Value]) -> Result<Value> {
        let str = String::try_from(&args[0])?;
        let start = i64::try_from(&args[1])? as usize;
        let stop = i64::try_from(&args[2])? as usize;
        let iter = str.chars();
        let it = iter.skip(start).take(stop - start);
        let subs = String::from_iter(it);
        self.make_string(&subs)
    }

    fn value_printer(&self, args: &[Value]) -> Result<Value> {
        println!("{}", args[0]);
        Ok(Value::Nil)
    }

    pub fn to_string(&self, args: &[Value]) -> Result<Value> {
        let arg = &args[0];
        if self.is_type(arg, "string") {
            return Ok(arg.clone());
        }
        let str = format!("{}", arg);
        self.make_string(&str)
    }

    pub fn format_string(&self, arg: &Value) -> Result<String> {
        let str = if self.is_type(arg, "string") {
            String::try_from(arg)?
        } else {
            format!("{}", arg)
        };
        Ok(str)
    }

    fn print_string(&self, arg: &[Value]) -> Result<Value> {
        let str = String::try_from(&arg[0])?;
        print!("{}", str);
        Ok(Value::Integer(str.len() as i64))
    }

    fn array_create(&self, arg: &[Value]) -> Result<Value> {
        let size = i64::try_from(&arg[0])? as usize;
        let mut v = Vec::with_capacity(size);
        v.resize(size, Value::Nil);
        Ok(Value::new_array(v.as_slice()))
    }

    pub fn is_type(&self, arg: &Value, t: &str) -> bool {
        match self.type_query(arg) {
            Ok(Value::Table(type_table)) => {
                let table = &type_table.borrow().table;
                let tid = self.get_symbol("type_id");
                if let Some(Value::Bytes(b)) = table.get(&tid) {
                    str::from_utf8(&b.borrow().bytes).unwrap_or_default() == t
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn type_prim(&self, args: &[Value]) -> Result<Value> {
        self.type_query(&args[0])
    }

    pub fn type_query(&self, arg: &Value) -> Result<Value> {
        match arg {
            Value::Nil => Ok(self.get_global("nil_type").unwrap()),
            Value::Undefined => Ok(Value::Nil),
            Value::Bool(_) => Ok(self.get_global("bool").unwrap()),
            Value::Integer(_) => Ok(self.get_global("int").unwrap()),
            Value::Real(_) => Ok(self.get_global("float").unwrap()),
            Value::Char(_) => Ok(self.get_global("char").unwrap()),
            Value::Pointer(_) => Ok(self.get_global("pointer").unwrap()),
            Value::Symbol(sym) => Ok(self.get_global("symbol").unwrap()),
            Value::Array(array) => {
                if let Some(type_table) = &array.borrow().type_table {
                    Ok(Value::Table(type_table.clone()))
                } else {
                    Ok(self.get_global("array").unwrap())
                }
            }
            Value::Bytes(bytes) => {
                if let Some(type_table) = &bytes.borrow().type_table {
                    Ok(Value::Table(type_table.clone()))
                } else {
                    Ok(self.get_global("bytes").unwrap())
                }
            }
            Value::Table(table) => {
                if let Some(type_table) = &table.borrow().type_table {
                    Ok(Value::Table(type_table.clone()))
                } else {
                    Ok(self.get_global("table").unwrap())
                }
            }
            Value::Closure(fun) => {
                if let Some(type_table) = &fun.borrow().type_table {
                    Ok(Value::Table(type_table.clone()))
                } else {
                    Ok(self.get_global("function").unwrap())
                }
            }
        }
    }

    fn set_type(&mut self, args: &[Value]) -> Result<Value> {
        match &args[1] {
            Value::Table(t) => {
                match &args[0] {
                    Value::Array(arr) => {
                        let mut array = arr.borrow_mut();
                        array.type_table = Some(t.clone());
                    }
                    Value::Bytes(b) => {
                        let mut bytes = b.borrow_mut();
                        bytes.type_table = Some(t.clone());
                    }
                    Value::Table(table) => {
                        let mut table = table.borrow_mut();
                        table.type_table = Some(t.clone());
                    }
                    _ => return self.error(
                        "The first argument to _prime_type_set must be an array, bytes, or table",
                    ),
                }
            }
            _ => {
                return self.error("The second argument to _prime_type_set must be a table");
            }
        }
        Ok(Value::Nil)
    }

    fn project_table(&self, from: &Value, projection: &Value) -> Result<Value> {
        match (from, projection) {
            (Value::Table(from), Value::Symbol(sym)) => {
                let table_ref = from.borrow();
                let table = &table_ref.table;
                if table.contains_key(sym) {
                    Ok(table[sym].clone())
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => return self.error("invalid values in projection, must be tables and symbols"),
        }
    }

    fn project(&mut self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return self.error("project requires two args");
        }
        let mut from = args[0].clone();
        let mut projections = &args[1..];
        loop {
            if projections.is_empty() {
                return Ok(from);
            }
            let tt = self.type_query(&from)?;
            if let type_tab = tt.get_table()? {
                let tab = &type_tab.borrow().table;
                let res = if let Some(proj_fun) = tab.get(&self.get_symbol("project")) {
                    self.prim_apply(&vec![from, projections[0].clone()])?
                } else {
                    self.project_table(&from, &projections[0])?
                };
                from = res;
                projections = &projections[1..];
            }
        }
    }

    fn load_prim(&mut self, arg: &[Value]) -> Result<Value> {
        match &arg[0] {
            Value::Bytes(b) => {
                let name = &b.borrow().bytes;
                let file = str::from_utf8(name)
                    .map_err(|_| Error::runtime_error("Invalid utf8 string"))?;
                match self.load(file) {
                    Ok(_) => Ok(Value::Bool(true)),
                    Err(_) => Ok(Value::Bool(false)),
                }
            }
            _ => self.error(&format!("_prim_load expects a string, got {}", &arg[0])),
        }
    }

    fn prim_apply(&mut self, args: &[Value]) -> Result<Value> {
        let arr = &args[1].get_array()?;
        let mut vals = vec![args[0].clone()];
        for v in &arr.borrow().array {
            vals.push(v.clone());
        }
        self.apply_values("", &vals)
    }

    fn table_keys(&self, arg: &[Value]) -> Result<Value> {
        let tab = &arg[0].get_table()?;
        let mut keys = Vec::new();
        let table = &tab.borrow().table;
        for k in table.keys() {
            keys.push(Value::Symbol(k.clone()));
        }
        // TODO: sort the keys?
        Ok(Value::new_array(&keys))
    }

    fn nib_panic(&self, msg: &[Value]) -> Result<Value> {
        let str = self.format_string(&msg[0])?;
        Err(Error::NibPanic { msg: str })
    }

    fn nib_exit(&self, arg: &[Value]) -> Result<Value> {
        let n = i32::try_from(&arg[0])?;
        Err(Error::NibExit { exit_code: n })
    }

    fn bytes_make(&mut self, args: &[Value]) -> Result<Value> {
        let mut b = Vec::new();
        for a in args {
            let v = u8::try_from(a)?;
            b.push(v);
        }
        Ok(Value::new_bytes(b))
    }
}
