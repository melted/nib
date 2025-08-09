use std::ops::{Shl, Shr};

use crate::common::{Error, Name, Result};
use crate::core::Arity;
use crate::runtime::{Runtime, Value};

impl Runtime {
    pub(super) fn call_primitive0(&mut self, prim: &Primitive) -> Result<Value> {
        match prim {
            _ => self.error("Boom!"),
        }
    }

    pub(super) fn call_primitive1(&mut self, prim: &Primitive, arg: &Value) -> Result<Value> {
        match prim {
            Primitive::RepPrint => self.value_printer(arg),
            Primitive::StringPrint => self.print_string(arg),
            Primitive::ToString => self.to_string(arg),
            Primitive::Type => self.type_query(arg),
            Primitive::ArrayCreate => self.array_create(arg),
            Primitive::Exit => self.nib_exit(arg),
            Primitive::Panic => self.nib_panic(arg),
            Primitive::Load => self.load_prim(arg),
            Primitive::TableKeys => self.table_keys(arg),
            Primitive::TableCreate => Ok(Value::new_table()),
            Primitive::TableSize => match arg {
                Value::Table(t) => Ok(Value::Integer(t.borrow().table.len() as i64)),
                _ => self.error("The argument to _prim_table_size must be a table"),
            },
            Primitive::BitNot => match arg {
                Value::Integer(a) => Ok(Value::Integer(!a)),
                Value::Pointer(a) => Ok(Value::Pointer(!a)),
                _ => self.error("The argument to _prim_bitnot must be an integer or pointer"),
            },
            Primitive::ArraySize => match arg {
                Value::Array(arr) => Ok(Value::Integer(arr.borrow().array.len() as i64)),
                _ => self.error(&format!(
                    "The argument to _prim_array_size must be an array, got {}",
                    arg
                )),
            },
            Primitive::BytesSize => match arg {
                Value::Bytes(bytes) => Ok(Value::Integer(bytes.borrow().bytes.len() as i64)),
                _ => self.error(&format!(
                    "The argument to _prim_bytes_size must be an array, got {}",
                    arg
                )),
            },
            Primitive::SymbolName => match arg {
                Value::Symbol(sym) => {
                    let name = &sym.symbol_info.borrow().symbol;
                    if !name.is_empty() {
                        Ok(self.make_string(name)?)
                    } else {
                        Ok(Value::Bool(false))
                    }
                }
                _ => self.error("The argument to _prim_symbol_name must be a symbol"),
            },
            Primitive::Ceiling => match arg {
                Value::Real(f) => Ok(Value::Real(f.ceil())),
                _ => self.error("The argument to _prim_ceiling must be a float"),
            },
            Primitive::Floor => match arg {
                Value::Real(f) => Ok(Value::Real(f.floor())),
                _ => self.error("The argument to _prim_floor must be a float"),
            },
            Primitive::Round => match arg {
                Value::Real(f) => Ok(Value::Real(f.round())),
                _ => self.error("The argument to _prim_round must be a float"),
            },
            Primitive::Sin => match arg {
                Value::Real(f) => Ok(Value::Real(f.sin())),
                _ => self.error("The argument to _prim_sin must be a float"),
            },
            Primitive::Cos => match arg {
                Value::Real(f) => Ok(Value::Real(f.cos())),
                _ => self.error("The argument to _prim_cos must be a float"),
            },
            Primitive::Tan => match arg {
                Value::Real(f) => Ok(Value::Real(f.tan())),
                _ => self.error("The argument to _prim_tan must be a float"),
            },
            Primitive::Atan => match arg {
                Value::Real(f) => Ok(Value::Real(f.atan())),
                _ => self.error("The argument to _prim_atan must be a float"),
            },
            Primitive::Acos => match arg {
                Value::Real(f) => Ok(Value::Real(f.acos())),
                _ => self.error("The argument to _prim_acos must be a float"),
            },
            Primitive::Asin => match arg {
                Value::Real(f) => Ok(Value::Real(f.asin())),
                _ => self.error("The argument to _prim_asin must be a float"),
            },
            Primitive::Log => match arg {
                Value::Real(f) => Ok(Value::Real(f.round())),
                _ => self.error("The argument to _prim_log must be a float"),
            },
            Primitive::Exp => match arg {
                Value::Real(f) => Ok(Value::Real(f.exp())),
                _ => self.error("The argument to _prim_exp must be a float"),
            },
            Primitive::Negate => match arg {
                Value::Integer(n) => Ok(Value::Integer(-n)),
                Value::Real(f) => Ok(Value::Real(-f)),
                _ => self.error("The argument to _prim_negate must be an int or a float"),
            },
            Primitive::ToInt => match arg {
                Value::Real(f) => Ok(Value::Integer(*f as i64)),
                Value::Bool(b) => Ok(Value::Integer(if *b { 1 } else { 0 })),
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => self.error("The argument to _prim_to_int must be a float or bool"),
            },
            Primitive::ToChar => match arg {
                Value::Integer(n) => {
                    if let Some(c) = char::from_u32(*n as u32) {
                        Ok(Value::Char(c))
                    } else {
                        Ok(Value::Bool(false))
                    }
                }
                _ => self.error(
                    "The argument to _prim_to_char must be an integer between 0 and 1048576",
                ),
            },
            Primitive::StringUnpack => match arg {
                Value::Bytes(b) if self.is_type(arg, "string") => {
                    let str = self.format_string(&arg)?;
                    let vals: Vec<Value> = str.chars().map(|c| Value::Char(c)).collect();
                    Ok(Value::new_array(&vals))
                }
                _ => self.error("The argument to _prim_string_unpack must be a string"),
            },
            Primitive::StringPack => {
                match arg {
                    Value::Array(a) => {
                        let mut chars = Vec::new();
                        for v in &a.borrow().array {
                            match v {
                                Value::Char(c) => chars.push(*c),
                                _ => return self.error(
                                    "The argument to _prim_string_pack must be an array of chars",
                                ),
                            }
                        }
                        let str = String::from_iter(chars.iter());
                        self.make_string(&str)
                    }
                    _ => self.error("The argument to _prim_string_pack must be an array of chars"),
                }
            }
            _ => self.error(&format!("Primitive {:?} is not implemented", prim)),
        }
    }

    pub(super) fn call_primitive2(
        &mut self,
        prim: &Primitive,
        arg: &Value,
        arg2: &Value,
    ) -> Result<Value> {
        match prim {
            Primitive::Add => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a + b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 + b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a + *b as f64)),
                _ => self.error(&format!("Can't add {} and {}", arg, arg2)),
            },
            Primitive::Sub => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a - b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 - b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a - *b as f64)),
                _ => self.error(&format!("Can't subtract {} from {}", arg2, arg)),
            },
            Primitive::Mul => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
                _ => self.error(&format!("Can't multiply {} and {}", arg, arg2)),
            },
            Primitive::Div => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a / b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a / b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 / b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a / *b as f64)),
                _ => self.error(&format!("Can't divide {} by {}", arg, arg2)),
            },
            Primitive::Mod => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a % b)),
                _ => self.error("`mod` requires two integers as arguments")
            }
            Primitive::Gte => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a >= b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a >= b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool(*a as f64 >= *b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a >= *b as f64)),
                (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a >= b)),
                _ => self.error(&format!("Can't compare {} with {}", arg, arg2)),
            },
            Primitive::Gt => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a > b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a > b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool(*a as f64 > *b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a > *b as f64)),
                (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a > b)),
                _ => self.error(&format!("Can't compare {} with {}", arg, arg2)),
            },
            Primitive::Lte => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a <= b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a <= b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool(*a as f64 <= *b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a <= *b as f64)),
                (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a <= b)),
                _ => self.error(&format!("Can't compare {} with {}", arg, arg2)),
            },
            Primitive::Lt => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a < b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a < b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) < *b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a < *b as f64)),
                (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a < b)),
                _ => self.error(&format!("Can't compare {} with {}", arg, arg2)),
            },
            Primitive::Eq => match (arg, arg2) {
                (x, y) if std::mem::discriminant(x) == std::mem::discriminant(y) => Ok(Value::Bool(x == y)),
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a == b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a == b)),
                _ => self.error(&format!("Can't compare {} with {}", arg, arg2)),
            },
            Primitive::ArrayRef => match (arg, arg2) {
                (Value::Array(arr), Value::Integer(n)) => {
                    let array = arr.borrow();
                    Ok(array.array[*n as usize].clone())
                },
                (x, y) => self.error(&format!("The arguments to _prim_array_ref should be an array and an integer, got {} and {}", x, y))
            },
            Primitive::BitAnd => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a & b)),
                (Value::Pointer(a), Value::Pointer(b)) => Ok(Value::Pointer(a & b)),
                _ => self.error(&format!("The arguments to _prim_bitand should be int or ptr, got {} and {}", arg, arg2))
            },
            Primitive::BitOr => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a | b)),
                (Value::Pointer(a), Value::Pointer(b)) => Ok(Value::Pointer(a | b)),
                _ => self.error(&format!("The arguments to _prim_bitor should be int or ptr, got {} and {}", arg, arg2))
            },
            Primitive::BitXor => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a ^ b)),
                (Value::Pointer(a), Value::Pointer(b)) => Ok(Value::Pointer(a ^ b)),
                _ => self.error(&format!("The arguments to _prim_bitxor should be int or ptr, got {} and {}", arg, arg2))
            },
            Primitive::BitShift => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => {
                    let val = if *b < 0 { a.shl(b.abs())} else { a.shr(b) };
                    Ok(Value::Integer(val))
                },
                (Value::Pointer(a), Value::Integer(b)) => {
                    let val = if *b < 0 { a.shl(b.abs())} else { a.shr(b) };
                    Ok(Value::Pointer(val))
                },
                _ => self.error(&format!("The arguments to _prim_bitshift should be int or ptr and an int, got {} and {}", arg, arg2))
            },
            Primitive::BytesRef => match(arg, arg2) {
                (Value::Bytes(b), Value::Integer(n)) => {
                    let bytes = &b.borrow().bytes;
                    Ok(Value::Integer(bytes[*n as usize] as i64))
                },
                _ => self.error(&format!("The arguments to _prim_bytes_ref should be a bytes object and an int, got {} and {}", arg, arg2))
            },
            Primitive::BytesCreate => match (arg, arg2) {
                (Value::Integer(n), Value::Integer(v)) => {
                    if *v < 0 || *v > 255 {
                        return self.error("The second argument to _prim_bytes_create must be an integer between 0-255");
                    }
                    let mut b = Vec::with_capacity(*n as usize);
                    b.resize(*n as usize, *v as u8);
                    Ok(Value::new_bytes(b))
                },
                _ => self.error(&format!(
                    "The argument to _prim_bytes_create must be an integer, got {}",
                    arg
                ))
            },
            Primitive::TableDelete => match(arg, arg2) {
                (Value::Table(tab), Value::Symbol(sym)) => {
                    let table = &mut tab.borrow_mut().table;
                    let res = table.remove(sym).is_some();
                    Ok(Value::Bool(res))
                },
                _ => self.error(&format!("The arguments to _prim_table_delete should be a table object and a symbol, got {} and {}", arg, arg2))
            },
            Primitive::TypeSet => self.set_type(arg, arg2),
            Primitive::Apply => self.prim_apply(arg, arg2),
            _ => self.error(&format!("Primitive {:?} is not implemented", prim)),
        }
    }

    pub(super) fn call_primitive3(
        &mut self,
        prim: &Primitive,
        arg: &Value,
        arg2: &Value,
        arg3: &Value,
    ) -> Result<Value> {
        match prim {
            Primitive::ArraySet => {
                let Value::Array(arr) = arg else {
                    return self.error("First argument to _prim_array_set must be an array");
                };
                let Value::Integer(n) = arg2 else {
                    return self.error("Second argument to _prim_array_set must be an integer");
                };
                let mut array = arr.borrow_mut();
                array.array[*n as usize] = arg3.clone();
                Ok(Value::Nil)
            },
            Primitive::BytesSet => match(arg, arg2, arg3) {
                (Value::Bytes(b), Value::Integer(n), Value::Integer(v)) => {
                    if *v < 0 || *v > 255 {
                        return self.error("_prim_bytes_set, value to set is outside 0-255");
                    }
                    let bytes = &mut b.borrow_mut().bytes;
                    bytes[*n as usize] = *v as u8;
                    Ok(Value::Nil)
                },
                _ => self.error(&format!("The arguments to _prim_bytes_ref should be a bytes object, an int and an int, got {}, {} and {}", arg, arg2, arg3))
            },
            Primitive::TableSet => {
                let Value::Table(tab) = arg else {
                    return self.error("First argument to _prim_table_set must be a table");
                };
                let Value::Symbol(sym) = arg2 else {
                    return self.error("Second argument to _prim_table_set must be a symbol");
                };
                let table = &mut tab.borrow_mut().table;
                table.insert(sym.clone(), arg3.clone());
                Ok(Value::Nil)
            },
            Primitive::StringSub => match (arg, arg2, arg3) {
                (Value::Bytes(b), Value::Integer(start), Value::Integer(stop)) => {
                    let str = self.format_string(arg)?;
                    let iter = str.chars();
                    let it = iter.skip(*start as usize).take((stop-start) as usize);
                    let subs = String::from_iter(it);
                    self.make_string(&subs)
                },
                _ => self.error("_prim_string_substring takes a string, an integer and an integer as argument")
            }
            _ => self.error(&format!("Primitive {:?} is not implemented", prim)),
        }
    }

    pub(super) fn call_primitive_vararg(
        &mut self,
        prim: &Primitive,
        args: &[Value],
    ) -> Result<Value> {
        match prim {
            Primitive::Project => self.project(args),
            Primitive::ArrayMk => Ok(Value::new_array(args)),
            Primitive::BytesMake => {
                let mut b = Vec::new();
                for a in args {
                    match a {
                        Value::Integer(v) if *v >= 0 && *v < 256 => {
                            b.push(*v as u8);
                        }
                        _ => {
                            return self.error(
                                "The arguments to _prim_bytes_make must be ints between 0-255",
                            );
                        }
                    }
                }
                Ok(Value::new_bytes(b))
            }
            _ => self.error(&format!("Primitive {:?} is not implemented", prim)),
        }
    }

    pub(super) fn register_primitives(&mut self) -> Result<()> {
        self.add_global("global", Value::Table(self.globals.clone()));
        self.add_global(
            "_prim_print_representation",
            Value::Primitive(Primitive::RepPrint, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_project",
            Value::Primitive(Primitive::Project, Arity::VarArg(2)),
        );
        self.add_global(
            "_prim_array_make",
            Value::Primitive(Primitive::ArrayMk, Arity::VarArg(1)),
        );
        self.add_global(
            "_prim_type",
            Value::Primitive(Primitive::Type, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_type_set",
            Value::Primitive(Primitive::TypeSet, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_add",
            Value::Primitive(Primitive::Add, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_sub",
            Value::Primitive(Primitive::Sub, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_mul",
            Value::Primitive(Primitive::Mul, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_div",
            Value::Primitive(Primitive::Div, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_mod",
            Value::Primitive(Primitive::Mod, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_negate",
            Value::Primitive(Primitive::Negate, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_bitand",
            Value::Primitive(Primitive::BitAnd, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bitor",
            Value::Primitive(Primitive::BitOr, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bitxor",
            Value::Primitive(Primitive::BitXor, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bitnot",
            Value::Primitive(Primitive::BitNot, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_bitshift",
            Value::Primitive(Primitive::BitShift, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_gte",
            Value::Primitive(Primitive::Gte, Arity::Fixed(2)),
        );
        self.add_global("_prim_gt", Value::Primitive(Primitive::Gt, Arity::Fixed(2)));
        self.add_global(
            "_prim_lte",
            Value::Primitive(Primitive::Lte, Arity::Fixed(2)),
        );
        self.add_global("_prim_lt", Value::Primitive(Primitive::Lt, Arity::Fixed(2)));
        self.add_global("_prim_eq", Value::Primitive(Primitive::Eq, Arity::Fixed(2)));
        self.add_global(
            "_prim_array_ref",
            Value::Primitive(Primitive::ArrayRef, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_array_set",
            Value::Primitive(Primitive::ArraySet, Arity::Fixed(3)),
        );
        self.add_global(
            "_prim_array_create",
            Value::Primitive(Primitive::ArrayCreate, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_array_size",
            Value::Primitive(Primitive::ArraySize, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_string_print",
            Value::Primitive(Primitive::StringPrint, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_to_string",
            Value::Primitive(Primitive::ToString, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_load",
            Value::Primitive(Primitive::Load, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_symbol_name",
            Value::Primitive(Primitive::SymbolName, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_apply",
            Value::Primitive(Primitive::Apply, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bytes_ref",
            Value::Primitive(Primitive::BytesRef, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bytes_set",
            Value::Primitive(Primitive::BytesSet, Arity::Fixed(3)),
        );
        self.add_global(
            "_prim_bytes_make",
            Value::Primitive(Primitive::BytesMake, Arity::VarArg(1)),
        );
        self.add_global(
            "_prim_bytes_create",
            Value::Primitive(Primitive::BytesCreate, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_bytes_size",
            Value::Primitive(Primitive::BytesSize, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_table_create",
            Value::Primitive(Primitive::TableCreate, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_table_set",
            Value::Primitive(Primitive::TableSet, Arity::Fixed(3)),
        );
        self.add_global(
            "_prim_table_size",
            Value::Primitive(Primitive::TableSize, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_table_keys",
            Value::Primitive(Primitive::TableKeys, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_table_delete",
            Value::Primitive(Primitive::TableDelete, Arity::Fixed(2)),
        );
        self.add_global(
            "_prim_exit",
            Value::Primitive(Primitive::Exit, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_panic",
            Value::Primitive(Primitive::Panic, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_string_pack",
            Value::Primitive(Primitive::StringPack, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_string_unpack",
            Value::Primitive(Primitive::StringUnpack, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_string_substring",
            Value::Primitive(Primitive::StringSub, Arity::Fixed(3)),
        );
        self.add_global(
            "_prim_to_char",
            Value::Primitive(Primitive::ToChar, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_ceiling",
            Value::Primitive(Primitive::Ceiling, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_floor",
            Value::Primitive(Primitive::Floor, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_round",
            Value::Primitive(Primitive::Round, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_sin",
            Value::Primitive(Primitive::Sin, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_cos",
            Value::Primitive(Primitive::Cos, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_tan",
            Value::Primitive(Primitive::Tan, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_asin",
            Value::Primitive(Primitive::Asin, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_acos",
            Value::Primitive(Primitive::Acos, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_atan",
            Value::Primitive(Primitive::Atan, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_log",
            Value::Primitive(Primitive::Log, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_exp",
            Value::Primitive(Primitive::Exp, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_to_int",
            Value::Primitive(Primitive::ToInt, Arity::Fixed(1)),
        );
        self.add_global(
            "_prim_foreign_import",
            Value::Primitive(Primitive::ForeignImport, Arity::VarArg(1)),
        );
        self.add_global(
            "_prim_foreign_export",
            Value::Primitive(Primitive::ForeignExport, Arity::VarArg(1)),
        );
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
        self.add_name(&Name::name(&format!("{}.type_id", table_name)), &tname)
            .unwrap();
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Primitive {
    RepPrint,
    Project,
    Apply,
    Type,
    TypeSet,

    // Arítmethic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Negate,

    // Comparison
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,

    // Array
    ArrayMk,
    ArrayRef,
    ArraySet,
    ArrayCreate,
    ArraySize,

    // Conversion to string
    ToString,
    ToInt,
    ToChar,

    // Bytes
    BytesSet,
    BytesRef,
    BytesSize,
    BytesMake,
    BytesCreate,

    // String
    StringPrint,
    StringPack,
    StringUnpack,
    StringSub,

    // Tables
    TableCreate,
    TableKeys,
    TableSet,
    TableSize,
    TableDelete,

    // Float Math
    Ceiling,
    Floor,
    Round,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Log,
    Exp,

    // BitOps
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    BitShift,

    // Symbols
    SymbolNamed,
    SymbolNew,
    SymbolName,

    // FFI
    ForeignImport,
    ForeignExport,

    // System
    Load,
    Exit,
    Panic,
}

impl Runtime {
    fn value_printer(&self, arg: &Value) -> Result<Value> {
        print!("{}\n", arg);
        Ok(Value::Nil)
    }

    fn to_string(&self, arg: &Value) -> Result<Value> {
        if self.is_type(arg, "string") {
            return Ok(arg.clone());
        }
        let str = format!("{}", arg);
        self.make_string(&str)
    }

    fn format_string(&self, arg: &Value) -> Result<String> {
        let str = match arg {
            Value::Bytes(b) if self.is_type(arg, "string") => str::from_utf8(&b.borrow().bytes)
                .map_err(|_| Error::runtime_error("Invalid string in _prim_string_print"))?
                .to_owned(),
            _ => format!("{}", arg),
        };
        Ok(str)
    }

    fn print_string(&self, arg: &Value) -> Result<Value> {
        match arg {
            Value::Bytes(bytes) => {
                let b = &bytes.borrow().bytes;
                print!(
                    "{}",
                    str::from_utf8(&b).map_err(|_| Error::runtime_error(
                        "Invalid string in _prim_string_print"
                    ))?
                );
                Ok(Value::Integer(b.len() as i64))
            }
            _ => self.error(&format!("_prim_string_print takes a string, got {}", arg)),
        }
    }

    fn array_create(&self, arg: &Value) -> Result<Value> {
        let size = match arg {
            Value::Integer(n) => *n as usize,
            _ => {
                return self.error(&format!(
                    "The argument to _prim_array_create should be an integer, got {}",
                    arg
                ));
            }
        };
        let mut v = Vec::with_capacity(size);
        v.resize(size, Value::Nil);
        Ok(Value::new_array(v.as_slice()))
    }

    fn is_type(&self, arg: &Value, t: &str) -> bool {
        match self.type_query(arg) {
            Ok(Value::Table(type_table)) => {
                let table = &type_table.borrow().table;
                let Some(tid) = self.named_symbols.get("type_id").cloned() else {
                    return false;
                };
                if let Some(Value::Bytes(b)) = table.get(&tid) {
                    str::from_utf8(&b.borrow().bytes).unwrap_or_default() == t
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn type_query(&self, arg: &Value) -> Result<Value> {
        match arg {
            Value::Nil => Ok(self.get_global("nil_type").unwrap()),
            Value::Undefined => Ok(Value::Nil),
            Value::Primitive(_, _) => Ok(self.get_global("prim").unwrap()),
            Value::Bool(_) => Ok(self.get_global("bool").unwrap()),
            Value::Integer(_) => Ok(self.get_global("int").unwrap()),
            Value::Real(_) => Ok(self.get_global("float").unwrap()),
            Value::Char(_) => Ok(self.get_global("char").unwrap()),
            Value::Pointer(_) => Ok(self.get_global("pointer").unwrap()),
            Value::Symbol(sym) => {
                if let Some(type_table) = &sym.symbol_info.borrow().type_table {
                    Ok(Value::Table(type_table.clone()))
                } else {
                    Ok(self.get_global("symbol").unwrap())
                }
            }
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

    fn set_type(&mut self, arg: &Value, arg1: &Value) -> Result<Value> {
        match arg1 {
            Value::Table(t) => match arg {
                Value::Array(arr) => {
                    let mut array = arr.borrow_mut();
                    array.type_table = Some(t.clone());
                }
                Value::Bytes(b) => {
                    let mut bytes = b.borrow_mut();
                    bytes.type_table = Some(t.clone());
                }
                Value::Symbol(symb) => {
                    let mut sym_info = symb.symbol_info.borrow_mut();
                    sym_info.type_table = Some(t.clone());
                }
                Value::Table(table) => {
                    let mut table = table.borrow_mut();
                    table.type_table = Some(t.clone());
                }
                _ => return self.error(&format!(
                    "The first argument to _prime_type_set must be an array, bytes, symbol or table"
                )),
            },
            _ => {
                return self.error(&format!(
                    "The second argument to _prime_type_set must be a table"
                ));
            }
        }
        Ok(Value::Nil)
    }

    fn project(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return self.error("project requires two args");
        }
        let mut tab = args[0].clone();
        let mut slice = &args[1..];
        loop {
            match (tab, &slice[0]) {
                (Value::Table(from), Value::Symbol(sym)) => {
                    let table_ref = from.borrow();
                    let table = &table_ref.table;
                    if table.contains_key(&sym) {
                        if slice.len() > 1 {
                            tab = table[&sym].clone();
                            slice = &slice[1..];
                        } else {
                            return Ok(table[&sym].clone());
                        }
                    } else {
                        return Ok(Value::Nil);
                    }
                }
                _ => return self.error("invalid values in projection, must be tables and symbols"),
            }
        }
    }

    fn load_prim(&mut self, arg: &Value) -> Result<Value> {
        match arg {
            Value::Bytes(b) => {
                let name = &b.borrow().bytes;
                let file = str::from_utf8(name)
                    .map_err(|_| Error::runtime_error("Invalid utf8 string"))?;
                match self.load(file) {
                    Ok(_) => Ok(Value::Bool(true)),
                    Err(_) => Ok(Value::Bool(false)),
                }
            }
            _ => self.error(&format!("_prim_load expects a string, got {}", arg)),
        }
    }

    fn prim_apply(&mut self, fun: &Value, args: &Value) -> Result<Value> {
        let Value::Array(arr) = args else {
            return self.error("The second argument to _prim_apply must be an array");
        };
        let mut vals = vec![fun.clone()];
        for v in &arr.borrow().array {
            vals.push(v.clone());
        }
        self.apply_values("", &vals)
    }

    fn table_keys(&self, arg: &Value) -> Result<Value> {
        match arg {
            Value::Table(tab) => {
                let mut keys = Vec::new();
                let table = &tab.borrow().table;
                for k in table.keys() {
                    keys.push(Value::Symbol(k.clone()));
                }
                // TODO: sort the keys?
                Ok(Value::new_array(&keys))
            }
            _ => self.error("The argument to _prim_table_keys must be a table"),
        }
    }

    fn nib_panic(&mut self, msg: &Value) -> Result<Value> {
        let str = self.format_string(msg)?;
        Err(Error::NibPanic { msg: str })
    }

    fn nib_exit(&mut self, arg: &Value) -> Result<Value> {
        match arg {
            Value::Integer(n) => Err(Error::NibExit {
                exit_code: *n as i32,
            }),
            _ => self.error("Argument to _prim_exit must be an int"),
        }
    }
}
