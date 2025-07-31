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
            Primitive::Print => self.value_printer(arg),
            Primitive::StringPrint => self.print_string(arg),
            Primitive::ToString => self.to_string(arg),
            Primitive::Type => self.type_query(arg),
            Primitive::ArrayCreate => self.array_create(arg),
            Primitive::Load => self.load_prim(arg),
            Primitive::ArraySize => match arg {
                Value::Array(arr) => Ok(Value::Integer(arr.borrow().array.len() as i64)),
                _ => self.error(&format!(
                    "The argument to _prim_array_size must be an array, got {}",
                    arg
                )),
            },
            _ => self.error("Boom!"),
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
            }
            _ => self.error("Boom!"),
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
            }
            _ => self.error("Boom!"),
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
            _ => self.error("Boom!"),
        }
    }

    pub(super) fn register_primitives(&mut self) -> Result<()> {
        self.add_global("global", Value::Table(self.globals.clone()));
        self.add_global("print", Value::Primitive(Primitive::Print, Arity::Fixed(1)));
        self.add_global(
            "_prim_project",
            Value::Primitive(Primitive::Project, Arity::VarArg(2)),
        );
        self.add_global(
            "_prim_array_make",
            Value::Primitive(Primitive::ArrayMk, Arity::VarArg(1)),
        );
        self.add_global("type", Value::Primitive(Primitive::Type, Arity::Fixed(1)));
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
    Print,
    Project,
    Type,
    TypeSet,

    // Arítmethic
    Add,
    Sub,
    Mul,
    Div,

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

    // Bytes
    BytesSet,
    BytesRef,
    BytesSize,
    BytesMake,
    BytesCreate,

    // String
    StringPrint,

    // Tables
    TableCreate,
    TableGet,
    TableSet,
    TableSize,

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
    BitPop,

    // Symbols
    SymbolNamed,
    SymbolNew,

    // IO

    // System
    Load,
    Extern,
}

impl Runtime {
    fn value_printer(&self, arg: &Value) -> Result<Value> {
        print!("{}\n", arg);
        Ok(Value::Nil)
    }

    fn to_string(&self, arg: &Value) -> Result<Value> {
        let str = format!("{}", arg);
        self.make_string(&str)
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
                let val = self.make_string(t).unwrap();
                table.get(&tid).map_or(false, |id| val == id.clone())
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
                let file = str::from_utf8(name).map_err(|_| Error::runtime_error("Invalid utf8 string"))?;
                match self.load(file) {
                    Ok(_) => Ok(Value::Bool(true)),
                    Err(_) => Ok(Value::Bool(false)) 
                }
            },
            _ => self.error(&format!("_prim_load expects a string, got {}", arg)) 
        }
    }
}
