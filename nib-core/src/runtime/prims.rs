use crate::common::{Name, Result};
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
            Primitive::Print => self.printer(arg),
            Primitive::Type => self.type_query(arg),
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
                _ => self.error(&format!("Can't add {:?} and {:?}", arg, arg2)),
            },
            Primitive::Sub => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a - b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 - b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a - *b as f64)),
                _ => self.error(&format!("Can't subtract {:?} from {:?}", arg2, arg)),
            },
            Primitive::Mul => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
                _ => self.error(&format!("Can't multiply {:?} and {:?}", arg, arg2)),
            },
            Primitive::Div => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a / b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a / b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 / b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a / *b as f64)),
                _ => self.error(&format!("Can't divide {:?} by {:?}", arg, arg2)),
            },
            Primitive::Gte => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a >= b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a >= b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool(*a as f64 >= *b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a >= *b as f64)),
                (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a >= b)),
                _ => self.error(&format!("Can't compare {:?} with {:?}", arg, arg2)),
            },
            Primitive::Gt => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a > b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a > b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool(*a as f64 > *b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a > *b as f64)),
                (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a > b)),
                _ => self.error(&format!("Can't compare {:?} with {:?}", arg, arg2)),
            },
            Primitive::Lte => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a <= b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a <= b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool(*a as f64 <= *b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a <= *b as f64)),
                (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a <= b)),
                _ => self.error(&format!("Can't compare {:?} with {:?}", arg, arg2)),
            },
            Primitive::Lt => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a < b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a < b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) < *b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a < *b as f64)),
                (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a < b)),
                _ => self.error(&format!("Can't compare {:?} with {:?}", arg, arg2)),
            },
            Primitive::Eq => match (arg, arg2) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a == b)),
                (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a == b)),
                (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool(*a as f64 == *b)),
                (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a == *b as f64)),
                (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a == b)),
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
                _ => self.error(&format!("Can't compare {:?} with {:?}", arg, arg2)),
            },
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
            _ => self.error("Boom!"),
        }
    }

    pub(super) fn register_primitives(&mut self) -> Result<()> {
        self.add_global("global", Value::Table(self.globals.clone()));
        self.add_global("print", Value::Primitive(Primitive::Print, Arity::Fixed(1)));
        self.add_global(
            "project",
            Value::Primitive(Primitive::Project, Arity::VarArg(2)),
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

        Ok(())
    }

    pub(super) fn register_type_tables(&mut self) -> Result<()> {
        self.add_global("nil_type", Value::new_table());
        let tname = self.make_string("nil")?;
        self.add_name(&Name::name("nil_type.name"), &tname)?;
        self.add_global("bool", Value::new_table());
        self.add_global("int", Value::new_table());
        self.add_global("float", Value::new_table());
        self.add_global("char", Value::new_table());
        self.add_global("pointer", Value::new_table());
        self.add_global("symbol", Value::new_table());
        self.add_global("bytes", Value::new_table());
        self.add_global("string", Value::new_table());
        self.add_global("array", Value::new_table());
        self.add_global("table", Value::new_table());
        self.add_global("function", Value::new_table());
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Primitive {
    Print,
    Project,
    Type,

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
}

impl Runtime {
    fn printer(&self, arg: &Value) -> Result<Value> {
        print!("{:?}", arg);
        Ok(Value::Nil)
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
}
