use std::{cell::LazyCell, collections::{HashMap, HashSet}, rc::Rc, sync::{Arc, LazyLock}};

use crate::{common::{Error, Metadata, Result}, core};

pub mod heap;
pub mod table;
mod prims;


pub struct Runtime {
    metadata: HashMap<String, Metadata>,
    globals: Rc<Table>
}

impl Runtime {
    pub fn new() -> Self {
        Runtime { metadata: HashMap::new(), globals: Rc::new(Table::new()) }
    }

    pub fn error<T>(&self, msg : &str) -> Result<T> {
        Err(Error::Runtime { msg: msg.to_owned() })
    }

/*     pub fn get_runtime() -> &'static Self {
        static RUNTIME : LazyLock<Runtime> = LazyLock::new(|| {
            Runtime::new()
        });
        &RUNTIME
    }
    */
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Primitive,
    Bool(bool),
    Integer(i64),
    Real(f64),
    Char(char),
    Pointer(u64),
    Symbol(Rc<Symbol>),
    Bytes(Rc<Bytes>),
    Array(Rc<Array>),
    Table(Rc<Table>),
    Closure(Rc<Closure>)
}

impl Value {

}


#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    type_table : Option<Rc<Table>>,
    symbol : String
}

impl Symbol {
    pub fn new(s : &str) -> Self {
        Symbol { type_table: None, symbol: s.to_owned() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Table {
    type_table : Option<Rc<Table>>,
    table : HashMap<String, Value>
}

impl Table {
    fn new() -> Self {
        Table { type_table: None, table: HashMap::new() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    type_table : Option<Rc<Table>>,
    array : Vec<Value>
}

impl Array {
    fn new() -> Self {
        Array { type_table: None, array: Vec::new() }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Bytes {
    type_table : Option<Rc<Table>>,
    bytes : Vec<u8>
}

impl Bytes {
    fn new() -> Self {
        Bytes { type_table: None, bytes: Vec::new() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub code : core::Expression,
    pub vars : Vec<Value>
}
