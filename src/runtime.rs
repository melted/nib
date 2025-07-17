use std::{cell::{LazyCell, RefCell}, collections::{HashMap, HashSet}, ops::Deref, rc::Rc, sync::{Arc, LazyLock}};

use crate::{common::{Error, Metadata, Result}, core, runtime::prims::{Arity, Primitive}};

pub mod heap;
pub mod table;
mod prims;


pub struct Runtime {
    metadata: HashMap<String, Metadata>,
    globals: Rc<RefCell<Table>>
}

impl Runtime {
    pub fn new() -> Self {
        Runtime { metadata: HashMap::new(), globals: new_ref(Table::new()) }
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

fn new_ref<T>(val : T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(val))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Primitive(Primitive, Arity),
    Bool(bool),
    Integer(i64),
    Real(f64),
    Char(char),
    Pointer(u64),
    Symbol(Rc<RefCell<RefCell<Symbol>>>),
    Bytes(Rc<RefCell<Bytes>>),
    Array(Rc<RefCell<Array>>),
    Table(Rc<RefCell<Table>>),
    Closure(Rc<RefCell<Closure>>)
}

impl Value {

}


#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    type_table : Option<Rc<RefCell<Table>>>,
    symbol : String
}

impl Symbol {
    pub fn new(s : &str) -> Self {
        Symbol { type_table: None, symbol: s.to_owned() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Table {
    type_table : Option<Rc<RefCell<Table>>>,
    table : HashMap<String, Value>
}

impl Table {
    fn new() -> Self {
        Table { type_table: None, table: HashMap::new() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    type_table : Option<Rc<RefCell<Table>>>,
    array : Vec<Value>
}

impl Array {
    fn new() -> Self {
        Array { type_table: None, array: Vec::new() }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Bytes {
    type_table : Option<Rc<RefCell<Table>>>,
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
