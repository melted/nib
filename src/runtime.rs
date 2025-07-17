use std::{cell::{LazyCell, RefCell}, collections::{HashMap, HashSet}, hash::Hash, ops::Deref, rc::Rc, sync::{Arc, LazyLock}};

use crate::{common::{Error, Metadata, Result}, core, runtime::prims::{Arity, Primitive}};

pub mod heap;
pub mod table;
mod prims;


pub struct Runtime {
    metadata: HashMap<String, Metadata>,
    globals: Rc<RefCell<Table>>,
    named_symbols: HashMap<String, Symbol>
}

impl Runtime {
    pub fn new() -> Self {
        Runtime { metadata: HashMap::new(), globals: new_ref(Table::new()), named_symbols: HashMap::new() }
    }

    pub fn error<T>(&self, msg : &str) -> Result<T> {
        Err(Error::Runtime { msg: msg.to_owned() })
    }

    pub fn add_global(&mut self, name:&str, value:Value) {
        let sym = self.get_or_add_named_symbol(name);
        self.globals.borrow_mut().table.insert(sym, value);
    }

    pub fn delete_global(&mut self, name:&str) {
        let sym = self.get_or_add_named_symbol(name);
        self.globals.borrow_mut().table.remove(&sym);
    }

    pub fn get_or_add_named_symbol(&mut self, name : &str) -> Symbol {
        self.named_symbols.entry(name.to_owned()).or_insert_with(|| Symbol::named(name)).clone()
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
    Symbol(Symbol),
    Bytes(Rc<RefCell<Bytes>>),
    Array(Rc<RefCell<Array>>),
    Table(Rc<RefCell<Table>>),
    Closure(Rc<RefCell<Closure>>)
}

impl Value {

}

#[derive(Debug, Clone)]
pub struct Symbol {
    symbol_info : Rc<RefCell<SymbolInfo>>
}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol_info.as_ptr().hash(state);
    }
}

// Symbols are unique objects, so the only way they can be equal is if they are the
// same object. So let's compare pointers to the info block.
impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.symbol_info.as_ptr() == other.symbol_info.as_ptr()
    }
}

impl Eq for Symbol {
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolInfo {
    type_table : Option<Rc<RefCell<Table>>>,
    symbol : String
}

impl Symbol {
    pub fn named(s : &str) -> Self {
        let info = SymbolInfo { type_table: None, symbol: s.to_owned() };
        Symbol { symbol_info: new_ref(info) }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Table {
    type_table : Option<Rc<RefCell<Table>>>,
    table : HashMap<Symbol, Value>
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
