use std::{cell::RefCell, collections::HashMap, env::args, fmt::Display, hash::Hash, rc::Rc};

use crate::{common::{Error, Metadata, Result}, core, runtime::prims::{Arity, Primitive}};

pub mod heap;
pub mod table;
mod prims;
mod evaluate;

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

    pub fn get_global(&self, name:&str) -> Option<Value> {
        let Some(sym) = self.named_symbols.get(name) else {
            return None;
        };
        self.globals.borrow().table.get(sym).cloned()
    }
}

fn new_ref<T>(val : T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(val))
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Primitive(Primitive, Arity),
    Bool(bool),
    Integer(i64),
    Real(f64),
    Char(char),
    Pointer(usize),
    Symbol(Symbol),
    Bytes(Rc<RefCell<Bytes>>),
    Array(Rc<RefCell<Array>>),
    Table(Rc<RefCell<Table>>),
    Closure(Rc<RefCell<Closure>>),
    Placeholder(Box<String>) // Hasn't been defined yet, will always be replaced before evaluating
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primitive(l0, l1), Self::Primitive(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Placeholder(l0), Self::Placeholder(r0)) => l0 == r0,
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Real(l0), Self::Real(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::Pointer(l0), Self::Pointer(r0)) => l0 == r0,
            (Self::Symbol(l0), Self::Symbol(r0)) => l0 == r0,
            (Self::Bytes(l0), Self::Bytes(r0)) => l0.as_ptr() == r0.as_ptr(),
            (Self::Array(l0), Self::Array(r0)) => l0.as_ptr() == r0.as_ptr(),
            (Self::Table(l0), Self::Table(r0)) => l0.as_ptr() == r0.as_ptr(),
            (Self::Closure(l0), Self::Closure(r0)) => l0.as_ptr() == r0.as_ptr(),
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "()"),
            Value::Primitive(primitive, arity) => write!(f, "¤<primitive:{:?}>", primitive),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Real(x) => write!(f, "{}", x),
            Value::Char(c) => write!(f, "{}", c),
            Value::Pointer(p) => write!(f, "ptr({:x})", p),
            Value::Symbol(symbol) => write!(f, "{}", symbol),
            Value::Bytes(ref_cell) => write!(f, "{}", &ref_cell.borrow()),
            Value::Array(ref_cell) => write!(f, "{}", &ref_cell.borrow()),
            Value::Table(ref_cell) => write!(f, "{}", &ref_cell.borrow()),
            Value::Closure(ref_cell) => write!(f, "{}", &ref_cell.borrow()),
            Value::Placeholder(_) => write!(f, "#<placeholder>"),
        }
    }
}

impl Value {

}

#[derive(Debug, Clone)]
pub struct Symbol {
    symbol_info : Rc<RefCell<SymbolInfo>>
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = &self.symbol_info.borrow().symbol;
        if s.is_empty() {
            write!(f, "#anonymous_symbol({:x})", self.symbol_info.as_ptr().addr())
        } else {
            write!(f, "#{}", s)
        }
    }
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

impl Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut it = self.table.iter();
        write!(f, "#t{{")?;
        if let Some(b) = it.next() {
            write!(f, "{}: {}", b.0, b.1)?;
            for v in it {
                write!(f, " ,{}: {}", b.0, b.1)?;
            }
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    type_table : Option<Rc<RefCell<Table>>>,
    array : Vec<Value>
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut it = self.array.iter();
        write!(f, "[")?;
        if let Some(b) = it.next() {
            write!(f, "{}", b)?;
            for v in it {
                write!(f, " ,{}", b)?;
            }
        }
        write!(f, "]")
    }
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

impl Display for Bytes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut it = self.bytes.iter();
        write!(f, "#[")?;
        if let Some(b) = it.next() {
            write!(f, "{}", b)?;
            for b in it {
                write!(f, " ,{}", b)?;
            }
        }
        write!(f, "]")
    }
}

impl Bytes {
    fn new() -> Self {
        Bytes { type_table: None, bytes: Vec::new() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    type_table : Option<Rc<RefCell<Table>>>,
    pub code : Rc<RefCell<core::Expression>>,
    pub args : Vec<Value>,
    pub vars : Vec<Value>
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<function:{:x}/{}/{}>", self.code.as_ptr().addr(), self.args.len(),self.vars.len())
    }
}