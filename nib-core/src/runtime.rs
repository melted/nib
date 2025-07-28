use std::{cell::RefCell, collections::HashMap, fmt::Display, fs::read_to_string, hash::Hash, rc::Rc};

use crate::{
    common::{Error, Metadata, Name, Result}, core::{desugar, Arity, FunClause}, parser::parse_declarations, runtime::{evaluate::Environment, prims::Primitive}
};

mod evaluate;
pub mod heap;
mod prims;
pub mod table;
mod tests;

pub struct Runtime {
    metadata: HashMap<String, Metadata>,
    globals: Rc<RefCell<Table>>,
    named_symbols: HashMap<String, Symbol>,
    local_environment: Environment
}




impl Runtime {
    pub fn new() -> Self {
        let mut rt = Runtime {
            metadata: HashMap::new(),
            globals: new_ref(Table::new()),
            named_symbols: HashMap::new(),
            local_environment: Environment::new(),
        };
        rt.register_primitives();
        rt.register_type_tables();
        rt
    }

    pub fn load(&mut self, path:&str) -> Result<()> {
        let code = read_to_string(path)?;
        let mut ast_module = parse_declarations(Some(path.to_owned()), &code)?;
        let mut module = desugar(ast_module)?;
        let v = self.metadata.insert(path.to_owned(), module.metadata.clone());
        let mut env = Environment::new();
        self.evaluate(&mut module, &mut env)
    }

    pub fn add_code(&mut self, name:&str, code:&str) -> Result<()> {
        let mut ast_module = parse_declarations(Some(name.to_owned()), code)?;
        let mut module = desugar(ast_module)?;
        let v = self.metadata.insert(name.to_owned(), module.metadata.clone());
        let mut env = Environment::new();
        self.evaluate(&mut module, &mut env)
    }


    pub fn error<T>(&self, msg: &str) -> Result<T> {
        Err(Error::Runtime {
            msg: msg.to_owned(),
            loc: None
        })
    }

    pub fn add_global(&mut self, name: &str, value: Value) {
        self.add_to_table(self.globals.clone(), name, &value);
    }

    pub fn delete_global(&mut self, name: &str) {
        let sym = self.get_or_add_named_symbol(name);
        self.globals.borrow_mut().table.remove(&sym);
    }

    pub fn add_to_table(&mut self, table: Rc<RefCell<Table>>, name: &str, value: &Value) {
        let sym = self.get_or_add_named_symbol(name);
        table.borrow_mut().table.insert(sym, value.clone());
    }

    pub fn get_from_table(&mut self, table: Rc<RefCell<Table>>, name: &str) -> Option<Value> {
        let sym = self.get_or_add_named_symbol(name);
        table.borrow().table.get(&sym).cloned()
    }

    pub fn get_or_add_named_symbol(&mut self, name: &str) -> Symbol {
        self.named_symbols
            .entry(name.to_owned())
            .or_insert_with(|| Symbol::named(name))
            .clone()
    }

    pub fn get_global(&self, name: &str) -> Option<Value> {
        let Some(sym) = self.named_symbols.get(name) else {
            return None;
        };
        self.globals.borrow().table.get(sym).cloned()
    }

    pub fn add_name(&mut self, name: &Name, val: &Value) -> Result<()> {
        match name {
            Name::Qualified(path, name) => {
                let t = self.get_or_create_module_path(path)?;
                self.add_to_table(t, name, val);
            }
            Name::Plain(name) => {
                self.add_global(name, val.clone());
            }
        }
        Ok(())
    }

    pub fn get_name(&mut self, name: &Name) -> Option<Value> {
        match name {
            Name::Qualified(path, name) => {
                if let Some(t) = self.get_module_path(path) {
                    self.get_from_table(t, name)
                } else {
                    None
                }
            }
            Name::Plain(name) => self.get_global(name),
        }
    }

    pub fn get_module_path(&mut self, path: &[String]) -> Option<Rc<RefCell<Table>>> {
        let mut rest = path;
        let mut table = self.globals.clone();
        match path.get(0) {
            Some(s) if s == "global" => {
                rest = &rest[1..];
            }
            _ => {}
        };
        while !rest.is_empty() {
            let sym = self.get_or_add_named_symbol(&rest[0]);
            table = {
                let t = &mut table.borrow_mut().table;
                let v = t.get(&sym).clone();
                match v {
                    Some(Value::Table(n)) => n.clone(),
                    _ => {
                        return None;
                    }
                }
            }
        }
        Some(table)
    }

    pub fn get_or_create_module_path(&mut self, path: &[String]) -> Result<Rc<RefCell<Table>>> {
        let mut rest = path;
        let mut table = self.globals.clone();
        match path.get(0) {
            Some(s) if s == "global" => {
                rest = &rest[1..];
            }
            _ => {}
        };
        while !rest.is_empty() {
            let sym = self.get_or_add_named_symbol(&rest[0]);
            table = {
                let t = &mut table.borrow_mut().table;
                let v = t.get(&sym).clone();
                match v {
                    Some(Value::Table(n)) => n.clone(),
                    None | Some(Value::Nil) => {
                        let nt = new_ref(Table::new());
                        t.insert(sym, Value::Table(nt.clone()));
                        nt
                    }
                    _ => {
                        return self.error(&format!("Illegal module path {:?}", path));
                    }
                }
            }
        }
        Ok(table)
    }
}

fn new_ref<T>(val: T) -> Rc<RefCell<T>> {
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
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primitive(l0, l1), Self::Primitive(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
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
            Value::Primitive(primitive, arity) => {
                write!(f, "¤<primitive:{:?}:{}>", primitive, arity)
            }
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
        }
    }
}

impl Value {
    pub fn new_table() -> Self {
        Value::Table(new_ref(Table::new()))
    }

    pub fn new_bytes(bytes: Vec<u8>) -> Self {
        Value::Bytes(new_ref(Bytes::with(bytes)))
    }

    pub fn new_array(vals: &[Value]) -> Self {
        Value::Array(new_ref(Array::with(vals)))
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    symbol_info: Rc<RefCell<SymbolInfo>>,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = &self.symbol_info.borrow().symbol;
        if s.is_empty() {
            write!(
                f,
                "#anonymous_symbol({:x})",
                self.symbol_info.as_ptr().addr()
            )
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

impl Eq for Symbol {}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolInfo {
    type_table: Option<Rc<RefCell<Table>>>,
    symbol: String,
}

impl Symbol {
    pub fn named(s: &str) -> Self {
        let info = SymbolInfo {
            type_table: None,
            symbol: s.to_owned(),
        };
        Symbol {
            symbol_info: new_ref(info),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Table {
    type_table: Option<Rc<RefCell<Table>>>,
    table: HashMap<Symbol, Value>,
}

impl Table {
    fn new() -> Self {
        Table {
            type_table: None,
            table: HashMap::new(),
        }
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
    type_table: Option<Rc<RefCell<Table>>>,
    array: Vec<Value>,
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
        Array {
            type_table: None,
            array: Vec::new(),
        }
    }

    fn with(vals: &[Value]) -> Self {
        Array {
            type_table: None,
            array: vals.to_vec()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Bytes {
    type_table: Option<Rc<RefCell<Table>>>,
    bytes: Vec<u8>,
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
        Bytes {
            type_table: None,
            bytes: Vec::new(),
        }
    }

    fn with(bytes: Vec<u8>) -> Self {
        Bytes {
            type_table: None,
            bytes: bytes,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    type_table: Option<Rc<RefCell<Table>>>,
    pub code: Rc<RefCell<Vec<FunClause>>>,
    pub env: Environment,
    pub args: Vec<Value>,
    pub arity: Arity,
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#<function:{:x}{}>",
            self.code.as_ptr().addr(),
            self.arity
        )
    }
}
