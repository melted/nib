use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap, HashSet},
    fmt::{Debug, Display},
    fs::read_to_string,
    hash::Hash,
    rc::Rc,
};

use crate::{
    common::{Error, Metadata, Name, Result},
    core::{Arity, FunClause, desugar, desugar_expression},
    parser::{parse_declarations, parse_expression},
    runtime::evaluate::Environment,
};

mod evaluate;
pub mod heap;
mod prims;
pub mod table;
mod tests;

#[derive(Debug, Clone)]
pub struct Runtime {
    metadata: HashMap<String, Metadata>,
    globals: Rc<RefCell<Table>>,
    named_symbols: HashMap<String, Symbol>,
    local_module: Option<Rc<RefCell<Table>>>,
    closures_to_check: HashMap<String, HashSet<String>>,
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Runtime {
    pub fn new() -> Self {
        let mut rt = Runtime {
            metadata: HashMap::new(),
            globals: new_ref(Table::new()),
            named_symbols: HashMap::new(),
            local_module: None,
            closures_to_check: HashMap::new(),
        };
        rt.register_primitives().unwrap();
        rt.register_type_tables();
        rt
    }

    pub fn load(&mut self, path: &str) -> Result<()> {
        let code = read_to_string(path)?;
        self.add_code(path, &code)
    }

    pub fn add_code(&mut self, name: &str, code: &str) -> Result<()> {
        let ast_module = parse_declarations(Some(name.to_owned()), code)?;
        let mut module = desugar(ast_module)?;
        let v = self
            .metadata
            .insert(name.to_owned(), module.metadata.clone());
        let mut env = Environment::new();
        self.evaluate(&mut module, &mut env)?;
        Ok(())
    }

    pub fn run_expression(&mut self, code: &str) -> Result<Value> {
        let ast_expr = parse_expression(code)?;
        let expr = desugar_expression(ast_expr)?;
        self.evaluate_expression("", &expr, &mut Environment::new())
    }

    pub fn error<T>(&self, msg: &str) -> Result<T> {
        Err(Error::Runtime {
            msg: msg.to_owned(),
            loc: None,
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

    pub fn get_from_table(&self, table: Rc<RefCell<Table>>, name: &str) -> Option<Value> {
        let sym = self.get_named_symbol(name)?;
        table.borrow().table.get(&sym).cloned()
    }

    pub fn get_or_add_named_symbol(&mut self, name: &str) -> Symbol {
        self.named_symbols
            .entry(name.to_owned())
            .or_insert_with(|| Symbol::named(name))
            .clone()
    }

    pub fn get_named_symbol(&self, name: &str) -> Option<Symbol> {
        self.named_symbols.get(name).cloned()
    }

    pub fn get_global(&self, name: &str) -> Option<Value> {
        let sym = self.named_symbols.get(name)?;
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

    pub fn get_name(&self, name: &Name) -> Option<Value> {
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

    pub fn get_module_path(&self, path: &[String]) -> Option<Rc<RefCell<Table>>> {
        let mut rest = path;
        let mut table = self.globals.clone();
        while !rest.is_empty() {
            let sym = self.get_named_symbol(&rest[0])?;
            table = {
                let t = &mut table.borrow_mut().table;
                let v = t.get(&sym);
                match v {
                    Some(Value::Table(n)) => n.clone(),
                    _ => {
                        return None;
                    }
                }
            };
            rest = &rest[1..];
        }
        Some(table)
    }

    pub fn get_or_create_module_path(&mut self, path: &[String]) -> Result<Rc<RefCell<Table>>> {
        let mut rest = path;
        let mut table = self.globals.clone();
        while !rest.is_empty() {
            let sym = self.get_or_add_named_symbol(&rest[0]);
            table = {
                let t = &mut table.borrow_mut().table;
                let v = t.get(&sym);
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
            };
            rest = &rest[1..];
        }
        Ok(table)
    }

    pub fn make_string(&self, s: &str) -> Result<Value> {
        let mut b = Bytes::with(s.as_bytes().to_vec());
        b.type_table = self.get_module_path(&["string".to_owned()]);
        Ok(Value::Bytes(new_ref(b)))
    }
}

fn new_ref<T>(val: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(val))
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Undefined,
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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Value {}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Integer(a), Value::Integer(b)) => a.cmp(b),
            (Value::Real(a), Value::Real(b)) => {
                a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Less)
            }
            (Value::Char(a), Value::Char(b)) => a.cmp(b),
            (Value::Pointer(a), Value::Pointer(b)) => a.cmp(b),
            (Value::Symbol(a), Value::Symbol(b)) => a.cmp(b),
            (Value::Bytes(a), Value::Bytes(b)) => a.as_ptr().cmp(&b.as_ptr()),
            (Value::Array(a), Value::Array(b)) => a.as_ptr().cmp(&b.as_ptr()),
            (Value::Table(a), Value::Table(b)) => a.as_ptr().cmp(&b.as_ptr()),
            (Value::Closure(a), Value::Closure(b)) => a.as_ptr().cmp(&b.as_ptr()),
            (x, y) => x.number().cmp(&y.number()),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "()"),
            Value::Undefined => write!(f, "<undefined>"),
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

    pub fn new_extern_mut_fun(
        fun: fn(&mut Runtime, &[Value]) -> Result<Value>,
        arity: &Arity,
    ) -> Self {
        Value::Closure(new_ref(Closure::extern_mut_fun(fun, arity)))
    }

    pub fn new_extern_fun(fun: fn(&Runtime, &[Value]) -> Result<Value>, arity: &Arity) -> Self {
        Value::Closure(new_ref(Closure::extern_fun(fun, arity)))
    }

    pub fn is_complex(&self) -> bool {
        match self {
            Value::Array(_) | Value::Table(_) | Value::Closure(_) => true,
            _ => false,
        }
    }

    pub fn number(&self) -> usize {
        match self {
            Value::Nil => 0,
            Value::Undefined => 1,
            Value::Bool(_) => 3,
            Value::Integer(_) => 4,
            Value::Real(_) => 5,
            Value::Char(_) => 6,
            Value::Pointer(_) => 7,
            Value::Symbol(symbol) => 8,
            Value::Bytes(ref_cell) => 9,
            Value::Array(ref_cell) => 10,
            Value::Table(ref_cell) => 11,
            Value::Closure(ref_cell) => 12,
        }
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

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.symbol_info.as_ptr().cmp(&other.symbol_info.as_ptr())
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

#[derive(Clone, PartialEq)]
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

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        done: &mut BTreeSet<Value>,
    ) -> std::fmt::Result {
        write!(f, "Table {{ ")?;
        for (k, v) in &self.table {
            write!(f, "{}: ", k)?;
            if done.contains(v) {
                write!(f, "<recurse>")?;
            } else if let Value::Table(inner) = v {
                done.insert(v.clone());
                inner.borrow().pretty_print(f, done)?;
            } else {
                write!(f, "{}, ", v)?;
            }
        }
        write!(f, "}}")
    }
}

impl Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(f, &mut BTreeSet::new())
    }
}

impl Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(f, &mut BTreeSet::new())
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
                write!(f, ", {}", v)?;
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
            array: vals.to_vec(),
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
                write!(f, ", {}", b)?;
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
            bytes,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Code {
    Nib(Vec<FunClause>),
    Extern(fn(&Runtime, &[Value]) -> Result<Value>),
    ExternMut(fn(&mut Runtime, &[Value]) -> Result<Value>),
    ExternSimple(fn(&[Value]) -> Result<Value>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    type_table: Option<Rc<RefCell<Table>>>,
    pub code: Rc<RefCell<Code>>,
    pub env: Environment,
    pub args: Vec<Value>,
    pub arity: Arity,
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#<function:{:x}:{}>",
            self.code.as_ptr().addr(),
            self.arity
        )
    }
}

impl Closure {
    pub fn with_args(&self, args: &[Value]) -> Self {
        Closure {
            type_table: self.type_table.clone(),
            code: self.code.clone(),
            env: self.env.clone(),
            args: args.to_vec(),
            arity: self.arity.clone(),
        }
    }

    pub fn extern_mut_fun(fun: fn(&mut Runtime, &[Value]) -> Result<Value>, arity: &Arity) -> Self {
        Closure {
            type_table: None,
            code: new_ref(Code::ExternMut(fun)),
            env: Environment::new(),
            args: Vec::new(),
            arity: arity.clone(),
        }
    }

    pub fn extern_fun(fun: fn(&Runtime, &[Value]) -> Result<Value>, arity: &Arity) -> Self {
        Closure {
            type_table: None,
            code: new_ref(Code::Extern(fun)),
            env: Environment::new(),
            args: Vec::new(),
            arity: arity.clone(),
        }
    }
}
