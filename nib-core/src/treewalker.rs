#![allow(clippy::not_unsafe_ptr_arg_deref)]
#![allow(clippy::mutable_key_type)]

use crate::common::{Signature, Symbol};
use crate::core::Function;
use crate::runtime::Interpreter;
use crate::{
    common::{Error, Metadata, Name, Result},
    core::{Arity, desugar, desugar_expression},
    parser::{parse_declarations, parse_expression},
    treewalker::evaluate::Environment,
};
use libffi::middle::CodePtr;
use std::path::Path;
use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap, HashSet},
    ffi::c_void,
    fmt::{Debug, Display},
    fs::read_to_string,
    rc::Rc,
};

mod evaluate;
mod foreign;
mod prims;
mod tests;

pub struct Runtime {
    metadata: HashMap<String, Metadata>,
    globals: Rc<RefCell<Table>>,
    local_module: Option<Rc<RefCell<Table>>>,
    closures_to_check: HashMap<Symbol, Vec<Value>>,
    output_core: bool,
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter for Runtime {
    fn load(&mut self, path: &Path, reload: bool) -> Result<()> {
        let path_str = path.as_os_str().to_str().unwrap();
        log::info!("Loading {path_str}");
        let code = read_to_string(path)?;
        self.add_code(path_str, &code)
    }

    fn add_code(&mut self, name: &str, code: &str) -> Result<()> {
        let mut ast_module = crate::ast::Module::new(Some(name.to_owned()), code);
        parse_declarations(&mut ast_module)?;
        let mut module = desugar(ast_module)?;
        if self.output_core {
            for b in &module.bindings {
                println!("{}", b);
            }
        }
        let v = self
            .metadata
            .insert(name.to_owned(), module.metadata.clone());
        let mut env = Environment::new();
        self.evaluate(&mut module, &mut env)?;
        Ok(())
    }

    fn set_output_core(&mut self, output: bool) {
        self.output_core = output;
    }
}

impl Runtime {
    pub fn new() -> Self {
        let mut rt = Runtime {
            metadata: HashMap::new(),
            globals: new_ref(Table::new()),
            local_module: None,
            closures_to_check: HashMap::new(),
            output_core: false,
        };

        rt.register_type_tables();
        rt.register_primitives().unwrap();
        rt.register_system_constants().unwrap();
        rt.register_foreign_interface().unwrap();
        rt
    }

    pub fn run_expression(&mut self, code: &str) -> Result<Value> {
        let ast_expr = parse_expression(code)?;
        let expr = desugar_expression(ast_expr)?;
        self.evaluate_expression(expr)
    }

    pub fn error<T>(&self, msg: &str) -> Result<T> {
        Err(Error::Runtime {
            msg: msg.to_owned(),
            loc: None,
        })
    }

    pub fn add_global(&mut self, name: &str, value: Value) {
        self.add_global_symbol(&Symbol::from(name), value);
    }

    pub fn add_global_symbol(&mut self, sym: &Symbol, value: Value) {
        self.add_to_table(self.globals.clone(), sym, &value);
    }

    pub fn delete_global(&mut self, name: &Symbol) {
        self.globals.borrow_mut().table.remove(name);
    }

    pub fn add_to_table(&mut self, table: Rc<RefCell<Table>>, name: &Symbol, value: &Value) {
        table.borrow_mut().table.insert(*name, value.clone());
    }

    pub fn get_from_table(&self, table: Rc<RefCell<Table>>, name: &Symbol) -> Option<Value> {
        table.borrow().table.get(name).cloned()
    }

    pub fn get_global(&self, name: &Symbol) -> Option<Value> {
        self.globals.borrow().table.get(name).cloned()
    }

    pub fn add_name(&mut self, name: &Name, val: &Value) -> Result<()> {
        match name {
            Name::Qualified(path, name) => {
                let t = self.get_or_create_module_path(path, self.globals.clone())?;
                self.add_to_table(t, name, val);
            }
            Name::Plain(name) => {
                self.add_global(name.as_str(), val.clone());
            }
        }
        Ok(())
    }

    pub fn get_module_path(&self, path: &[Symbol]) -> Option<Rc<RefCell<Table>>> {
        let mut rest = path;
        let mut table = self.globals.clone();
        while !rest.is_empty() {
            let sym = &rest[0];
            table = {
                let t = &mut table.borrow_mut().table;
                let v = t.get(sym);
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

    pub fn get_or_create_module_path(
        &mut self,
        path: &[Symbol],
        start: Rc<RefCell<Table>>,
    ) -> Result<Rc<RefCell<Table>>> {
        let mut rest = path;
        let mut table = start;
        while !rest.is_empty() {
            let sym = &rest[0];
            table = {
                let t = &mut table.borrow_mut().table;
                let v = t.get(sym);
                match v {
                    Some(Value::Table(n)) => n.clone(),
                    None | Some(Value::Nil) => {
                        let nt = new_ref(Table::new());
                        t.insert(*sym, Value::Table(nt.clone()));
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
        b.type_table = self.get_module_path(&[Symbol::from("string")]);
        if b.type_table.is_none() {
            return self.error("trying to make string before string type table exists");
        }
        Ok(Value::Bytes(new_ref(b)))
    }
}

fn new_ref<T>(val: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(val))
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Undefined(Symbol),
    Bool(bool),
    Integer(i64),
    Real(f64),
    Char(char),
    Pointer(*mut c_void),
    Symbol(Symbol),
    Reference(Rc<RefCell<Array>>),
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
            Value::Undefined(symbol) => write!(f, "<undefined:{}>", symbol.as_str()),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Real(x) => write!(f, "{}", x),
            Value::Char(c) => write!(f, "{}", c),
            Value::Pointer(p) => write!(f, "ptr({:x})", p.addr()),
            Value::Symbol(symbol) => write!(f, "#<{}>", symbol),
            Value::Reference(ref_cell) => write!(f, "ref {}", &ref_cell.borrow()),
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

    pub fn new_foreign_fun(signature: &Signature, code: CodePtr) -> Self {
        Value::Closure(new_ref(Closure::foreign_fun(code, signature)))
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
            Value::Undefined(_) => 1,
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
            Value::Reference(_) => 13,
        }
    }

    pub fn get_type_table(&self) -> Option<Rc<RefCell<Table>>> {
        match self {
            Value::Bytes(b) => b.borrow().type_table.clone(),
            Value::Closure(c) => c.borrow().type_table.clone(),
            Value::Array(a) => a.borrow().type_table.clone(),
            Value::Table(t) => t.borrow().type_table.clone(),
            _ => None,
        }
    }

    pub fn get_table(&self) -> Result<Rc<RefCell<Table>>> {
        match self {
            Value::Table(t) => Ok(t.clone()),
            _ => Err(Error::runtime_error("Value not a table")),
        }
    }

    pub fn get_array(&self) -> Result<Rc<RefCell<Array>>> {
        match self {
            Value::Array(t) => Ok(t.clone()),
            _ => Err(Error::runtime_error("Value not an array")),
        }
    }

    pub fn get_bytes(&self) -> Result<Rc<RefCell<Bytes>>> {
        match self {
            Value::Bytes(t) => Ok(t.clone()),
            _ => Err(Error::runtime_error("Value not a bytes array")),
        }
    }

    pub fn get_closure(&self) -> Result<Rc<RefCell<Closure>>> {
        match self {
            Value::Closure(t) => Ok(t.clone()),
            _ => Err(Error::runtime_error("Value not a closure")),
        }
    }

    pub fn get_symbol(&self) -> Result<Symbol> {
        match self {
            Value::Symbol(t) => Ok(*t),
            _ => Err(Error::runtime_error("Value not a symbol")),
        }
    }

    pub fn get_pointer(&self) -> Result<&*mut c_void> {
        match self {
            Value::Pointer(t) => Ok(t),
            _ => Err(Error::runtime_error("Value not a pointer")),
        }
    }
}

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value::Integer(i64::from(value))
    }
}

impl From<u16> for Value {
    fn from(value: u16) -> Self {
        Value::Integer(i64::from(value))
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Integer(i64::from(value))
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Value::Integer(value as i64)
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Value::Integer(value as i64)
    }
}

impl From<i8> for Value {
    fn from(value: i8) -> Self {
        Value::Integer(i64::from(value))
    }
}

impl From<i16> for Value {
    fn from(value: i16) -> Self {
        Value::Integer(i64::from(value))
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Integer(i64::from(value))
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Real(value)
    }
}

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Value::Real(value as f64)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl<T> From<*mut T> for Value {
    fn from(value: *mut T) -> Self {
        Value::Pointer(value as *mut c_void)
    }
}

impl<T> From<*const T> for Value {
    fn from(value: *const T) -> Self {
        Value::Pointer(value as *mut c_void)
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Value::Char(value)
    }
}

impl From<&[u8]> for Value {
    fn from(value: &[u8]) -> Self {
        Value::new_bytes(value.to_vec())
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        // BUG: Can't set type to string without runtime
        Value::new_bytes(value.as_bytes().to_vec())
    }
}

impl TryFrom<&Value> for u8 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => {
                u8::try_from(*i).map_err(|_| Error::runtime_error("Value not an u8"))
            }
            _ => Err(Error::runtime_error("Value not an u8")),
        }
    }
}

impl TryFrom<&Value> for u16 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => {
                u16::try_from(*i).map_err(|_| Error::runtime_error("Value not an u16"))
            }
            _ => Err(Error::runtime_error("Value not an u16")),
        }
    }
}

impl TryFrom<&Value> for u32 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => {
                u32::try_from(*i).map_err(|_| Error::runtime_error("Value not an u32"))
            }
            _ => Err(Error::runtime_error("Value not an u32")),
        }
    }
}

impl TryFrom<&Value> for u64 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => Ok(*i as u64),
            _ => Err(Error::runtime_error("Value not an u64")),
        }
    }
}

impl TryFrom<&Value> for i8 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => {
                i8::try_from(*i).map_err(|_| Error::runtime_error("Value not an i8"))
            }
            _ => Err(Error::runtime_error("Value not an i8")),
        }
    }
}

impl TryFrom<&Value> for i16 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => {
                i16::try_from(*i).map_err(|_| Error::runtime_error("Value not an i16"))
            }
            _ => Err(Error::runtime_error("Value not an i16")),
        }
    }
}

impl TryFrom<&Value> for i32 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => {
                i32::try_from(*i).map_err(|_| Error::runtime_error("Value not an i32"))
            }
            _ => Err(Error::runtime_error("Value not an i32")),
        }
    }
}

impl TryFrom<&Value> for i64 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => Ok(*i),
            _ => Err(Error::runtime_error("Value not an u64")),
        }
    }
}

impl TryFrom<&Value> for usize {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => Ok(*i as usize),
            _ => Err(Error::runtime_error("Value not an usize")),
        }
    }
}

impl TryFrom<&Value> for f32 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Real(f) => Ok(*f as f32),
            _ => Err(Error::runtime_error("Value not a float")),
        }
    }
}

impl TryFrom<&Value> for f64 {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Real(f) => Ok(*f),
            _ => Err(Error::runtime_error("Value not a float")),
        }
    }
}

impl TryFrom<&Value> for bool {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Bool(i) => Ok(*i),
            _ => Err(Error::runtime_error("Value not a bool")),
        }
    }
}

impl TryFrom<&Value> for char {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Char(i) => Ok(*i),
            _ => Err(Error::runtime_error("Value not a char")),
        }
    }
}

impl<T> TryFrom<&Value> for *mut T {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Pointer(i) => Ok(*i as *mut T),
            _ => Err(Error::runtime_error("Value not a pointer")),
        }
    }
}

impl<T> TryFrom<&Value> for *const T {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Pointer(i) => Ok(*i as *const T),
            _ => Err(Error::runtime_error("Value not a pointer")),
        }
    }
}

impl TryFrom<&Value> for Vec<u8> {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Bytes(bytes) => {
                let b = bytes.borrow();
                Ok(b.bytes.clone())
            }
            _ => Err(Error::runtime_error("Value not a bytes array")),
        }
    }
}

impl TryFrom<&Value> for String {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        match value {
            Value::Bytes(bytes) => {
                let str = str::from_utf8(&bytes.borrow().bytes)
                    .map_err(|_| Error::runtime_error("Not a valid UTF-8 string"))?
                    .to_owned();
                Ok(str)
            }
            _ => Err(Error::runtime_error("Value not a bytes array")),
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

#[derive(Debug, Clone)]
pub enum Code {
    Nib(Box<Function>),
    Extern(fn(&Runtime, &[Value]) -> Result<Value>),
    ExternMut(fn(&mut Runtime, &[Value]) -> Result<Value>),
    ExternSimple(fn(&[Value]) -> Result<Value>),
    Foreign(Signature, CodePtr),
}

impl PartialEq for Code {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nib(l0), Self::Nib(r0)) => l0 == r0,
            _ => false,
        }
    }
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

    pub fn foreign_fun(ptr: CodePtr, sig: &Signature) -> Self {
        Closure {
            type_table: None,
            code: new_ref(Code::Foreign(sig.clone(), ptr)),
            env: Environment::new(),
            args: Vec::new(),
            arity: Arity::Fixed(sig.arg_types.len() as u32),
        }
    }
}
