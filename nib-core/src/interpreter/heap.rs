use core::slice;
use std::{
    collections::HashMap,
    ffi::c_void,
    fmt::Debug,
    hash::{DefaultHasher, Hasher},
    ptr::copy_nonoverlapping,
    slice::from_raw_parts,
};

use libffi::low::CodePtr;
use region::Allocation;

use crate::{
    core::{Arity, Expression, FunClause},
    treewalker::Signature,
};

pub struct Heap {
    from_space: Space,
    to_space: Option<Space>,
    roots: Vec<Value>,
}

pub struct Space {
    pub alloc: Allocation,
    pub size: usize,
    pub top: usize,
}

impl Heap {
    pub fn new(size: usize) -> Self {
        Heap {
            from_space: Space::new(size),
            to_space: None,
            roots: Vec::new(),
        }
    }

    pub fn allocate<T>(&mut self, size: usize) -> *mut T {
        unsafe {
            let unaligned = size % 8;
            let size = size + if unaligned > 0 { 8 - unaligned } else { 0 };
            if self.from_space.top + size > self.from_space.size {
                self.collect(size);
            }
            let base_ptr = self.from_space.alloc.as_mut_ptr() as *mut T;
            let top_ptr = base_ptr.byte_add(self.from_space.top);
            self.from_space.top += size;
            top_ptr
        }
    }

    pub fn collect(&mut self, needed: usize) {
        let new_size = if self.from_space.top > (3 * self.from_space.size) / 4 {
            self.from_space.size + usize::min(self.from_space.size / 2, 1000000)
        } else {
            self.from_space.size
        };
        let mut to_space = Space::new(new_size);
        unsafe {
            self.copy_live(&mut to_space);
        }
        self.from_space = to_space;
    }

    unsafe fn copy_live(&mut self, to_space: &mut Space) {
        let mut to_copy = self.roots.clone();
        let forwarded_headerless: HashMap<usize, usize> = HashMap::new();
        while let Some(v) = to_copy.pop() {
            match v.get_immediate_repr() {
                _ => {}
            }
        }
    }
}

impl Space {
    pub(super) fn new(size: usize) -> Self {
        let Ok(alloc) = region::alloc(size, region::Protection::READ_WRITE_EXECUTE) else {
            panic!("Couldn't allocate {size} bytes!");
        };
        Space {
            alloc,
            size,
            top: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ObjectHeader {
    size: u32,
    flags: u8,
    repr: ValueRepr,
    tag: u16,
}

impl ObjectHeader {
    pub(super) fn make(heap: &mut Heap, size: u32, repr: ValueRepr) -> *mut Self {
        let space: *mut Self = heap.allocate(size as usize);
        unsafe {
            (*space).size = size;
            (*space).flags = 0;
            (*space).repr = repr;
            (*space).tag = 0;
        }
        space
    }

    pub(super) fn is_forward(&self) -> bool {
        (self.flags & FORWARD_FLAG) == FORWARD_FLAG
    }
}

pub(super) fn forward(from: *mut ObjectHeader, to: *mut ObjectHeader) {
    unsafe {
        (*from).flags &= FORWARD_FLAG;
        let next = from.add(1) as *mut *mut ObjectHeader;
        *next = to;
    }
}

pub(super) fn get_value(base: *mut ObjectHeader, index: usize) -> Value {
    unsafe { *get_object_ptr(base, index) }
}

pub(super) fn set_value(base: *mut ObjectHeader, index: usize, value: Value) {
    unsafe {
        *get_object_ptr(base, index) = value;
    }
}

pub(super) fn get_object_ptr<T>(base: *mut ObjectHeader, index: usize) -> *mut T {
    unsafe {
        let base_ptr = base.add(1) as *mut T;
        let index_ptr = base_ptr.add(index);
        index_ptr
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value {
    pub val: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueRepr {
    Nil,
    Undefined,
    Bool,
    Integer,
    Pointer,
    Char,
    Float,
    BoxedInteger,
    Symbol,
    Array,
    Bytes,
    Table,
    Closure,
    Object,
}

const FORWARD_FLAG: u8 = 0x01;

const TAG_MASK: u64 = 0x07;
const STAG_MASK: u64 = 0xf7;
const BOOL_MASK: u64 = 0xff;

const INT_TAG: u64 = 0x0;
const FLOAT_TAG: u64 = 0x1;
const PTR_TAG: u64 = 0x2;
const SYM_TAG: u64 = 0x3;
const ARR_TAG: u64 = 0x4;
const BOXINT_TAG: u64 = 0x5;
const SMALL_TAG: u64 = 0x6;
const OBJECT_TAG: u64 = 0x7;

const CHAR_STAG: u64 = 0x16;
const BOOL_STAG: u64 = 0x26;
const NIL_STAG: u64 = 0x36;
const UDEF_STAG: u64 = 0x46;

const FALSE_BTAG: u64 = 0x26;
const TRUE_BTAG: u64 = 0x2e;

impl Value {
    pub fn hash(&self) -> usize {
        // All values except byte arrays and floats use
        // the hash of val, since arrays, tables and closures
        // better use object identity and symbols should by
        // definition.
        let mut hasher = DefaultHasher::new();
        match self.get_immediate_repr() {
            ValueRepr::Object if self.is_bytearray() => {
                let bytes = self.get_bytes();
                hasher.write(bytes.get_slice());
            }
            ValueRepr::Float => {
                let f = self.get_float();
                hasher.write_u64(f.to_bits());
            }
            _ => {
                hasher.write_u64(self.val);
            }
        }
        hasher.finish() as usize
    }

    fn check_pointer<T>(ptr: *mut T) {
        if ptr.addr() as u64 & TAG_MASK != 0 {
            panic!("Pointer is not aligned, can't make value");
        }
    }

    pub fn integer(int: i64) -> Self {
        Value {
            val: (int << 3) as u64,
        }
    }

    pub fn pointer<T>(ptr: *mut T) -> Self {
        Value {
            val: (ptr.addr() as u64) << 3 | PTR_TAG,
        }
    }

    pub fn cpointer<T>(ptr: *const T) -> Self {
        Value {
            val: (ptr.addr() as u64) << 3 | PTR_TAG,
        }
    }

    pub fn alloc_float(heap: &mut Heap, x: f64) -> Self {
        let object = ObjectHeader::make(heap, 16, ValueRepr::Float);
        let ptr = get_object_ptr(object, 0) as *mut f64;
        unsafe {
            *ptr = x;
        }
        Self::float(object)
    }

    pub fn float(flt: *mut ObjectHeader) -> Self {
        Self::check_pointer(flt);
        Value {
            val: (flt.addr() as u64) | FLOAT_TAG,
        }
    }

    pub fn symbol(sym: *mut ObjectHeader) -> Self {
        Self::check_pointer(sym);
        Value {
            val: (sym.addr() as u64) | SYM_TAG,
        }
    }

    pub fn array(arr: *mut ObjectHeader) -> Self {
        Self::check_pointer(arr);
        Value {
            val: (arr.addr() as u64) | ARR_TAG,
        }
    }

    pub fn bool(b: bool) -> Self {
        let val = if b { TRUE_BTAG } else { FALSE_BTAG };
        Value { val }
    }

    pub fn char(ch: char) -> Self {
        let codepoint = u64::from(ch) << 8;
        Value {
            val: codepoint | CHAR_STAG,
        }
    }

    pub fn nil() -> Self {
        Value { val: NIL_STAG }
    }

    pub fn undefined() -> Self {
        Value { val: UDEF_STAG }
    }

    pub fn object(object: *mut ObjectHeader) -> Self {
        Self::check_pointer(object);
        Value {
            val: (object.addr() as u64) | OBJECT_TAG,
        }
    }

    pub fn get_immediate_repr(&self) -> ValueRepr {
        match self.val & TAG_MASK {
            INT_TAG => ValueRepr::Integer,
            FLOAT_TAG => ValueRepr::Float,
            PTR_TAG => ValueRepr::Pointer,
            SYM_TAG => ValueRepr::Symbol,
            ARR_TAG => ValueRepr::Array,
            BOXINT_TAG => ValueRepr::BoxedInteger, // Reserved, not used ATM
            SMALL_TAG => match self.val & 0xf7 {
                CHAR_STAG => ValueRepr::Char,
                BOOL_STAG => ValueRepr::Bool,
                NIL_STAG => ValueRepr::Nil,
                UDEF_STAG => ValueRepr::Undefined,
                _ => ValueRepr::Undefined,
            },
            OBJECT_TAG => ValueRepr::Object,
            _ => ValueRepr::Undefined,
        }
    }

    pub fn get_repr(&self) -> ValueRepr {
        match self.get_immediate_repr() {
            ValueRepr::Object => {
                let obj = self.get_object();
                unsafe { (*obj).repr }
            }
            repr => repr,
        }
    }

    pub fn is_float(&self) -> bool {
        (self.val & TAG_MASK) == FLOAT_TAG
    }

    pub fn is_immediate_integer(&self) -> bool {
        (self.val & TAG_MASK) == INT_TAG
    }

    pub fn is_machine_pointer(&self) -> bool {
        (self.val & TAG_MASK) == PTR_TAG
    }

    pub fn is_symbol(&self) -> bool {
        (self.val & TAG_MASK) == SYM_TAG
    }

    pub fn is_array(&self) -> bool {
        (self.val & TAG_MASK) == ARR_TAG
    }

    pub fn is_bytearray(&self) -> bool {
        self.get_repr() == ValueRepr::Bytes
    }

    pub fn is_table(&self) -> bool {
        self.get_repr() == ValueRepr::Table
    }

    pub fn is_closure(&self) -> bool {
        self.get_repr() == ValueRepr::Closure
    }

    pub fn is_object(&self) -> bool {
        (self.val & TAG_MASK) == OBJECT_TAG
    }

    pub fn is_char(&self) -> bool {
        (self.val & STAG_MASK) == CHAR_STAG
    }

    pub fn is_bool(&self) -> bool {
        (self.val & STAG_MASK) == BOOL_STAG
    }

    pub fn is_true(&self) -> bool {
        (self.val & BOOL_MASK) == TRUE_BTAG
    }

    pub fn is_false(&self) -> bool {
        (self.val & BOOL_MASK) == FALSE_BTAG
    }

    pub fn is_nil(&self) -> bool {
        (self.val & STAG_MASK) == NIL_STAG
    }

    pub fn is_undefined(&self) -> bool {
        (self.val & STAG_MASK) == UDEF_STAG
    }

    pub fn get_integer(&self) -> i64 {
        (self.val as i64) >> 3
    }

    pub fn get_pointer<T>(&self) -> *mut T {
        // Keep the high bits the same.
        let high_byte = self.val & (0xff << 56);
        let ptr = (self.val >> 3) | high_byte;
        ptr as *mut T
    }

    pub fn get_cpointer<T>(&self) -> *const T {
        // Keep the high bits the same.
        let high_byte = self.val & (0xff << 56);
        let ptr = (self.val >> 3) | high_byte;
        ptr as *const T
    }

    pub fn get_float(&self) -> f64 {
        unsafe {
            let ptr = self.get_object().add(1) as *mut f64;
            *ptr
        }
    }

    pub fn get_object(&self) -> *mut ObjectHeader {
        let ptr = (self.val & !TAG_MASK) as usize;
        ptr as *mut ObjectHeader
    }

    pub fn get_array(&self) -> Array {
        let ptr = self.get_object();
        Array { ptr }
    }

    pub fn get_bytes(&self) -> Bytes {
        let ptr = self.get_object();
        Bytes { ptr }
    }

    pub fn get_table(&self) -> Table {
        let ptr = self.get_object();
        Table { ptr }
    }

    pub fn get_symbol(&self) -> Symbol {
        let ptr = self.get_object();
        Symbol { ptr }
    }

    pub fn get_closure(&self) -> Closure {
        let ptr = self.get_object();
        Closure { ptr }
    }

    pub fn get_char(&self) -> char {
        unsafe { char::from_u32_unchecked((self.val >> 8) as u32) }
    }

    pub fn get_bool(&self) -> bool {
        (self.val & TRUE_BTAG) > 0
    }
}

impl From<Table> for Value {
    fn from(value: Table) -> Self {
        Self::object(value.ptr)
    }
}

impl From<Array> for Value {
    fn from(value: Array) -> Self {
        Self::array(value.ptr)
    }
}

impl From<Closure> for Value {
    fn from(value: Closure) -> Self {
        Self::object(value.ptr)
    }
}

impl From<Bytes> for Value {
    fn from(value: Bytes) -> Self {
        Self::object(value.ptr)
    }
}

impl From<Symbol> for Value {
    fn from(value: Symbol) -> Self {
        Self::symbol(value.ptr)
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.get_repr() {
            ValueRepr::Nil => write!(f, "nil"),
            ValueRepr::Undefined => write!(f, "undefined"),
            ValueRepr::Bool => write!(f, "{:?}", self.get_bool()),
            ValueRepr::Integer => write!(f, "{:?}", self.get_integer()),
            ValueRepr::Pointer => write!(f, "{:?}", self.get_pointer::<*mut c_void>()),
            ValueRepr::Char => write!(f, "{:?}", self.get_char()),
            ValueRepr::Float => write!(f, "{:?}", self.get_float()),
            ValueRepr::BoxedInteger => todo!(),
            ValueRepr::Symbol => write!(f, "{:?}", self.get_symbol()),
            ValueRepr::Array => write!(f, "{:?}", self.get_array()),
            ValueRepr::Bytes => write!(f, "{:?}", self.get_bytes()),
            ValueRepr::Table => write!(f, "{:?}", self.get_table()),
            ValueRepr::Closure => write!(f, "{:?}", self.get_closure()),
            ValueRepr::Object => write!(f, "{:?}", self.get_object()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Array {
    ptr: *mut ObjectHeader,
}

impl Array {
    pub fn make(heap: &mut Heap, size: usize) -> Self {
        let header = ObjectHeader::make(heap, (size * 8 + 16) as u32, ValueRepr::Array);
        let me = Array { ptr: header };
        me.set_type_table(Value::nil());
        for i in 0..size {
            me.set(i, Value::nil());
        }
        me
    }

    pub fn at(&self, index: usize) -> Value {
        get_value(self.ptr, index + 1)
    }

    pub fn size(&self) -> usize {
        unsafe { (*self.ptr).size as usize / 8 - 2 }
    }

    pub fn set(&self, index: usize, value: Value) {
        set_value(self.ptr, index + 1, value);
    }

    pub fn values(&self) -> &[Value] {
        unsafe {
            let ptr = get_object_ptr(self.ptr, 1) as *const Value;
            slice::from_raw_parts(ptr, self.size())
        }
    }

    pub fn values_mut(&self) -> &[Value] {
        unsafe {
            let ptr = get_object_ptr(self.ptr, 1) as *mut Value;
            slice::from_raw_parts(ptr, self.size())
        }
    }

    pub fn fill(&mut self, values: &[Value], from: usize, to: usize) {
        for (i, v) in (from..to).zip(values) {
            self.set(i, *v);
        }
    }

    pub fn type_table(&self) -> Value {
        get_value(self.ptr, 0)
    }

    pub fn set_type_table(&self, value: Value) {
        set_value(self.ptr, 0, value);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Table {
    ptr: *mut ObjectHeader,
}

const INITIAL_SIZE: usize = 16;

impl Table {
    pub fn make(heap: &mut Heap) -> Self {
        let header = ObjectHeader::make(heap, 32, ValueRepr::Table);
        let mut me = Table { ptr: header };
        me.clear(heap);
        me
    }

    fn resize(&mut self, heap: &mut Heap) {
        let new_size = self.capacity() * 2;
        let storage = self.storage();
        let new_storage = Array::make(heap, new_size * 2);
        for i in 0..self.capacity() {
            let key = storage.at(i * 2);
            if Self::valid_key(key) {
                let value = storage.at(i * 2 + 1);
                Self::store(&new_storage, key, value);
            }
        }
        set_value(self.ptr, 2, Value::from(new_storage));
    }

    fn storage(&self) -> Array {
        get_value(self.ptr, 2).get_array()
    }

    fn valid_key(key: Value) -> bool {
        !(key.is_nil() || key.is_undefined())
    }

    pub fn insert(&mut self, heap: &mut Heap, key: Value, value: Value) {
        if !Self::valid_key(key) {
            return;
        }
        if 4 * self.size() > 3 * self.capacity() {
            self.resize(heap);
        }
        let storage = self.storage();
        let new_size = self.size() + 1;
        set_value(self.ptr, 1, Value::integer(new_size as i64));
        Self::store(&storage, key, value);
    }

    fn store(storage: &Array, key: Value, value: Value) -> usize {
        let hash_index = 2 * (key.hash() % (storage.size() / 2));
        let mut offset: usize = 0;
        let size = storage.size();
        while offset < size {
            let pos = (hash_index + offset) % size;
            let candidate = storage.at(pos);
            if !Self::valid_key(candidate) {
                storage.set(pos, Value::from(key));
                storage.set(pos + 1, value);
                return pos;
            }
            offset += 2;
        }
        panic!("Couldn't find space in table, this should be impossible");
    }

    fn find(&self, key: Value) -> Option<usize> {
        let hash_index = (key.hash() % self.capacity()) * 2;
        let mut offset: usize = 0;
        let storage = self.storage();
        let size = storage.size();
        while offset < size {
            let candidate = storage.at((hash_index + offset) % size);
            if Value::from(key) == candidate {
                return Some(hash_index);
            } else if candidate.is_nil() {
                return None;
            }
            offset += 2;
        }
        None
    }

    pub fn delete(&mut self, key: Value) {
        if let Some(index) = self.find(key) {
            self.storage().set(index, Value::undefined()); // set a tombstone
            self.storage().set(index + 1, Value::nil());
            let new_size = self.size() - 1;
            set_value(self.ptr, 1, Value::integer(new_size as i64));
        }
    }

    pub fn clear(&mut self, heap: &mut Heap) {
        let storage = Array::make(heap, INITIAL_SIZE);
        set_value(self.ptr, 1, Value::integer(0));
        set_value(self.ptr, 2, Value::from(storage));
    }

    pub fn get(&self, key: Value) -> Value {
        if let Some(index) = self.find(key) {
            self.storage().at(index + 1)
        } else {
            Value::bool(false)
        }
    }

    pub fn keys(&self, heap: &mut Heap) -> Value {
        let keys = Array::make(heap, self.size());
        let mut key_index = 0;
        let storage = self.storage();
        for i in 0..self.capacity() {
            let key = storage.at(i * 2);
            if key.is_symbol() {
                keys.set(key_index, key);
                key_index += 1;
            }
        }
        Value::from(keys)
    }

    pub fn size(&self) -> usize {
        get_value(self.ptr, 1).get_integer() as usize
    }

    pub fn capacity(&self) -> usize {
        let storage = self.storage();
        storage.size() / 2
    }

    pub fn type_table(&self) -> Value {
        get_value(self.ptr, 0)
    }

    pub fn set_type_table(&self, value: Value) {
        set_value(self.ptr, 0, value);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bytes {
    ptr: *mut ObjectHeader,
}

impl Bytes {
    pub fn make(heap: &mut Heap, size: usize, v: u8) -> Self {
        let header = ObjectHeader::make(heap, (size + 16) as u32, ValueRepr::Bytes);
        let me = Bytes { ptr: header };
        me.set_type_table(Value::nil());
        for i in 0..size {
            me.set(i, v);
        }
        me
    }

    pub fn with(heap: &mut Heap, bytes: &[u8]) -> Self {
        let header = ObjectHeader::make(heap, (bytes.len() + 16) as u32, ValueRepr::Bytes);
        let me = Bytes { ptr: header };
        let from = bytes.as_ptr();
        let to = get_object_ptr::<u8>(me.ptr, 8);
        me.set_type_table(Value::nil());
        unsafe {
            copy_nonoverlapping(from, to, bytes.len());
        }
        me
    }

    pub fn at(&self, index: usize) -> u8 {
        unsafe {
            let ptr = get_object_ptr(self.ptr, 8 + index);
            *ptr
        }
    }

    pub fn set(&self, index: usize, value: u8) {
        unsafe {
            let ptr = get_object_ptr(self.ptr, 8 + index);
            *ptr = value
        }
    }

    pub fn size(&self) -> usize {
        unsafe { ((*self.ptr).size - 16) as usize }
    }

    pub(super) fn get_slice(&self) -> &[u8] {
        unsafe {
            let ptr = self.ptr.byte_add(16) as *const u8;
            from_raw_parts(ptr, self.size())
        }
    }

    pub fn type_table(&self) -> Value {
        get_value(self.ptr, 0)
    }

    pub fn set_type_table(&self, value: Value) {
        set_value(self.ptr, 0, value);
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Symbol {
    ptr: *mut ObjectHeader,
}

impl Symbol {
    pub fn make(heap: &mut Heap, name: &str) -> Self {
        let header = ObjectHeader::make(heap, 24, ValueRepr::Symbol);
        let me = Symbol { ptr: header };
        me.set_type_table(Value::nil());
        let name_bytes = Bytes::with(heap, name.as_bytes());
        set_value(header, 1, Value::from(name_bytes));
        me
    }

    pub fn type_table(&self) -> Value {
        get_value(self.ptr, 0)
    }

    pub fn set_type_table(&self, value: Value) {
        set_value(self.ptr, 0, value);
    }

    pub fn name(&self) -> Value {
        get_value(self.ptr, 1)
    }

    pub fn as_string(&self) -> String {
        let name_bytes = self.name().get_bytes();
        let name_str = str::from_utf8(name_bytes.get_slice()).expect("symbols must be utf-8");
        name_str.to_owned()
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#({})", self.as_string())
    }
}

pub enum Code {
    Bytecode(Vec<u8>),
    Core(*const Vec<FunClause>),
    Extern(*const c_void),
    ExternMut(*const c_void),
    Foreign(*const Signature, CodePtr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Closure {
    ptr: *mut ObjectHeader,
}

pub const TYPE_INCOMPLETE: u16 = 0xffff;
pub const TYPE_BYTECODE: u16 = 0;
pub const TYPE_CORE: u16 = 1;
pub const TYPE_EXTERN: u16 = 2;
pub const TYPE_EXTERN_MUT: u16 = 3;
pub const TYPE_FOREIGN: u16 = 4;

impl Closure {
    fn make(
        heap: &mut Heap,
        code: &Code,
        captures: &[Value],
        arity: usize,
        vararg: Option<usize>,
    ) -> Self {
        let header = ObjectHeader::make(heap, 48, ValueRepr::Closure);
        let mut me = Closure { ptr: header };
        me.set_type_table(Value::nil());

        let env_size = captures.len() + arity + if vararg.is_some() { 1 } else { 0 };
        let mut env = Array::make(heap, env_size);
        env.fill(captures, 0, captures.len());
        me.set_code(heap, &code);
        set_value(header, 2, Value::from(env));
        set_value(header, 3, Value::integer(arity as i64));
        let var_pos = if let Some(pos) = vararg {
            Value::integer(pos as i64)
        } else {
            Value::bool(false)
        };
        set_value(header, 4, var_pos);
        me
    }

    pub fn set_tag(&mut self, tag: u16) {
        unsafe {
            (*self.ptr).tag = tag;
        }
    }

    pub fn get_tag(&self) -> u16 {
        unsafe { (*self.ptr).tag }
    }

    pub fn set_code(&mut self, heap: &mut Heap, code: &Code) {
        match code {
            Code::Bytecode(items) => {
                let bc = Bytes::with(heap, &items);
                self.set_tag(TYPE_BYTECODE);
                set_value(self.ptr, 1, Value::from(bc));
            }
            Code::Core(ptr) => {
                self.set_tag(TYPE_CORE);
                set_value(self.ptr, 1, Value::cpointer(ptr));
            }
            Code::Extern(ptr) => {
                self.set_tag(TYPE_EXTERN);
                set_value(self.ptr, 1, Value::cpointer(ptr));
            }
            Code::ExternMut(ptr) => {
                self.set_tag(TYPE_EXTERN_MUT);
                set_value(self.ptr, 1, Value::cpointer(ptr));
            }
            Code::Foreign(sig_ptr, code_ptr) => {
                self.set_tag(TYPE_EXTERN_MUT);
                let arr = Array::make(heap, 2);
                arr.set(0, Value::cpointer(sig_ptr));
                arr.set(1, Value::cpointer(code_ptr));
                set_value(self.ptr, 1, Value::from(arr));
            }
        }
    }

    pub fn get_code(&self) -> Code {
        let val = get_value(self.ptr, 1);
        match self.get_tag() {
            TYPE_BYTECODE => {
                let bytes = val.get_bytes();
                Code::Bytecode(bytes.get_slice().to_vec())
            }
            TYPE_CORE => Code::Core(val.get_cpointer()),
            TYPE_EXTERN => Code::Extern(val.get_cpointer()),
            TYPE_EXTERN_MUT => Code::ExternMut(val.get_cpointer()),
            TYPE_FOREIGN => {
                let arr = val.get_array();
                Code::Foreign(
                    arr.at(0).get_cpointer(),
                    CodePtr::from_ptr(arr.at(1).get_cpointer()),
                )
            }
            _ => panic!("Unexpected code type tag in get_code"),
        }
    }

    pub fn type_table(&self) -> Value {
        get_value(self.ptr, 0)
    }

    pub fn set_type_table(&mut self, value: Value) {
        set_value(self.ptr, 0, value);
    }
}
