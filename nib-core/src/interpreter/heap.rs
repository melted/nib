use core::slice;
use std::{
    array, collections::{HashMap, HashSet}, ffi::c_void, fmt::{Debug, Display, write}, hash::{DefaultHasher, Hash, Hasher}, mem, ptr::copy_nonoverlapping, slice::{from_raw_parts, from_raw_parts_mut}
};

use libffi::middle::Cif;
use region::Allocation;

use crate::{
    ast::Expression,
    common,
    common::{Symbol, align_int},
    interpreter::Runtime,
};

const BIG_OBJECT_THRESHOLD: usize = 0x3fff;

pub struct BigObject {
    count: usize,
    object: Allocation,
}

pub(super) struct Heap {
    from_space: Space,
    to_space: Space,
    big_objects: HashMap<usize, BigObject>,
}

pub(super) struct Space {
    pub alloc: Allocation,
    pub size: usize,
    pub top: usize,
}

impl Heap {
    pub(super) fn new(size: usize) -> Self {
        Heap {
            from_space: Space::new(size),
            to_space: Space::new(size),
            big_objects: HashMap::new(),
        }
    }
}

impl Runtime {
    pub fn allocate<T>(&mut self, size: usize) -> *mut T {
        unsafe {
            let aligned_size = align_int(size, 8);
            if aligned_size > BIG_OBJECT_THRESHOLD {
                return self.allocate_big_object(aligned_size);
            }
            if self.heap.from_space.top + aligned_size > self.heap.from_space.size {
                self.collect(aligned_size);
            }
            let base_ptr = self.heap.from_space.alloc.as_mut_ptr() as *mut T;
            let top_ptr = base_ptr.byte_add(self.heap.from_space.top);
            self.heap.from_space.top += aligned_size;
            top_ptr
        }
    }

    pub fn allocate_big_object<T>(&mut self, size: usize) -> *mut T {
        let Ok(mut alloc) = region::alloc(size, region::Protection::READ_WRITE) else {
            panic!("Couldn't allocate {size} bytes!");
        };
        let ptr = alloc.as_mut_ptr() as *mut T;
        self.heap.big_objects.insert(
            ptr.addr(),
            BigObject {
                count: 0,
                object: alloc,
            },
        );
        ptr
    }

    pub fn collect(&mut self, needed: usize) {
        let new_size = if self.heap.from_space.top > (3 * self.heap.from_space.size) / 4 {
            self.heap.from_space.size + usize::min(self.heap.from_space.size / 2, 1000000)
        } else {
            self.heap.from_space.size
        };
        if new_size > self.heap.to_space.size {
            self.heap.to_space = Space::new(new_size);
        }

        unsafe {
            self.copy_live();
        }
        std::mem::swap(&mut self.heap.to_space, &mut self.heap.from_space);
    }

    unsafe fn copy_live(&mut self) {
        let mut scan = 0;
        self.trace_roots();
        while scan < self.heap.to_space.top {
            let obj = self.heap.to_space.get_object_at(scan);
            scan += self.trace_object(obj);
        }
        let mut to_delete = vec![];
        for (k, v) in self.heap.big_objects.iter_mut() {
            if v.count == 0 {
                to_delete.push(*k);
            }
            v.count = 0;
        }
        for k in to_delete {
            self.heap.big_objects.remove(&k);
        }
    }

    fn trace_roots(&mut self) {
        let new_env = self.copy_object(self.global_env);
        self.global_env = new_env;
        let new_stack = self.copy_object(self.stack.to_value());
        self.stack.array = new_stack.get_array();
        let new_call_stack = self.copy_object(self.call_stack.to_value());
        self.call_stack.array = new_call_stack.get_array();
    }

    fn trace_object(&mut self, obj: *mut ObjectHeader) -> usize {
        unsafe {
            let repr = (*obj).repr;
            let size = (*obj).size as usize;
            match repr {
                ValueRepr::Array | ValueRepr::PartialApplication => {
                    let arr = Array { ptr: obj };
                    self.copy_object(arr.type_table());
                    for v in arr.values() {
                        self.copy_object(*v);
                    }
                }
                ValueRepr::Bytes => {
                    let bytes = Bytes { ptr: obj };
                    self.copy_object(bytes.type_table());
                }
                ValueRepr::Closure => {
                    let closure = Closure { ptr: obj };
                    self.copy_object(closure.type_table());
                    self.copy_object(get_value(obj, 1));
                    self.copy_object(closure.env());
                }
                ValueRepr::Table => {
                    let table = Table { ptr: obj };
                    self.copy_object(table.type_table());
                    self.copy_object(Value::from(table.storage()));
                }
                _ => {}
            }
            size
        }
    }

    fn copy_object(&mut self, value: Value) -> Value {
        if value.is_immediate() {
            return value;
        }

        unsafe {
            let obj = value.get_object();
            if (*obj).flags & BIG_OBJECT_FLAG == BIG_OBJECT_FLAG {
                if let Some(big_obj) = self.heap.big_objects.get_mut(&obj.addr()) {
                    big_obj.count += 1;
                }
                return value;
            }
            if (*obj).flags & FORWARD_FLAG == FORWARD_FLAG {
                return get_value(obj, 0);
            }
            let tag = value.get_tag();
            let size = (*obj).size as usize;
            let dst = self.heap.to_space.get_object_at(self.heap.to_space.top);
            copy_nonoverlapping(obj, dst, size);
            self.heap.to_space.top += align_int(size, 8);
            (*obj).flags &= FORWARD_FLAG;
            let new_value = Value::with_tag(dst, tag);
            set_value(obj, 0, new_value);
            new_value
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

    pub(super) fn get_object_at(&mut self, pos: usize) -> *mut ObjectHeader {
        unsafe { self.alloc.as_mut_ptr::<ObjectHeader>().byte_add(pos) }
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
    pub(super) fn make(rt: &mut Runtime, size: u32, repr: ValueRepr) -> *mut Self {
        let space: *mut Self = rt.allocate(size as usize);
        unsafe {
            (*space).size = size;
            (*space).flags = if size > BIG_OBJECT_THRESHOLD as u32 {
                BIG_OBJECT_FLAG
            } else {
                0
            };
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

const CELL_SIZE: usize = size_of::<Value>();

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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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
    PartialApplication,
    CallContinuation,
}

const FORWARD_FLAG: u8 = 0x01;
const BIG_OBJECT_FLAG: u8 = 0x02;

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
const CC_STAG: u64 = 0x56;

const FALSE_BTAG: u64 = 0x26;
const TRUE_BTAG: u64 = 0x2e;

impl Value {
    pub fn hash(&self) -> usize {
        let mut hasher = DefaultHasher::new();
        self.add_hash(&mut hasher);
        hasher.finish() as usize
    }

    fn add_hash<T: Hasher>(&self, hasher: &mut T) {
        match self.get_repr() {
            ValueRepr::Bytes => {
                let bytes = self.get_bytes();
                hasher.write(bytes.get_slice());
            }
            ValueRepr::Float => {
                let f = self.get_float();
                hasher.write_u64(f.to_bits());
            }
            ValueRepr::Array | ValueRepr::PartialApplication => {
                let arr = self.get_array();
                for v in arr.values() {
                    v.add_hash(hasher);
                }
            }
            ValueRepr::Closure => {
                let closure = self.get_closure();
                closure.get_code().hash(hasher);
                closure.env().add_hash(hasher);
            }
            ValueRepr::Table => {
                let table = self.get_table();
                let content = Value::from(table.storage());
                content.add_hash(hasher);
            }
            _ => {
                hasher.write_u64(self.val);
            }
        }
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

    pub fn alloc_float(rt: &mut Runtime, x: f64) -> Self {
        let object = ObjectHeader::make(rt, 16, ValueRepr::Float);
        let ptr = get_object_ptr(object, 0) as *mut f64;
        unsafe {
            *ptr = x;
        }
        Self::float(object)
    }

    pub fn float(flt: *mut ObjectHeader) -> Self {
        Self::with_tag(flt, FLOAT_TAG)
    }

    pub fn symbol(sym: &Symbol) -> Self {
        let v = common::symbol_id(sym) as u64;
        Value {
            val: v << 3 | SYM_TAG,
        }
    }

    pub fn array(arr: *mut ObjectHeader) -> Self {
        Self::with_tag(arr, ARR_TAG)
    }

    pub fn with_tag(obj: *mut ObjectHeader, tag: u64) -> Self {
        Self::check_pointer(obj);
        Value {
            val: (obj.addr() as u64) | tag,
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

    pub fn call_continuation(args: usize) -> Self {
        Value {
            val: (args as u64) << 8 | CC_STAG,
        }
    }

    pub fn partial_application(mut pap: Array) -> Self {
        pap.set_as_partial_application()
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
                CC_STAG => ValueRepr::CallContinuation,
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

    pub fn is_pointer(&self) -> bool {
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

    pub fn is_partial_application(&self) -> bool {
        self.get_repr() == ValueRepr::PartialApplication
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

    pub fn is_call_continuation(&self) -> bool {
        (self.val & STAG_MASK) == CC_STAG
    }

    pub fn is_immediate(&self) -> bool {
        let tag = self.val & TAG_MASK;
        tag == INT_TAG || tag == PTR_TAG || tag == SMALL_TAG
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
        let s = self.val >> 3;
        common::get_symbol(s as u32)
    }

    pub fn get_closure(&self) -> Closure {
        let ptr = self.get_object();
        Closure { ptr }
    }

    pub fn get_char(&self) -> char {
        unsafe { char::from_u32_unchecked((self.val >> 8) as u32) }
    }

    pub fn get_cc_args(&self) -> usize {
        (self.val >> 8) as usize
    }

    pub fn get_bool(&self) -> bool {
        (self.val & TRUE_BTAG) == TRUE_BTAG
    }

    pub fn get_tag(&self) -> u64 {
        self.val & TAG_MASK
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
        Self::symbol(&value)
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
            ValueRepr::PartialApplication => write!(f, "{:?}", self.get_array()),
            ValueRepr::CallContinuation => write!(f, "{:?}", self.get_cc_args()),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.get_repr() {
            ValueRepr::Nil => write!(f, "nil"),
            ValueRepr::Undefined => write!(f, "#<undefined>"),
            ValueRepr::Bool => write!(f, "{}", self.get_bool()),
            ValueRepr::Integer => write!(f, "{}", self.get_integer()),
            ValueRepr::Pointer => write!(f, "#<ptr:{:x}>", self.get_pointer::<*mut c_void>().addr()),
            ValueRepr::Char => write!(f, "{}", self.get_char()),
            ValueRepr::Float => write!(f, "{}", self.get_float()),
            ValueRepr::BoxedInteger => todo!(),
            ValueRepr::Symbol => write!(f, "#({})", self.get_symbol()),
            ValueRepr::Bytes => write!(f, "{}", self.get_bytes()),
            ValueRepr::Closure => write!(f, "{}", self.get_closure()),
            ValueRepr::PartialApplication => write!(f, "#<partial-application:{:x}>", self.val),
            ValueRepr::CallContinuation => write!(f, "#<call-continuation:{}>", self.get_cc_args()),
            ValueRepr::Object => write!(f, "#<object>"),
            _ => display_complex_object(&self, f, &mut HashSet::new())
        }
    }
}

fn display_complex_object(value: &Value, f: &mut std::fmt::Formatter<'_>, seen: &mut HashSet<Value>) -> std::fmt::Result {
    if seen.contains(value) {
        write!(f, "#<recurse:{:x}>", value.val)?;
        return Ok(());
    }
    seen.insert(value.clone());
    match value.get_repr() {
        ValueRepr::Array => {
            write!(f, "[")?;
            let array =  value.get_array();
            let mut iter = array.values().iter();
            if let Some(v) = iter.next() {
                display_complex_object(v, f, seen)?;
                for v in iter {
                    write!(f, ", ")?;
                    display_complex_object(v, f, seen)?;
                }
            }
            write!(f, "]")
        },
        ValueRepr::Table => {
            write!(f, "{{")?;
            let table = value.get_table();
            let pairs = table.pairs();
            let mut iter = pairs.iter();
            if let Some((k, v)) = iter.next() {
                display_complex_object(k, f, seen)?;
                write!(f, ": ")?;
                display_complex_object(v, f, seen)?;
                for (k, v) in iter {
                    write!(f, ", ")?;
                    display_complex_object(k, f, seen)?;
                    write!(f, ": ")?;
                    display_complex_object(v, f, seen)?;
                }
            }
            write!(f, "}}")
        }
        _ => write!(f, "{}", value)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Array {
    ptr: *mut ObjectHeader,
}

impl Debug for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Array[")?;
        let vals = self.values();
        let mut nil_count = 0;
        for v in vals {
            if v.is_nil() {
                nil_count += 1;
            } else {
                if nil_count > 0 {
                    write!(f, "... {} nil, ", nil_count)?;
                    nil_count = 0;
                }
                write!(f, "{:?}, ", v)?;
            }
        }
        if nil_count > 0 {
            write!(f, "... {} nil, ", nil_count)?;
            nil_count = 0;
        }
        write!(f, "]")
    }
}

impl Array {
    pub fn make(rt: &mut Runtime, size: usize) -> Self {
        let header = ObjectHeader::make(rt, ((size + 2) * CELL_SIZE) as u32, ValueRepr::Array);
        let me = Array { ptr: header };
        me.set_type_table(Value::nil());
        for i in 0..size {
            me.set(i, Value::nil());
        }
        me
    }

    pub fn with(rt: &mut Runtime, values: &[Value]) -> Self {
        let header = ObjectHeader::make(
            rt,
            ((values.len() + 2) * CELL_SIZE) as u32,
            ValueRepr::Array,
        );
        let me = Array { ptr: header };
        me.set_type_table(Value::nil());
        let src = values.as_ptr();
        let dst = get_object_ptr(header, 1) as *mut Value;
        unsafe {
            copy_nonoverlapping(src, dst, values.len());
        }
        me
    }

    pub fn at(&self, index: usize) -> Value {
        get_value(self.ptr, index + 1)
    }

    pub fn size(&self) -> usize {
        unsafe { (*self.ptr).size as usize / CELL_SIZE - 2 }
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

    pub fn values_mut(&self) -> &mut [Value] {
        unsafe {
            let ptr = get_object_ptr(self.ptr, 1) as *mut Value;
            slice::from_raw_parts_mut(ptr, self.size())
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

    pub fn set_as_partial_application(&mut self) -> Value {
        let mut header = unsafe { *(self.ptr) };
        header.repr = ValueRepr::PartialApplication;
        Value::with_tag(self.ptr, OBJECT_TAG)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Table {
    ptr: *mut ObjectHeader,
}

impl Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Table[ptr: {:?} keys: {}]", self.ptr, self.size())
    }
}


const INITIAL_SIZE: usize = 16;

impl Table {
    pub fn make(rt: &mut Runtime) -> Self {
        let header = ObjectHeader::make(rt, 32, ValueRepr::Table);
        let mut me = Table { ptr: header };
        me.clear(rt);
        me
    }

    fn resize(&mut self, rt: &mut Runtime) {
        let new_size = self.capacity() * 2;
        let storage = self.storage();
        let new_storage = Array::make(rt, new_size * 2);
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

    pub fn insert(&mut self, rt: &mut Runtime, key: Value, value: Value) {
        if !Self::valid_key(key) {
            return;
        }
        if 4 * self.size() > 3 * self.capacity() {
            self.resize(rt);
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
                storage.set(pos, key);
                storage.set(pos + 1, value);
                return pos;
            }
            offset += 2;
        }
        panic!("Couldn't find space in table, this should be impossible");
    }

    fn find(&self, key: Value) -> Option<usize> {
        let storage = self.storage();
        let hash_index = 2 * (key.hash() % (storage.size() / 2));
        let mut offset: usize = 0;
        let size = storage.size();
        while offset < size {
            let slot = (hash_index + offset) % size;
            let candidate = storage.at(slot);
            if Value::from(key) == candidate {
                return Some(slot);
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

    pub fn clear(&mut self, rt: &mut Runtime) {
        let storage = Array::make(rt, INITIAL_SIZE);
        set_value(self.ptr, 1, Value::integer(0));
        set_value(self.ptr, 2, Value::from(storage));
    }

    pub fn get(&self, key: Value) -> Value {
        if let Some(index) = self.find(key) {
            self.storage().at(index + 1)
        } else {
            Value::nil()
        }
    }

    pub fn keys(&self, rt: &mut Runtime) -> Value {
        let keys = Array::make(rt, self.size());
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

    pub fn pairs(&self) -> Vec<(Value, Value)> {
        let mut kv = Vec::new();
        let storage = self.storage();
        for i in 0..self.capacity() {
            let key = storage.at(i * 2);
            if Self::valid_key(key) {
                let value = storage.at(i*2+1);
                kv.push((key, value));
            }
        }
        kv
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Bytes {
    ptr: *mut ObjectHeader,
}

impl Debug for Bytes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Bytes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#[")?;
        let vals = self.get_slice();
        let mut iter = vals.iter();
        if let Some(b) = iter.next() {
            write!(f, "{}", b)?;
            for b in iter {
                write!(f, ", {}", b);
            }
        }
        write!(f, "]")
    }
}
impl Bytes {
    pub fn make(rt: &mut Runtime, size: usize, v: u8) -> Self {
        let header = ObjectHeader::make(rt, (size + 2 * CELL_SIZE) as u32, ValueRepr::Bytes);
        let me = Bytes { ptr: header };
        me.set_type_table(Value::nil());
        for i in 0..size {
            me.set(i, v);
        }
        me
    }

    pub fn with(rt: &mut Runtime, bytes: &[u8]) -> Self {
        let header = ObjectHeader::make(rt, (bytes.len() + 2 * CELL_SIZE) as u32, ValueRepr::Bytes);
        let me = Bytes { ptr: header };
        let from = bytes.as_ptr();
        let to = get_object_ptr::<u8>(me.ptr, CELL_SIZE);
        me.set_type_table(Value::nil());
        unsafe {
            copy_nonoverlapping(from, to, bytes.len());
        }
        me
    }

    pub fn at(&self, index: usize) -> u8 {
        unsafe {
            let ptr = get_object_ptr(self.ptr, CELL_SIZE + index);
            *ptr
        }
    }

    pub fn set(&self, index: usize, value: u8) {
        unsafe {
            let ptr = get_object_ptr(self.ptr, CELL_SIZE + index);
            *ptr = value
        }
    }

    pub fn size(&self) -> usize {
        unsafe { (*self.ptr).size as usize - 2 * CELL_SIZE }
    }

    pub(super) fn get_slice(&self) -> &[u8] {
        unsafe {
            let ptr = self.ptr.byte_add(2 * CELL_SIZE) as *const u8;
            from_raw_parts(ptr, self.size())
        }
    }

    pub(super) fn get_slice_mut(&self) -> &mut [u8] {
        unsafe {
            let ptr = self.ptr.byte_add(2 * CELL_SIZE) as *mut u8;
            from_raw_parts_mut(ptr, self.size())
        }
    }

    pub fn type_table(&self) -> Value {
        get_value(self.ptr, 0)
    }

    pub fn set_type_table(&self, value: Value) {
        set_value(self.ptr, 0, value);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Foreign {
    code: *const c_void,
    signature: *const Cif,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Code {
    Bytecode(Vec<u8>),
    Core(*const Vec<Expression>),
    Extern(*const c_void),
    Foreign(Foreign),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Closure {
    ptr: *mut ObjectHeader,
}

impl Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Closure[ptr: {:?}, code: {:?}, env: {:?}, arg: {:?}, vararg: {:?}]",
                 self.ptr, self.get_code(), self.env().get_array().size(), self.num_args(), self.vararg())
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<closure:{:x}>", self.ptr.addr())
    }
}

pub const TYPE_INCOMPLETE: u16 = 0xffff;
pub const TYPE_BYTECODE: u16 = 0;
pub const TYPE_CORE: u16 = 1;
pub const TYPE_EXTERN: u16 = 2;
pub const TYPE_FOREIGN: u16 = 3;

impl Closure {
    pub fn make(
        rt: &mut Runtime,
        code: &Code,
        captures: &[Value],
        arity: usize,
        vararg: Option<usize>,
    ) -> Self {
        let header = ObjectHeader::make(rt, 48, ValueRepr::Closure);
        let mut me = Closure { ptr: header };
        me.set_type_table(Value::nil());

        let env_size = captures.len() + arity + if vararg.is_some() { 1 } else { 0 };
        let mut env = Array::make(rt, env_size);
        env.fill(captures, 0, captures.len());
        me.set_code(rt, &code);
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

    pub fn make_low(
        rt: &mut Runtime,
        code: &Bytes,
        env: Value,
        arity: Value,
        vararg: Value,
    ) -> Self {
        let header = ObjectHeader::make(rt, 48, ValueRepr::Closure);
        let mut me = Closure { ptr: header };
        me.set_type_table(Value::nil());

        me.set_tag(TYPE_BYTECODE);
        set_value(header, 1, Value::from(code.clone()));
        set_value(header, 2, env);
        set_value(header, 3, arity);
        set_value(header, 4, vararg);
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

    pub fn set_code(&mut self, rt: &mut Runtime, code: &Code) {
        match code {
            Code::Bytecode(items) => {
                let bc = Bytes::with(rt, &items);
                self.set_tag(TYPE_BYTECODE);
                set_value(self.ptr, 1, Value::from(bc));
            }
            Code::Core(ptr) => {
                self.set_tag(TYPE_CORE);
                set_value(self.ptr, 1, Value::cpointer(*ptr));
            }
            Code::Extern(ptr) => {
                self.set_tag(TYPE_EXTERN);
                set_value(self.ptr, 1, Value::cpointer(*ptr));
            }
            Code::Foreign(foreign) => {
                self.set_tag(TYPE_FOREIGN);
                let array = Array::make(rt, 2);
                array.set(0, Value::cpointer(foreign.code));
                array.set(1, Value::cpointer(foreign.signature));
                set_value(self.ptr, 1, Value::from(array));
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
            _ => panic!("Unexpected code type tag in get_code"),
        }
    }

    pub fn code_value(&self) -> Value {
        get_value(self.ptr, 1)
    }

    pub fn type_table(&self) -> Value {
        get_value(self.ptr, 0)
    }

    pub fn set_type_table(&mut self, value: Value) {
        set_value(self.ptr, 0, value);
    }

    pub fn env(&self) -> Value {
        get_value(self.ptr, 2)
    }

    pub fn is_vararg(&self) -> bool {
        !get_value(self.ptr, 4).is_bool()
    }

    pub fn vararg(&self) -> Option<usize> {
        let val = get_value(self.ptr, 4);
        if val.is_immediate_integer() {
            Some(val.get_integer() as usize)
        } else {
            None
        }
    }

    pub fn num_args(&self) -> usize {
        get_value(self.ptr, 3).get_integer() as usize
    }
}
