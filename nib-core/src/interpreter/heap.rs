use std::{collections::{HashMap, HashSet}, os::raw::c_void};

use region::Allocation;


pub struct Heap {
    from_space : Space,
    to_space : Option<Space>,
    roots : Vec<Value>
}

pub struct Space {
    pub alloc : Allocation,
    pub size : usize,
    pub top : usize,
}

impl Heap {
    pub fn new(size: usize) -> Self {
        Heap { from_space: Space::new(size), to_space: None, roots : Vec::new() }
    }

    pub fn allocate<T>(&mut self, size: usize) -> *mut T {
        unsafe {
            if self.from_space.top + size > self.from_space.size {
                self.collect(size);
            }
            let base_ptr = self.from_space.alloc.as_mut_ptr() as *mut T;
            let top_ptr = base_ptr.byte_add(self.from_space.top);
            self.from_space.top += size;
            top_ptr
        }
    }

    pub fn collect(&mut self, needed:usize) {
        let new_size =   if self.from_space.top > (3*self.from_space.size)/4 { 
                                    self.from_space.size + usize::min(self.from_space.size/2, 1000000) 
                                } else { 
                                    self.from_space.size
                                };
        let mut to_space = Space::new(new_size);
        unsafe {
            self.copy_live(&mut to_space);
        }
        self.from_space = to_space;
    }

    unsafe fn copy_live(&mut self, to_space:&mut Space) {
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
    pub(super) fn new(size:usize) -> Self {
        let Ok(alloc) = region::alloc(size, region::Protection::READ_WRITE_EXECUTE) else {
            panic!("Couldn't allocate {size} bytes!");
        };
        Space { alloc, size, top: 0 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ObjectHeader {
    size: u32,
    flags:u8,
    repr:ValueRepr,
    tag:u16
}

impl ObjectHeader {
    pub(super) fn make(heap: &mut Heap, size:u32, repr:ValueRepr) -> *mut Self {
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

pub(super) fn forward(from:*mut ObjectHeader, to:*mut ObjectHeader) {
    unsafe {
        (*from).flags &= FORWARD_FLAG;
        let next = from.add(1) as *mut *mut ObjectHeader;
        *next = to;
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Object
}

const FORWARD_FLAG:u8 = 0x01;

const TAG_MASK:u64 = 0x07;
const STAG_MASK:u64 = 0xf7;
const BOOL_MASK:u64 = 0xff;

const INT_TAG:u64 = 0x0;
const FLOAT_TAG:u64 = 0x1;
const PTR_TAG:u64 = 0x2;
const SYM_TAG:u64 = 0x3;
const ARR_TAG:u64 = 0x4;
const BOXINT_TAG:u64 = 0x5;
const SMALL_TAG:u64 = 0x6;
const OBJECT_TAG:u64 = 0x7;

const CHAR_STAG:u64 = 0x16;
const BOOL_STAG:u64 = 0x26;
const NIL_STAG:u64 = 0x36;
const UDEF_STAG:u64 = 0x46;

const FALSE_BTAG:u64 = 0x26;
const TRUE_BTAG:u64 = 0x2e;

impl Value {
    fn check_pointer<T>(ptr : *mut T) {
        if ptr.addr() as u64 & TAG_MASK != 0 {
            panic!("Pointer is not aligned, can't make value");
        }
    }

    pub fn integer(int:i64) -> Self {
        Value { val: (int << 3) as u64 }
    }

    pub fn pointer<T>(ptr : *mut T) -> Self {
        Value { val: (ptr.addr() as u64) << 3 | PTR_TAG }
    }

    pub fn float(flt : *mut f64) -> Self {
        Self::check_pointer(flt);
        Value { val: (flt.addr() as u64) | FLOAT_TAG }
    }

    pub fn symbol(sym : *mut ObjectHeader) -> Self {
        Self::check_pointer(sym);
        Value { val: (sym.addr() as u64) | SYM_TAG  }
    }

    pub fn array(arr : *mut ObjectHeader) -> Self {
        Self::check_pointer(arr);
        Value { val: (arr.addr() as u64) | ARR_TAG  }
    }

    pub fn bool(b : bool) -> Self {
        let val = if b { TRUE_BTAG } else { FALSE_BTAG };
        Value { val }
    }

    pub fn char(ch : char) -> Self {
        let codepoint = u64::from(ch) << 8;
        Value { val: codepoint | CHAR_STAG }
    }

    pub fn nil() -> Self {
        Value { val: NIL_STAG }
    }

    pub fn undefined() -> Self {
        Value { val: UDEF_STAG }
    }

    pub fn object(object:*mut ObjectHeader) -> Self {
        Self::check_pointer(object);
        Value { val: (object.addr() as u64) | OBJECT_TAG  }
    }

    pub fn get_immediate_repr(&self) -> ValueRepr {
        match self.val & TAG_MASK {
            INT_TAG => ValueRepr::Integer,
            FLOAT_TAG => ValueRepr::Float,
            PTR_TAG => ValueRepr::Pointer,
            SYM_TAG => ValueRepr::Symbol,
            ARR_TAG => ValueRepr::Array,
            BOXINT_TAG => ValueRepr::BoxedInteger,
            SMALL_TAG => {
                match self.val & 0xf7 {
                    CHAR_STAG => ValueRepr::Char,
                    BOOL_STAG => ValueRepr::Bool,
                    NIL_STAG => ValueRepr::Nil,
                    UDEF_STAG => ValueRepr::Undefined,
                    _ => ValueRepr::Undefined
                }
            }
            OBJECT_TAG => ValueRepr::Object,
            _ => ValueRepr::Undefined
        }
    }

    pub fn get_repr(&self) -> ValueRepr {
        match self.get_immediate_repr() {
            ValueRepr::Object => {
                let obj = self.get_object();
                unsafe { (*obj).repr }
            },
            repr => repr 
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

    pub fn get_float(&self) -> *mut f64 {
        let ptr = (self.val & !TAG_MASK) as usize;
        ptr as *mut f64
    }

    pub fn get_object(&self) -> *mut ObjectHeader {
        let ptr = (self.val & !TAG_MASK) as usize;
        ptr as *mut ObjectHeader
    }

    pub fn get_char(&self) -> char {
        unsafe {
            char::from_u32_unchecked((self.val >> 8) as u32)
        }
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


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Array {
    ptr : *mut ObjectHeader
}

impl Array {
    pub fn make(heap : &mut Heap, size:usize) -> Self {
        let header = ObjectHeader::make(heap, (size*8+16) as u32, ValueRepr::Array);
        let me = Array { ptr: header };
        me.set_type_table(Value::nil());
        for i in [0..size] {
            me.set(i, Value::nil());
        }
        me
    }

    pub fn at(&self, index:usize) -> Value {
        unsafe {
            let ptr = self.ptr.byte_add(8+index*8) as *mut Value;
            *ptr
        }
    }

    pub fn set(&self, index:usize, value:Value) {
        unsafe {
            let ptr = self.ptr.byte_add(8+index*8) as *mut Value;
            *ptr = value
        }
    }

    pub fn type_table(&self) -> Value {
        unsafe {
            let ptr = self.ptr.byte_add(8) as *mut Value;
            *ptr
        }
    }

    pub fn set_type_table(&self, value:Value) {
        unsafe {
            let ptr = self.ptr.byte_add(8) as *mut Value;
            *ptr = value; 
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Table {
    ptr: *mut ObjectHeader
}

impl Table {

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bytes {
    ptr: *mut ObjectHeader
}

impl Bytes {

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Symbol {
    ptr: *mut ObjectHeader
}

impl Symbol {
    
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Closure {
    ptr: *mut ObjectHeader
}

impl Closure {

}



