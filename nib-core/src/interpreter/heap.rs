
pub struct Heap {
    memory : Vec<u8>
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            memory: Vec::with_capacity(1000000)
        }
    }
}


pub struct ObjectHeader {
    size: u32,
    flags:u8,
    repr:ValueRepr,
    tag:u16
}

impl ObjectHeader {}


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
    Symbol,
    Array,
    Bytes,
    Table,
    Closure
}

const TAG_MASK:u64 = 0x07;
const ETAG_MASK:u64 = 0x0f;
const STAG_MASK:u64 = 0xf7;
const BOOL_MASK:u64 = 0xff;

const INT_TAG:u64 = 0x0;
const FLOAT_TAG:u64 = 0x1;
const PTR_TAG:u64 = 0x2;
const SYM_TAG:u64 = 0x3;
const ARR_TAG:u64 = 0x4;
const BYTES_TAG:u64 = 0x5;
const SMALL_TAG:u64 = 0x6;
const OBJECT_TAG:u64 = 0x7;
const TABLE_ETAG:u64 = 0x7;
const CLOSURE_ETAG:u64 = 0xf;

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

    fn check_pointer_etag<T>(ptr : *mut T) {
        if ptr.addr() as u64 & ETAG_MASK != 0 {
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

    pub fn bytes(ptr : *mut ObjectHeader) -> Self {
        Self::check_pointer(ptr);
        Value { val: (ptr.addr() as u64) | BYTES_TAG  }
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

    pub fn table(table:*mut ObjectHeader) -> Self {
        Self::check_pointer_etag(table);
        Value { val: (table.addr() as u64) | TABLE_ETAG  }
    }

    pub fn closure(closure:*mut ObjectHeader) -> Self {
        Self::check_pointer_etag(closure);
        Value { val: (closure.addr() as u64) | CLOSURE_ETAG  }
    }

    pub fn get_repr(&self) -> ValueRepr {
        match self.val & TAG_MASK {
            INT_TAG => ValueRepr::Integer,
            FLOAT_TAG => ValueRepr::Float,
            PTR_TAG => ValueRepr::Pointer,
            SYM_TAG => ValueRepr::Symbol,
            ARR_TAG => ValueRepr::Array,
            BYTES_TAG => ValueRepr::Bytes,
            SMALL_TAG => {
                match self.val & 0xf7 {
                    CHAR_STAG => ValueRepr::Char,
                    BOOL_STAG => ValueRepr::Bool,
                    NIL_STAG => ValueRepr::Nil,
                    UDEF_STAG => ValueRepr::Undefined,
                    _ => ValueRepr::Undefined
                }
            }
            OBJECT_TAG if self.val & ETAG_MASK == CLOSURE_ETAG => ValueRepr::Closure,
            OBJECT_TAG => ValueRepr::Table,
            _ => ValueRepr::Undefined
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
        (self.val & TAG_MASK) == BYTES_TAG
    }

    pub fn is_table(&self) -> bool {
        (self.val & ETAG_MASK) == TABLE_ETAG
    }

    pub fn is_closure(&self) -> bool {
        (self.val & ETAG_MASK) == CLOSURE_ETAG
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

    pub fn get_etag_object(&self) -> *mut ObjectHeader {
        let ptr = (self.val & !ETAG_MASK) as usize;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Array {
    ptr : *mut ObjectHeader
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Table {
    ptr: *mut ObjectHeader
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bytes {
    ptr: *mut ObjectHeader
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Symbol {
    ptr: *mut ObjectHeader
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Closure {
    ptr: *mut ObjectHeader
}



