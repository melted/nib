
pub struct Heap {

}

pub struct ObjectHeader {
    header : u64
}

impl ObjectHeader {

}

pub struct Value {
    val : u64
}

impl Value {
    pub fn is_float(&self) -> bool {
        (self.val & 0x07) == 0x01
    }

    pub fn is_immediate_integer(&self) -> bool {
        (self.val & 0x07) == 0x00
    }

    pub fn is_machine_pointer(&self) -> bool {
        (self.val & 0x07) == 0x02
    }

    pub fn is_symbol(&self) -> bool {
        (self.val & 0x07) == 0x03
    }

    pub fn is_array(&self) -> bool {
        (self.val & 0x07) == 0x04
    }

    pub fn is_bytearray(&self) -> bool {
        (self.val & 0x07) == 0x05
    }

    pub fn is_reference(&self) -> bool {
        (self.val & 0x07) == 0x07
    }

    pub fn is_char(&self) -> bool {
        (self.val & 0xff) == 0x16
    }

    pub fn is_bool(&self) -> bool {
        (self.val & 0xf7) == 0x26
    }

    pub fn is_true(&self) -> bool {
        (self.val & 0xff) == 0x2e
    }

    pub fn is_false(&self) -> bool {
        (self.val & 0xff) == 0x26
    }

    pub fn is_nil(&self) -> bool {
        (self.val & 0xff) == 0x36
    }
}
