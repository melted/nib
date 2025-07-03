

pub struct Environment {

}

pub struct ObjectHeader {
    header : u64
}

pub struct Value {
    val : u64
}

impl Value {
    pub fn is_reference(&self) -> bool {
        (self.val & 0x23) == 0x03
    }

    pub fn is_immediate_float(&self) -> bool {
        (self.val & 0x03) == 0x01
    }

    pub fn is_immediate_integer(&self) -> bool {
        (self.val & 0x03) == 0x00
    }

    pub fn is_machine_pointer(&self) -> bool {
        (self.val & 0x03) == 0x02
    }

    pub fn is_float(&self) -> bool {
        todo!()
    }

    pub fn is_integer(&self) -> bool {
        todo!()
    }

    pub fn is_falsish(&self) -> bool {
        (self.val & 0xff) == 0x7f
    }

}
