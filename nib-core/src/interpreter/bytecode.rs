#![allow(unused_variables)]
#![allow(dead_code)]

//! The byte code and functions to manipulate it.

use std::collections::HashMap;
use std::mem;

pub struct BytecodeBuilder {
    pieces: Vec<Vec<u8>>,
    labels: HashMap<String, usize>,
    references: HashMap<String, Vec<usize>>
}

impl BytecodeBuilder {
    pub fn new() -> Self {
        BytecodeBuilder {
            pieces: Vec::new(),
            labels: HashMap::new(),
            references: HashMap::new(),
         }
    }

    pub fn build(self) -> Vec<u8> {
        self.pieces.into_iter().flatten().collect::<Vec<u8>>()
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Instruction {
    Add = 33,
    Sub = 34,
    Mul = 35,
    Div = 36,
    Mod = 37,

    TailCall = 38,
    Call = 39,
    Type = 40,
    SetType = 41,
    AllocTable = 45,
    AllocBytes = 46,
    AllocArray = 47,
    AllocSymbol = 48,
    AllocClosure = 49,

    CallExternal = 50,

    Project = 60,
    TableSet = 61,
    TableDelete = 62,

    ArrayRef = 70,
    ArraySet = 71,

    Load = 80,
    Store = 81,
    LoadImm8 = 82,
    LoadImm16 = 83,
    LoadImm32 = 84,
    LoadImm64 = 85,
    BytesImm = 86,
    Branch = 100,
    BranchZero = 101,
    BranchPositive = 102,
    BranchNegative = 103,
}
