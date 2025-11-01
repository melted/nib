#![allow(unused_variables)]
#![allow(dead_code)]

//! The byte code and functions to manipulate it.

use std::collections::HashMap;
use std::mem;

pub struct BytecodeBuilder {
    pieces: Vec<Vec<u8>>,
    labels: HashMap<String, usize>,
    references: HashMap<String, Vec<usize>>,
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

    Cmp = 40,
    Eq = 41,

    BitAnd = 42,
    BitOr = 43,
    BitXor = 44,
    BitNot = 45,

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

    TailCall = 88,
    Call = 89,
    Type = 90,
    SetType = 91,
    AllocTable = 95,
    AllocBytes = 96,
    AllocArray = 97,
    AllocSymbol = 98,
    AllocClosure = 99,
    Branch = 100,
    BranchZero = 101,
    BranchPositive = 102,
    BranchNegative = 103,
    BranchNotNegative = 104,
    BranchNotPositive = 105,
    BranchFalse = 106,
    BranchNotFalse = 107,
    Exit = 127,
    Invalid = 255,
}

// Arithmetic
pub const INSTR_ADD: u8 = 33;
pub const INSTR_SUB: u8 = 34;
pub const INSTR_MUL: u8 = 35;
pub const INSTR_DIV: u8 = 36;
pub const INSTR_MOD: u8 = 37;
pub const INSTR_NEG: u8 = 38;

// Comparison
pub const INSTR_CMP: u8 = 39;
pub const INSTR_EQ: u8 = 40;
pub const INSTR_NEQ: u8 = 41;

// Bit Logic
pub const INSTR_BAND: u8 = 42;
pub const INSTR_BOR: u8 = 43;
pub const INSTR_BXOR: u8 = 44;
pub const INSTR_BNOT: u8 = 45;
pub const INSTR_BSHIFT: u8 = 46;

// Float Ops
pub const INSTR_SIN: u8 = 47;
pub const INSTR_COS: u8 = 48;
pub const INSTR_TAN: u8 = 49;
pub const INSTR_ASIN: u8 = 50;
pub const INSTR_ACOS: u8 = 51;
pub const INSTR_ATAN: u8 = 52;
pub const INSTR_CEILING: u8 = 53;
pub const INSTR_FLOOR: u8 = 54;
pub const INSTR_ROUND: u8 = 55;
pub const INSTR_LOG: u8 = 56;
pub const INSTR_EXP: u8 = 57;

// Conversions
pub const INSTR_TOINT: u8 = 58;
pub const INSTR_TOPTR: u8 = 59;

// Calls
pub const INSTR_CALL: u8 = 60;
pub const INSTR_CALL_TAIL: u8 = 61;
// Moves
pub const INSTR_MOVE: u8 = 64;
pub const INSTR_LOAD_IMM8: u8 = 65;
pub const INSTR_LOAD_IMM16: u8 = 66;
pub const INSTR_LOAD_IMM32: u8 = 67;
pub const INSTR_LOAD_IMM64: u8 = 68;
pub const INSTR_LOAD_BYTES_IMM: u8 = 69;
pub const INSTR_PUSH: u8 = 70;
pub const INSTR_POP: u8 = 71;
pub const INSTR_PUSH_RANGE: u8 = 72;
pub const INSTR_POP_RANGE: u8 = 73;
pub const INSTR_LOAD_MEM: u8 = 74;
pub const INSTR_LOAD_STACK: u8 = 75;
pub const INSTR_STORE_STACK: u8 = 76;

// Branches
pub const INSTR_JUMP: u8 = 77;
pub const INSTR_JUMP_IMM8: u8 = 78;
pub const INSTR_JZ: u8 = 80;
pub const INSTR_JZ_IMM8: u8 = 81;
pub const INSTR_JPOS: u8 = 83;
pub const INSTR_JPOS_IMM8: u8 = 84;
pub const INSTR_JNEG: u8 = 86;
pub const INSTR_JNEG_IMM8: u8 = 87;
pub const INSTR_JNPOS: u8 = 89;
pub const INSTR_JNPOS_IMM8: u8 = 90;
pub const INSTR_JNNEG: u8 = 92;
pub const INSTR_JNNEG_IMM8: u8 = 93;
pub const INSTR_JFALSE: u8 = 95;
pub const INSTR_JFALSE_IMM8: u8 = 96;
pub const INSTR_JNFALSE: u8 = 98;
pub const INSTR_JNFALSE_IMM8: u8 = 99;

// Type
pub const INSTR_TYPE: u8 = 101;
pub const INSTR_SET_TYPE: u8 = 102;

// Allocation
pub const INSTR_ALLOC_FLOAT: u8 = 103;
pub const INSTR_ALLOC_TABLE: u8 = 104;
pub const INSTR_ALLOC_ARRAY: u8 = 105;
pub const INSTR_ALLOC_BYTES: u8 = 106;
pub const INSTR_ALLOC_CLOSURE: u8 = 107;

// Arrays
pub const INSTR_ARRAY_REF: u8 = 108;
pub const INSTR_ARRAY_SET: u8 = 109;
pub const INSTR_ARRAY_SIZE: u8 = 120;

// Tables
pub const INSTR_TABLE_GET: u8 = 110;
pub const INSTR_TABLE_SET: u8 = 111;
pub const INSTR_TABLE_DELETE: u8 = 112;
pub const INSTR_TABLE_SIZE: u8 = 121;

// Bytes
pub const INSTR_BYTES_REF: u8 = 113;
pub const INSTR_BYTES_SET: u8 = 114;
pub const INSTR_BYTES_SIZE: u8 = 122;

// Misc
pub const INSTR_EXIT: u8 = 115;
pub const INSTR_PANIC: u8 = 116;
pub const INSTR_INVALID: u8 = 117;
pub const INSTR_HALT: u8 = 118;
pub const INSTR_NOP: u8 = 0;
pub const INSTR_RETURN: u8 = 119;
pub const INSTR_APPLY: u8 = 123;
pub const INSTR_APPLY_TAIL: u8 = 124;

