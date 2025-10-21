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
const INSTR_ADD:u8 = 33;
const INSTR_SUB:u8 = 34;
const INSTR_MUL:u8 = 35;
const INSTR_DIV:u8 = 36;
const INSTR_MOD:u8 = 37;
const INSTR_NEG:u8 = 38;

// Comparison
const INSTR_CMP:u8 = 39;
const INSTR_EQ:u8 = 40;
const INSTR_NEQ:u8 = 41;

// Bit Logic
const INSTR_BAND:u8 = 42;
const INSTR_BOR:u8 = 43;
const INSTR_BXOR:u8 = 44;
const INSTR_BNOT:u8 = 45;
const INSTR_BSHIFT:u8 = 46;

// Float Ops
const INSTR_SIN:u8 = 47;
const INSTR_COS:u8 = 48;
const INSTR_TAN:u8 = 49;
const INSTR_ASIN:u8 = 50;
const INSTR_ACOS:u8 = 51;
const INSTR_ATAN:u8 = 52;
const INSTR_CEILING:u8 = 53;
const INSTR_FLOOR:u8 = 54;
const INSTR_ROUND:u8 = 55;
const INSTR_LOG:u8 = 56;
const INSTR_EXP:u8 = 57;

// Conversions
const INSTR_TOINT:u8 = 58;
const INSTR_TOPTR:u8 = 59;

// Calls
const INSTR_CALL:u8 = 60;
const INSTR_CALL_TAIL:u8 = 61;
const INSTR_CALL_PRIM:u8 = 62;
const INSTR_CALL_FOREIGN:u8 = 63;

// Moves
const INSTR_MOVE:u8 = 64;
const INSTR_LOAD_IMM8:u8 = 65;
const INSTR_LOAD_IMM16:u8 = 66;
const INSTR_LOAD_IMM32:u8 = 67;
const INSTR_LOAD_IMM64:u8 = 68;
const INSTR_LOAD_BYTES_IMM:u8 = 69;
const INSTR_PUSH:u8 = 70;
const INSTR_POP:u8 = 71;
const INSTR_PUSH_RANGE:u8 = 72;
const INSTR_POP_RANGE:u8 = 73;
const INSTR_LOAD_MEM:u8 = 74;
const INSTR_LOAD_STACK:u8 = 75;
const INSTR_STORE_STACK:u8 = 76;

// Branches
const INSTR_JUMP:u8 = 77;
const INSTR_JUMP_IMM8:u8 = 78;
const INSTR_JUMP_IMM32:u8 = 79;
const INSTR_JZ:u8 = 80;
const INSTR_JZ_IMM8:u8 = 81;
const INSTR_JZ_IMM32:u8 = 82;
const INSTR_JPOS:u8 = 83;
const INSTR_JPOS_IMM8:u8 = 84;
const INSTR_JPOS_IMM32:u8 = 85;
const INSTR_JNEG:u8 = 86;
const INSTR_JNEG_IMM8:u8 = 87;
const INSTR_JNEG_IMM32:u8 = 88;
const INSTR_JNPOS:u8 = 89;
const INSTR_JNPOS_IMM8:u8 = 90;
const INSTR_JNPOS_IMM32:u8 = 91;
const INSTR_JNNEG:u8 = 92;
const INSTR_JNNEG_IMM8:u8 = 93;
const INSTR_JNNEG_IMM32:u8 = 94;
const INSTR_JFALSE:u8 = 95;
const INSTR_JFALSE_IMM8:u8 = 96;
const INSTR_JFALSE_IMM32:u8 = 97;
const INSTR_JNFALSE:u8 = 98;
const INSTR_JNFALSE_IMM8:u8 = 99;
const INSTR_JNFALSE_IMM32:u8 = 100;

// Type
const INSTR_TYPE:u8 = 101;
const INSTR_SET_TYPE:u8 = 102;

// Allocation
const INSTR_ALLOC_FLOAT:u8 = 103;
const INSTR_ALLOC_TABLE:u8 = 104;
const INSTR_ALLOC_ARRAY:u8 = 105;
const INSTR_ALLOC_BYTES:u8 = 106;
const INSTR_ALLOC_CLOSURE:u8 = 107;

// Arrays
const INSTR_ARRAY_REF:u8 = 108;
const INSTR_ARRAY_SET:u8 = 109;

// Tables
const INSTR_TABLE_GET:u8 = 110;
const INSTR_TABLE_SET:u8 = 111;
const INSTR_TABLE_DELETE:u8 = 112;

// Bytes
const INSTR_BYTES_REF:u8 = 113;
const INSTR_BYTES_SET:u8 = 114;

// Misc
const INSTR_EXIT:u8 = 115;
const INSTR_PANIC:u8 = 116;
const INSTR_INVALID:u8 = 117;
const INSTR_NOP:u8 = 0;
const INSTR_RETURN:u8 = 119;
