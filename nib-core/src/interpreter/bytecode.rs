//! The byte code and functions to manipulate it.

use std::{collections::HashMap, fmt::Display, mem, usize};

pub struct BytecodeBuilder {
    pieces: Vec<Vec<u8>>,
    labels: HashMap<String, usize>,
    references: HashMap<String, Vec<usize>>,
}

impl Default for BytecodeBuilder {
    fn default() -> Self {
        Self::new()
    }
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
    Push0 = 0,
    Push1 = 1,
    Push2 = 2,
    Push3 = 3,
    Push4 = 4,
    Push5 = 5,
    Push6 = 6,
    Push7 = 7,
    Push8 = 8,
    Push9 = 9,
    Push10 = 10,
    Push11 = 11,
    Push12 = 12,
    Push13 = 13,
    Push14 = 14,
    Push15 = 15,
    Push16 = 16,
    Push17 = 17,
    Push18 = 18,
    Push19 = 19,
    PushLastSmall = 20,
    Nop = 22,

    Gt = 29,
    GtE = 30,
    Lt = 31,
    LtE = 32,

    Add = 33,
    Sub = 34,
    Mul = 35,
    Div = 36,
    Mod = 37,
    Neg = 38,

    Cmp = 39,
    Eq = 40,
    NEq = 41,

    BitAnd = 42,
    BitOr = 43,
    BitXor = 44,
    BitShift = 45,
    BitNot = 46,

    Sin = 47,
    Cos = 48,
    Tan = 49,
    ASin = 50,
    ACos = 51,
    ATan = 52,
    Ceiling = 53,
    Floor = 54,
    Round = 55,
    Log = 56,
    Exp = 57,

    ToInt = 58,
    ToPtr = 59,

    Call = 60,
    TailCall = 61,

    Dup = 62,
    Swap = 63,
    Drop = 64,

    StackStore = 67,
    StackLoad = 68,

    Load8 = 73,
    Load16 = 74,
    Load32 = 75,
    Load64 = 76,
    LoadBytes = 77,
    LoadBytes8 = 78,

    Rot = 79,

    Jump = 80,
    JumpZ = 81,
    JumpPos = 82,
    JumpNeg = 83,
    JumpNPos = 84,
    JumpNNeg = 85,
    JumpFalse = 86,
    JumpNFalse = 87,

    JumpImm = 88,
    JumpZImm = 89,
    JumpPosImm = 90,
    JumpNegImm = 91,
    JumpNPosImm = 92,
    JumpNNegImm = 93,
    JumpFalseImm = 94,
    JumpNFalseImm = 95,

    MakeSymbol = 100,

    Type = 101,
    SetType = 102,

    AllocFloat = 103,
    AllocTable = 104,
    AllocBytes = 105,
    AllocArray = 106,
    AllocClosure = 107,

    ArrayRef = 108,
    ArraySet = 109,
    ArraySize = 110,

    TableGet = 111,
    TableSet = 112,
    TableDelete = 113,
    TableSize = 114,

    ByteGet = 115,
    ByteSet = 116,
    ByteSize = 117,

    LocalGet = 118,
    LocalSet = 119,

    GlobalEnv = 120,
    Invalid = 121,
    Halt = 122,
    Return = 123,

    IsInt = 126,
    IsChar = 127,
    IsNil = 128,
    IsFloat = 129,
    IsPtr = 130,
    IsBool = 131,
    IsSymbol = 132,
    IsArray = 133,
    IsBytes = 134,
    IsTable = 135,
    IsClosure = 136,
    IsPap = 137,
    IsCC = 138,
    IsObject = 139,
    IsImmediate = 140,

    GetArg = 141,
    StackFrame = 142,
    StackArray = 143,
    ArgCount = 144,

    PushMinusOne = 152,
    PushNil = 153,
    PushFalse = 154,
    PushTrue = 155,
}

impl Instruction {
    pub fn trailing_bytes(&self) -> usize {
        match *self as u8 {
            INSTR_LOAD_IMM8 => 1,
            INSTR_LOAD_IMM16 => 2,
            INSTR_LOAD_IMM32 => 4,
            INSTR_LOAD_IMM64 => 8,
            INSTR_LOAD_BYTES8 => 8,
            INSTR_LOAD_BYTES_IMM => usize::MAX,
            INSTR_JUMP_IMM8..=INSTR_JNFALSE_IMM8 => 1,
            _ => 0,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Instruction::Push0 => "push 0",
            Instruction::Push1 => "push 1",
            Instruction::Push2 => "push 2",
            Instruction::Push3 => "push 3",
            Instruction::Push4 => "push 4",
            Instruction::Push5 => "push 5",
            Instruction::Push6 => "push 6",
            Instruction::Push7 => "push 7",
            Instruction::Push8 => "push 8",
            Instruction::Push9 => "push 9",
            Instruction::Push10 => "push 10",
            Instruction::Push11 => "push 11",
            Instruction::Push12 => "push 12",
            Instruction::Push13 => "push 13",
            Instruction::Push14 => "push 14",
            Instruction::Push15 => "push 15",
            Instruction::Push16 => "push 16",
            Instruction::Push17 => "push 17",
            Instruction::Push18 => "push 18",
            Instruction::Push19 => "push 19",
            Instruction::PushLastSmall => "push 20",
            Instruction::Nop => "nop",
            Instruction::Gt => "gt",
            Instruction::GtE => "gte",
            Instruction::Lt => "lt",
            Instruction::LtE => "lte",
            Instruction::Add => "add",
            Instruction::Sub => "sub",
            Instruction::Mul => "mul",
            Instruction::Div => "div",
            Instruction::Mod => "mod",
            Instruction::Neg => "neg",
            Instruction::Cmp => "cmp",
            Instruction::Eq => "eq",
            Instruction::NEq => "neq",
            Instruction::BitAnd => "band",
            Instruction::BitOr => "bor",
            Instruction::BitXor => "bxor",
            Instruction::BitShift => "shift",
            Instruction::BitNot => "bnot",
            Instruction::Sin => "sin",
            Instruction::Cos => "cos",
            Instruction::Tan => "tan",
            Instruction::ASin => "asin",
            Instruction::ACos => "acos",
            Instruction::ATan => "atan",
            Instruction::Ceiling => "ceiling",
            Instruction::Floor => "floor",
            Instruction::Round => "round",
            Instruction::Log => "log",
            Instruction::Exp => "exp",
            Instruction::ToInt => "toint",
            Instruction::ToPtr => "toptr",
            Instruction::Call => "call",
            Instruction::TailCall => "tailcall",
            Instruction::Dup => "dup",
            Instruction::Swap => "swap",
            Instruction::Drop => "drop",
            Instruction::StackStore => "store",
            Instruction::StackLoad => "load",
            Instruction::Load8 => "load8",
            Instruction::Load16 => "load16",
            Instruction::Load32 => "load32",
            Instruction::Load64 => "load64",
            Instruction::LoadBytes => "loadb",
            Instruction::LoadBytes8 => "loadb8",
            Instruction::Rot => "rot",
            Instruction::Jump => "jmp",
            Instruction::JumpZ => "jz",
            Instruction::JumpPos => "jpos",
            Instruction::JumpNeg => "jneg",
            Instruction::JumpNPos => "jnpos",
            Instruction::JumpNNeg => "jnneg",
            Instruction::JumpFalse => "jfalse",
            Instruction::JumpNFalse => "jnfalse",
            Instruction::JumpImm => "jmpi",
            Instruction::JumpZImm => "jzi",
            Instruction::JumpPosImm => "jposi",
            Instruction::JumpNegImm => "jnegi",
            Instruction::JumpNPosImm => "jnposi",
            Instruction::JumpNNegImm => "jnposi",
            Instruction::JumpFalseImm => "jfalsei",
            Instruction::JumpNFalseImm => "jnfalsei",
            Instruction::MakeSymbol => "sym",
            Instruction::Type => "type",
            Instruction::SetType => "set_type",
            Instruction::AllocFloat => "alloc_float",
            Instruction::AllocTable => "alloc_table",
            Instruction::AllocBytes => "alloc_bytes",
            Instruction::AllocArray => "alloc_array",
            Instruction::AllocClosure => "alloc_closure",
            Instruction::ArrayRef => "array_ref",
            Instruction::ArraySet => "array_set",
            Instruction::ArraySize => "array_size",
            Instruction::TableGet => "table_get",
            Instruction::TableSet => "table_set",
            Instruction::TableDelete => "table_delete",
            Instruction::TableSize => "table_size",
            Instruction::ByteGet => "bytes_ref",
            Instruction::ByteSet => "bytes_set",
            Instruction::ByteSize => "bytes_size",
            Instruction::LocalGet => "local_get",
            Instruction::LocalSet => "local_set",
            Instruction::GlobalEnv => "global_env",
            Instruction::Invalid => "<invalid>",
            Instruction::Halt => "halt",
            Instruction::Return => "return",
            Instruction::IsInt => "int?",
            Instruction::IsChar => "char?",
            Instruction::IsNil => "nil?",
            Instruction::IsFloat => "float?",
            Instruction::IsPtr => "ptr?",
            Instruction::IsBool => "bool?",
            Instruction::IsSymbol => "symbol?",
            Instruction::IsArray => "array?",
            Instruction::IsBytes => "bytes?",
            Instruction::IsTable => "table?",
            Instruction::IsClosure => "closure?",
            Instruction::IsPap => "pap?",
            Instruction::IsCC => "cc?",
            Instruction::IsObject => "object?",
            Instruction::IsImmediate => "immediate?",
            Instruction::GetArg => "arg",
            Instruction::StackFrame => "stack_frame",
            Instruction::StackArray => "stack_array",
            Instruction::ArgCount => "arg_count",
            Instruction::PushMinusOne => "push -1",
            Instruction::PushNil => "push nil",
            Instruction::PushFalse => "push false",
            Instruction::PushTrue => "push true",
        };
        write!(f, "{}", str)
    }
}

impl From<u8> for Instruction {
    fn from(value: u8) -> Self {
        match value {
            21 | 23..=28 | 65..=66 | 69..=72 | 96..=99 | 124..=125 | 145..=151 | 156.. => {
                Instruction::Invalid
            }
            _ => unsafe { mem::transmute(value) },
        }
    }
}

fn disassemble_instruction(code: &[u8], out: &mut String) -> usize {
    let op = code[0];
    let ins = Instruction::from(op);
    out.push_str(&format!("{}", ins));
    let num = if op == INSTR_LOAD_BYTES_IMM {
        let size = u32::from_le_bytes(*code[1..].first_chunk::<4>().unwrap()) as usize;
        let bytes = &code[5..5 + size];
        out.push_str(&format!("#{}[", size));
        let mut iter = bytes.iter();
        if let Some(b) = iter.next() {
            out.push_str(&format!("{}", b));
            for b in iter {
                out.push_str(&format!(", {}", b));
            }
        }
        out.push(']');
        5 + bytes.len()
    } else {
        match ins.trailing_bytes() {
            8 => {
                let val = u64::from_le_bytes(*code[1..].first_chunk::<8>().unwrap());
                out.push_str(&format!(" 0x{:x}", val));
                9
            }
            4 => {
                let val = u32::from_le_bytes(*code[1..].first_chunk::<4>().unwrap());
                out.push_str(&format!(" 0x{:x}", val));
                5
            }
            2 => {
                let val = u16::from_le_bytes(*code[1..].first_chunk::<2>().unwrap());
                out.push_str(&format!(" 0x{:x}", val));
                3
            }
            1 => {
                out.push_str(&format!(" {}", code[1]));
                2
            }
            _ => 1,
        }
    };
    out.push('\n');
    num
}

pub fn disassemble(code: &[u8]) -> String {
    let mut s = String::new();
    let mut pos = 0;
    while pos < code.len() {
        let consumed = disassemble_instruction(&code[pos..], &mut s);
        pos += consumed;
    }
    s
}

// Comparisons
pub const INSTR_GT: u8 = 29;
pub const INSTR_GTE: u8 = 30;
pub const INSTR_LT: u8 = 31;
pub const INSTR_LTE: u8 = 32;

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
pub const INSTR_BITAND: u8 = 42;
pub const INSTR_BITOR: u8 = 43;
pub const INSTR_BITXOR: u8 = 44;
pub const INSTR_BITSHIFT: u8 = 45;
pub const INSTR_BITNOT: u8 = 46;

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
pub const INSTR_TOPTR: u8 = 59; // Unused

// Calls
pub const INSTR_CALL: u8 = 60;
pub const INSTR_CALL_TAIL: u8 = 61;

// Moves
pub const INSTR_DUP: u8 = 62;
pub const INSTR_SWAP: u8 = 63; // Unused
pub const INSTR_DROP: u8 = 64;
pub const INSTR_DROP_FRAME: u8 = 65; // Unused
pub const INSTR_STACK_LIFT: u8 = 66; // Unused
pub const INSTR_STACK_STORE: u8 = 67; // Unused
pub const INSTR_STACK_LOAD: u8 = 68;
pub const INSTR_LOAD_IMM8: u8 = 73;
pub const INSTR_LOAD_IMM16: u8 = 74;
pub const INSTR_LOAD_IMM32: u8 = 75;
pub const INSTR_LOAD_IMM64: u8 = 76;
pub const INSTR_LOAD_BYTES_IMM: u8 = 77;
pub const INSTR_LOAD_BYTES8: u8 = 78;
pub const INSTR_ROT: u8 = 79; // Unused

// Branches
pub const INSTR_JUMP: u8 = 80;
pub const INSTR_JZ: u8 = 81;
pub const INSTR_JPOS: u8 = 82;
pub const INSTR_JNEG: u8 = 83;
pub const INSTR_JNPOS: u8 = 84;
pub const INSTR_JNNEG: u8 = 85;
pub const INSTR_JFALSE: u8 = 86;
pub const INSTR_JNFALSE: u8 = 87;

pub const INSTR_JUMP_IMM8: u8 = 88;
pub const INSTR_JZ_IMM8: u8 = 89; // Unused
pub const INSTR_JPOS_IMM8: u8 = 90; // Unused
pub const INSTR_JNEG_IMM8: u8 = 91; // Unused
pub const INSTR_JNPOS_IMM8: u8 = 92; // Unused
pub const INSTR_JNNEG_IMM8: u8 = 93; // Unused
pub const INSTR_JFALSE_IMM8: u8 = 94;
pub const INSTR_JNFALSE_IMM8: u8 = 95;

// Symbol
pub const INSTR_MAKE_SYMBOL: u8 = 100;

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
pub const INSTR_ARRAY_SIZE: u8 = 110;

// Tables
pub const INSTR_TABLE_GET: u8 = 111;
pub const INSTR_TABLE_SET: u8 = 112;
pub const INSTR_TABLE_DELETE: u8 = 113;
pub const INSTR_TABLE_SIZE: u8 = 114;

// Bytes
pub const INSTR_BYTES_REF: u8 = 115;
pub const INSTR_BYTES_SET: u8 = 116;
pub const INSTR_BYTES_SIZE: u8 = 117;

// Misc
pub const INSTR_GET_LOCAL: u8 = 118;
pub const INSTR_SET_LOCAL: u8 = 119;
pub const INSTR_GLOBAL_ENV: u8 = 120;
pub const INSTR_INVALID: u8 = 121; // Unused
pub const INSTR_HALT: u8 = 122; // Unused
pub const INSTR_NOP: u8 = 22;
pub const INSTR_RETURN: u8 = 123;

// Fast repr checks
pub const INSTR_IS_INTEGER: u8 = 126;
pub const INSTR_IS_CHAR: u8 = 127;
pub const INSTR_IS_NIL: u8 = 128;
pub const INSTR_IS_FLOAT: u8 = 129;
pub const INSTR_IS_POINTER: u8 = 130;
pub const INSTR_IS_BOOL: u8 = 131;
pub const INSTR_IS_SYMBOL: u8 = 132;
pub const INSTR_IS_ARRAY: u8 = 133;
pub const INSTR_IS_BYTES: u8 = 134;
pub const INSTR_IS_TABLE: u8 = 135;
pub const INSTR_IS_CLOSURE: u8 = 136;
pub const INSTR_IS_PAP: u8 = 137;
pub const INSTR_IS_CALL_CONT: u8 = 138; // Unused
pub const INSTR_IS_OBJECT: u8 = 139; // Unused
pub const INSTR_IS_IMMEDIATE: u8 = 140; // Unused

pub const INSTR_GET_ARG: u8 = 141;
pub const INSTR_STACK_FRAME: u8 = 142;
pub const INSTR_STACK_ARRAY: u8 = 143;
pub const INSTR_ARG_COUNT: u8 = 144;

pub const INSTR_PUSH_MINUS_ONE: u8 = 152;
pub const INSTR_PUSH_NIL: u8 = 153;
pub const INSTR_PUSH_FALSE: u8 = 154;
pub const INSTR_PUSH_TRUE: u8 = 155;
pub const INSTR_PUSH_ZERO: u8 = 0;
pub const INSTR_PUSH_LAST_SMALL: u8 = 20;

pub(super) fn is_immediate_jump(op: u8) -> bool {
    (INSTR_JUMP_IMM8..=INSTR_JNFALSE_IMM8).contains(&op)
}

pub(super) fn stack_return(op: u8) -> usize {
    match op {
        INSTR_SWAP | INSTR_DROP | INSTR_ROT | INSTR_DROP_FRAME | INSTR_STACK_LIFT
        | INSTR_STACK_STORE | INSTR_JUMP | INSTR_JZ | INSTR_JPOS | INSTR_JNEG | INSTR_JNPOS
        | INSTR_JNNEG | INSTR_JFALSE | INSTR_JNFALSE | INSTR_JUMP_IMM8 | INSTR_JZ_IMM8
        | INSTR_JPOS_IMM8 | INSTR_JNEG_IMM8 | INSTR_JNPOS_IMM8 | INSTR_JNNEG_IMM8
        | INSTR_JFALSE_IMM8 | INSTR_JNFALSE_IMM8 | INSTR_SET_TYPE | INSTR_ARRAY_SET
        | INSTR_TABLE_SET | INSTR_TABLE_DELETE | INSTR_BYTES_SET | INSTR_SET_LOCAL
        | INSTR_INVALID | INSTR_HALT | INSTR_NOP => 0,
        _ => 1,
    }
}
