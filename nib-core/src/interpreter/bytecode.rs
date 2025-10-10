#![allow(unused_variables)]
#![allow(dead_code)]

//! The byte code and functions to manipulate it.

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
    Branch = 100,
    BranchZero = 101,
    BranchPositive = 102,
    BranchNegative = 103,
}
