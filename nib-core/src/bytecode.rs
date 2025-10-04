//! Compile a module into a bytecode module
use crate::common::Result;

mod tests;

pub fn compile(module: crate::core::Module) -> Result<Module> {
    let mut module = Module::new();
    todo!()
}



#[derive(Debug, Clone)]
pub struct Module {
    bcode: Vec<u8>,
    scratch_mem_size: usize,
    /// A list of symbols that should be put into the scratch space.
    want_symbols: Vec<String>,
}

impl Module {
    pub fn new() -> Self {
        Module {
            bcode: Vec::new(),
            scratch_mem_size: 0,
            want_symbols: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct Compilation {
    module:Module
}

impl Compilation {
    fn new() -> Self {
        Compilation { module: Module::new() }
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
