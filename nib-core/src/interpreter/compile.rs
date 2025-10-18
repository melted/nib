use crate::common::{Result, Symbol};
use crate::common::{Metadata, Name};
use crate::core::Binder;
use crate::interpreter::heap::Value;
use std::collections::HashMap;
use std::mem;

pub fn compile(from: crate::core::Module) -> Result<Module> {
    let mut module = Module::new();
    let mut compilation = Compilation::new(from);
    compilation.compile()?;
    Ok(compilation.module)
}

pub fn compile_expression(expr: crate::core::Expression) -> Result<Module> {
    let binding = crate::core::Binding::binding(0, Binder::Local(Name::str("it")), expr);
    let module = crate::core::Module {
        metadata: Metadata::new(None),
        bindings: vec![binding],
    };
    compile(module)
}

#[derive(Debug, Clone)]
pub struct Module {
    metadata: Option<Metadata>,
    bcode: Vec<u8>,
    local_env_size: usize,
    /// A list of symbol literals that should be put into the local environment.
    want_symbols: HashMap<Symbol, usize>,
    /// Global variables used by the module.
    captures: HashMap<Symbol, usize>,
    constants: HashMap<usize, Value>,
}

impl Module {
    pub fn new() -> Self {
        Module {
            metadata: None,
            bcode: Vec::new(),
            local_env_size: 0,
            want_symbols: HashMap::new(),
            captures: HashMap::new(),
            constants: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub(in crate::interpreter) struct Compilation {
    module: Module,
    core_bindings: Vec<crate::core::Binding>,
    next_loc: usize,
    local_vars: HashMap<Symbol, usize>,
    data: Vec<u8>,
}

impl Compilation {
    pub(super) fn new(from: crate::core::Module) -> Self {
        let metadata = from.metadata;
        let bindings = from.bindings;
        let mut compilation = Compilation {
            module: Module::new(),
            core_bindings: bindings,
            next_loc: 0,
            local_vars: HashMap::new(),
            data: Vec::new(),
        };
        compilation.module.metadata = Some(metadata);
        compilation
    }

    pub(super) fn compile(&mut self) -> Result<()> {
        let bindings = mem::replace(&mut self.core_bindings, vec![]);
        for b in bindings {
            self.compile_binding(&b)?;
        }
        Ok(())
    }

    fn compile_binding(&mut self, binding: &crate::core::Binding) -> Result<()> {
        todo!();
    }

    fn fresh_env_location(&mut self) -> usize {
        let n = self.next_loc;
        self.next_loc += 1;
        n
    }
}
