use std::collections::HashMap;
use crate::common::Result;
use crate::common::{Metadata, Name};
use crate::core::Binder;

pub fn compile(from: crate::core::Module) -> Result<Module> {
    let mut module = Module::new();
    let mut compilation = Compilation::new(from);
    compilation.compile()?;
    Ok(compilation.module)
}

pub fn compile_expression(expr: crate::core::Expression) -> Result<Module> {
    let binding = crate::core::Binding::binding(0, Binder::Local(Name::str("it")), expr);
    let module = crate::core::Module { metadata: Metadata::new(None), bindings: vec![binding] };
    compile(module)
}

#[derive(Debug, Clone)]
pub struct Module {
    metadata: Option<Metadata>,
    bcode: Vec<u8>,
    local_env_size: usize,
    /// A list of symbols that should be put into the local environment.
    want_symbols: HashMap<String, usize>,
}

impl Module {
    pub fn new() -> Self {
        Module {
            metadata: None,
            bcode: Vec::new(),
            local_env_size: 0,
            want_symbols: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub(in crate::interpreter) struct Compilation {
    module: Module,
    from: crate::core::Module,
    next_loc : usize,
    local_vars:HashMap<String, usize>,
    data : Vec<u8>
}

impl Compilation {
    pub(super) fn new(from: crate::core::Module) -> Self {
        Compilation {
            module: Module::new(),
            from,
            next_loc: 0,
            local_vars: HashMap::new(),
            data: Vec::new()
        }
    }

    pub(super) fn compile(&mut self) -> Result<()> {
        for b in &self.from.bindings {

        }
        Ok(())
    }

    fn compile_binding(&mut self, binding: &crate::core::Binding) -> Result<()> {
        todo!();
    }

    fn fresh_local(&mut self) -> usize {
        let n = self.next_loc;
        self.next_loc += 1;
        n
    }
}