#![cfg(test)]

use crate::{bytecode::Compilation, common::Result, core::desugar, parser::parse_declarations};

#[test]
fn create_compilation() -> Result<()> {
    let ast_mod = parse_declarations(None, "a = 1")?;
    let core_mod = desugar(ast_mod)?;
    let comp = Compilation::new(core_mod);
    Ok(())
}