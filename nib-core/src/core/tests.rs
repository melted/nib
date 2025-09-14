#![cfg(test)]

use crate::{common::Result, core::desugar, parser::parse_declarations};



#[test]
fn desugar_simple() -> Result<()> {
    let t = parse_declarations(None, "a = 1")?;
    let co = desugar(t)?;
    assert_eq!(co.bindings.len(), 1);
    Ok(())
}