#![cfg(test)]

use crate::{common::Result, core::desugar, parser::parse_declarations};

#[test]
fn desugar_simple() -> Result<()> {
    let t = parse_declarations(None, "a = 1")?;
    let co = desugar(t)?;
    assert_eq!(co.bindings.len(), 1);
    Ok(())
}

#[test]
fn desugar_pattern() -> Result<()> {
    let t = parse_declarations(None, "[a, b] = [1, 2]")?;
    let co = desugar(t)?;
    dbg!(co);
    Ok(())
}

#[test]
fn desugar_function() -> Result<()> {
    let prog = "fac 0 = 1
fac n = n * (fac (n - 1))";
    let t = parse_declarations(None, prog)?;
    let co = desugar(t)?;
    dbg!(co);
    Ok(())
}
