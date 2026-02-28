#![cfg(test)]

use crate::{ast::Module, common::Result, core::desugar, parser::parse_declarations};

#[test]
fn desugar_simple() -> Result<()> {
    let mut t = Module::new(None, "a = 1");
    parse_declarations(&mut t)?;
    let co = desugar(t)?;
    assert_eq!(co.bindings.len(), 1);
    Ok(())
}

#[test]
fn desugar_pattern() -> Result<()> {
    let mut t = Module::new(None, "[a, b] = [1, 2]");
    parse_declarations(&mut t)?;
    let co = desugar(t)?;
    Ok(())
}

#[test]
fn desugar_function() -> Result<()> {
    let prog = "fac 0 = 1
fac n = n * (fac (n - 1))";
    let mut t = Module::new(None, prog);
    parse_declarations(&mut t)?;
    let co = desugar(t)?;
    Ok(())
}

#[test]
fn desugar_array() -> Result<()> {
    let prog = "test [a,b,c] = [1,2,3]";
    let mut t = Module::new(None, prog);
    parse_declarations(&mut t)?;
    let co = desugar(t)?;
    //   dbg!(co);
    Ok(())
}
