#![cfg(test)]
use crate::{common::{Error, Name, Result}, core::Pattern, runtime::{evaluate::Environment, Runtime, Value}};

#[test]
fn simple_pattern_match() -> Result<()> {
    let pats = vec![Pattern::Wildcard, Pattern::Ellipsis(Some(Name::name("xs"))), Pattern::Bind(Name::name("x"))];
    let args = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
    let mut rt = Runtime::new();
    let env = Environment::new();
    let res = rt.match_patterns(&args, &pats, &env)?;
    match res {
        Some(map) => {
            assert!(map["x"] == Value::Integer(3));
            assert!(matches!(map["xs"], Value::Array(_)));
        },
        _ => { assert!(false) }
    }
    Ok(())
}