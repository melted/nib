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

#[test]
fn pattern_arity_fail() -> Result<()> {
    let pats = vec![Pattern::Wildcard, Pattern::Bind(Name::name("x")), Pattern::Wildcard, Pattern::Wildcard];
    let args = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
    let mut rt = Runtime::new();
    let env = Environment::new();
    let res = rt.match_patterns(&args, &pats, &env)?;
    assert!(res.is_none());
    Ok(())
}

#[test]
fn equality_of_values() -> Result<()> {
    let a = Value::new_bytes(vec![1,2,3]);
    let b = Value::new_bytes(vec![1,2,3]);
    let c = a.clone();
    assert_eq!(a, c);
    assert_ne!(a, b);
    Ok(())
}