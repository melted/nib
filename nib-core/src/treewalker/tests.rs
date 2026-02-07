#![cfg(test)]

use crate::{common::Result, treewalker::Value};

#[test]
fn equality_of_values() -> Result<()> {
    let a = Value::new_bytes(vec![1, 2, 3]);
    let b = Value::new_bytes(vec![1, 2, 3]);
    let c = a.clone();
    assert_eq!(a, c);
    assert_ne!(a, b);
    Ok(())
}
