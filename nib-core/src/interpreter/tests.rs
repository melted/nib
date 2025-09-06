#![cfg(test)]

use crate::{common::Result, interpreter::heap::{Array, Heap, Space, Symbol, Table, Value, ValueRepr}};

#[test]
fn test_data_repr() -> Result<()> {
    let val = Value::integer(5);
    assert_eq!(val.get_repr(), ValueRepr::Integer);
    let b = Value::bool(false);
    assert_eq!(b.get_repr(), ValueRepr::Bool);
    Ok(())
}

#[test]
fn test_negative() -> Result<()> {
    let val = Value::integer(-3);
    assert_eq!(val.get_integer(), -3);
    Ok(())
}

#[test]
fn roundtrip_pointer() -> Result<()> {
    let hello = "hello";
    let hello_ptr = hello.as_ptr().cast_mut();
    let val = Value::pointer(hello_ptr);
    let roundtripped = val.get_pointer::<u8>();
    assert_eq!(roundtripped, hello_ptr);
    Ok(())
}

#[test]
fn roundtrip_char() -> Result<()> {
    let a = 'a';
    let val = Value::char(a);
    let b = val.get_char();
    assert_eq!(a,b);
    Ok(())
}

#[test]
fn create_space() {
    let space = Space::new(1000);
    assert_eq!(space.size, 1000);
}

#[test]
fn make_heap() {
    let mut heap = Heap::new(10000);
    let array = Array::make(&mut heap, 5);
    array.set(2, Value::integer(4));
    let val = array.at(2);
    assert_eq!(val.get_integer(), 4);
    let array2 = Array::make(&mut heap, 100);
    array.set(3, Value::from(array2));
}

#[test]
fn make_table() {
    let mut heap = Heap::new(10000);
    let mut table = Table::make(&mut heap);
    let key = Symbol::make(&mut heap, "key");
    table.insert(&mut heap, key, Value::integer(42));
    let keys = table.keys(&mut heap);
    assert_eq!(keys.get_immediate_repr(), ValueRepr::Array);
    let val = table.get(key);
    assert_eq!(val, Value::integer(42));
}