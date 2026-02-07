#![cfg(test)]

use crate::ast::Module;
use crate::common::sym;
use crate::core::desugar;
use crate::interpreter::Runtime;
use crate::interpreter::bytecode::INSTR_ADD;
use crate::interpreter::compile::Compilation;
use crate::interpreter::heap::{Bytes, Space};
use crate::parser::parse_declarations;
use crate::runtime::Interpreter;
use crate::{
    common::{Result, Symbol},
    interpreter::heap::{Array, Table, Value, ValueRepr},
};

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
fn roundtrip_cpointer() -> Result<()> {
    let hello = "hello";
    let hello_ptr = hello.as_ptr().cast();
    let val = Value::cpointer(hello_ptr);
    let roundtripped = val.get_cpointer::<u8>();
    assert_eq!(roundtripped, hello_ptr);
    Ok(())
}

#[test]
fn roundtrip_char() -> Result<()> {
    let a = 'a';
    let val = Value::char(a);
    let b = val.get_char();
    assert_eq!(a, b);
    Ok(())
}

#[test]
fn create_space() {
    let space = Space::new(1000);
    assert_eq!(space.size, 1000);
}

#[test]
fn make_heap() {
    let mut runtime = Runtime::new();
    let array = Array::make(&mut runtime, 5);
    array.set(2, Value::integer(4));
    let val = array.at(2);
    assert_eq!(val.get_integer(), 4);
    let array2 = Array::make(&mut runtime, 100);
    array.set(3, Value::from(array2));
    assert_eq!(array.size(), 5);
    assert_eq!(array2.size(), 100);
}

#[test]
fn make_table() {
    let mut runtime = Runtime::new();
    let mut table = Table::make(&mut runtime);
    let key = Value::from(Symbol::from("key"));
    table.insert(&mut runtime, key, Value::integer(42));
    let keys = table.keys(&mut runtime);
    assert_eq!(keys.get_immediate_repr(), ValueRepr::Array);
    assert_eq!(keys.get_array().at(0), Value::from(key));
    assert_eq!(keys.get_array().size(), 1);
    let val = table.get(key);
    assert_eq!(val, Value::integer(42));
}

#[test]
fn alloc_float() {
    let mut runtime = Runtime::new();
    let fl = Value::alloc_float(&mut runtime, 1.111);
    dbg!(fl.get_float());
}

#[test]
fn hash_stuff() {
    let mut runtime = Runtime::new();
    let mut table = Table::make(&mut runtime);
    let key = Value::from(Symbol::from("key"));
    table.insert(&mut runtime, key, Value::integer(42));
    table.insert(&mut runtime, Value::integer(12), Value::integer(22));
    let keys = table.keys(&mut runtime);
    dbg!(key.hash());
    dbg!(keys.hash());
    dbg!(Value::from(table).hash());
}

#[test]
fn create_compilation() -> Result<()> {
    let mut ast_mod = Module::new(None, "a = 1");
    parse_declarations(&mut ast_mod)?;
    let core_mod = desugar(ast_mod)?;
    let comp = Compilation::with(core_mod);
    Ok(())
}

#[test]
fn add_numbers() -> Result<()> {
    let mut rt = Runtime::new();
    let code = vec![INSTR_ADD];
    rt.stack_push(Value::integer(8));
    rt.stack_push(Value::integer(7));
    rt.code = Value::from(Bytes::with(&mut rt, &code));
    let err = rt.run();
    assert!(err.is_ok());
    let res = Value::get_integer(&rt.stack.pop());
    assert_eq!(res, 15);
    Ok(())
}

#[test]
fn run_literal_expression_integer() -> Result<()> {
    let mut rt = Runtime::new();
    let val = rt.run_expression("1")?;
    assert_eq!(val, Value::integer(1));
    Ok(())
}

#[test]
fn run_literal_expression_bytes() -> Result<()> {
    let mut rt = Runtime::new();
    let val = rt.run_expression("#[1,2,3]")?;
    assert_eq!(val.get_bytes().get_slice(), [1, 2, 3]);
    Ok(())
}

#[test]
fn run_literal_expression_string() -> Result<()> {
    let mut rt = Runtime::new();
    let val = rt.run_expression("\"hej\"")?;
    assert_eq!(val.get_bytes().get_slice(), [104, 101, 106]);
    Ok(())
}

#[test]
fn run_literal_expression_symbol() -> Result<()> {
    let mut rt = Runtime::new();
    let val = rt.run_expression("#hej")?;
    assert_eq!(val.get_symbol(), sym("hej"));
    Ok(())
}

#[test]
fn run_array_expression() -> Result<()> {
    let mut rt = Runtime::new();
    let val = rt.run_expression("[1,2]")?;
    assert_eq!(
        val.get_array().values(),
        [Value::integer(1), Value::integer(2)]
    );
    Ok(())
}

#[test]
fn run_lambda_expression() -> Result<()> {
    let mut rt = Runtime::new();
    let val = rt.run_expression("{a}")?;
    dbg!(val);
    Ok(())
}

#[test]
fn run_lambda_app_expression() -> Result<()> {
    let mut rt = Runtime::new();
    let val = rt.run_expression("{a} 1")?;
    assert_eq!(val.get_integer(), 1);
    Ok(())
}

#[test]
fn run_lambda_app_args_expression() -> Result<()> {
    let mut rt = Runtime::new();
    let val = rt.run_expression("{ a b -> b } 1 2")?;
    assert_eq!(val.get_integer(), 2);
    Ok(())
}

#[test]
fn run_lambda_app_multiple_args_expression() -> Result<()> {
    let mut rt = Runtime::new();
    let val = rt.run_expression("{ _prim_mul a b } 3 3")?;
    assert_eq!(val.get_integer(), 9);
    Ok(())
}

#[test]
fn run_simple_binding() -> Result<()> {
    let mut rt = Runtime::new();
    rt.add_code("test", "a=1")?;
    let val = rt.get_global(&sym("a"));
    assert_eq!(val.get_integer(), 1);
    Ok(())
}

#[test]
fn run_compute_binding() -> Result<()> {
    let mut rt = Runtime::new();
    rt.add_code("test", "a = _prim_mul 3 5")?;
    let val = rt.get_global(&sym("a"));
    assert_eq!(val.get_integer(), 15);
    Ok(())
}

#[test]
fn run_globalref_binding() -> Result<()> {
    let mut rt = Runtime::new();
    rt.add_code("test", "a = system.arch")?;
    let val = rt.get_global(&sym("a"));
    dbg!(val);
    Ok(())
}

#[test]
fn run_fun_binding() -> Result<()> {
    let mut rt = Runtime::new();
    rt.add_code("test", "calc a = _prim_mul a 2")?;
    let val = rt.get_global(&sym("calc"));
    assert_eq!(val.is_closure(), true);
    Ok(())
}

#[test]
fn run_fun_clauses_binding() -> Result<()> {
    let mut rt = Runtime::new();
    rt.add_code(
        "test",
        "fac 0 = 1\nfac n = _prim_mul n (fac (_prim_sub n 1))",
    )?;
    let val = rt.get_global(&sym("fac"));
    assert_eq!(val.is_closure(), true);
    Ok(())
}

#[test]
fn run_fun_clauses_app_binding() -> Result<()> {
    let mut rt = Runtime::new();
    rt.add_code(
        "test",
        "fac 0 = 1\nfac n = _prim_mul n (fac (_prim_sub n 1))\nres=fac 5\n",
    )?;
    let val = rt.get_global(&sym("res"));
    dbg!(val);
    assert_eq!(val.get_integer(), 120);
    Ok(())
}
