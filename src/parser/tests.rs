#![cfg(test)] 
use crate::ast::{Binding, Declaration, ExpressionKind, Literal, Name, PatternKind};
use crate::common::Result;
use crate::parser::expression::UsedImplicits;
use crate::parser::{lex, ParserState};
use crate::parser::lexer::TokenValue;

#[test]
fn lex_numbers() -> Result<()> {
    let tokens = lex("1 2 3")?;
    assert_eq!(tokens[0], TokenValue::Integer(1));
    assert_eq!(tokens[1], TokenValue::Integer(2));
    assert_eq!(tokens[2], TokenValue::Integer(3));
    Ok(())
}

#[test]
fn lex_identifier() -> Result<()> {
    let tokens = lex("hello_world aaa bbb ccc")?;
    assert_eq!(tokens.len(), 5);
    assert_eq!(tokens[0], TokenValue::Identifier("hello_world".to_string()));
    Ok(())
}

#[test]
fn parse_names() -> Result<()> {
    let mut state = ParserState::new("a.name");
    let ret = state.parse_name();
    assert!(ret.is_ok());
    match ret {
        Ok(Name::Qualified(p, n)) => {
            assert_eq!(n, "name");
            assert_eq!(p.len(), 1);
            assert_eq!(p[0], "a");
        },
        _ => assert!(false)
    }
    Ok(())
}

#[test]
fn parse_string_literal() -> Result<()> {
    let mut state = ParserState::new("\"hello, world\"");
    let lit = state.parse_literal()?;
    assert!(Literal::String("hello, world".to_string()) == lit);
    Ok(())
}

    #[test]
fn parse_bytearray_literal() -> Result<()> {
    let mut state = ParserState::new("#[123,133,123]");
    let lit = state.parse_literal()?;
    assert!(Literal::Bytearray(vec![123,133,123]) == lit);
    Ok(())
}

#[test]
fn parse_invalid_bytearray_literal() -> Result<()> {
    let mut state = ParserState::new("#[123,333,123]");
    let lit = state.parse_literal();
    assert!(lit.is_err());
    Ok(())
}

#[test]
fn parse_literal_expression() -> Result<()> {
    let mut state = ParserState::new("a");
    let exp = state.parse_expression()?;
    Ok(())
}

#[test]
fn parse_binop_expression() -> Result<()> {
    let mut state = ParserState::new("1 + 2");
    let exp = state.parse_expression()?;
    Ok(())
}

#[test]
fn parse_literal_pattern() -> Result<()> {
    let mut state = ParserState::new("1");
    let pat = state.parse_pattern()?;
    Ok(())
}

#[test]
fn parse_module_declaration() -> Result<()> {
    let mut state = ParserState::new("module cool.mod");
    let decl = state.parse_declaration()?;
    if let Declaration::Module(module) = decl {
        assert_eq!(module.name,
                   Name::Qualified(vec!["cool".to_string()], "mod".to_string()));
    } else {
        assert!(false);
    }
    Ok(())
}

#[test]
fn parse_custom_pattern() -> Result<()> {
    let mut state = ParserState::new("(pair a b)");
    let pat = state.parse_pattern()?;
    match &pat.pattern{
        PatternKind::Custom(name,fields ) => {
            assert_eq!(name.to_string(), "pair");
            assert_eq!(fields[0].pattern, PatternKind::Var(Name::name("a")));
            assert_eq!(fields[1].pattern, PatternKind::Var(Name::name("b")));
        },
        _ => assert!(false)
    }
    Ok(())
}

#[test]
fn parse_simple_binding() -> Result<()> {
    let mut state = ParserState::new("a = 1");
    let decl = state.parse_declaration()?;
    match decl {
        Declaration::Binding(Binding::VarBinding(bind )) => {
            assert_eq!(bind.lhs.pattern, PatternKind::Var(Name::name("a")));
            let exp = bind.rhs.expr;
            assert_eq!(exp, ExpressionKind::Literal(Literal::Integer(1)));
        }
        _ => assert!(false)
    }
    Ok(())
}

#[test]
fn parse_lambda_expression() -> Result<()> {
    let mut state = ParserState::new("{ a -> a + 1 }");
    let expr = state.parse_expression()?;
    match expr.expr {
        ExpressionKind::Lambda(fc) => {
            assert_eq!(fc.len(), 1);
            let PatternKind::Var(ref x) = fc[0].args[0].pattern else {
                assert!(false);
                return state.error("meh");
            };
            assert_eq!(x, &Name::name("a"));
            let ExpressionKind::Binop(ref op) = fc[0].body.expr else {
                assert!(false);
                return state.error("meh");
            };
        }
        _ => assert!(false)
    }
    Ok(())
}

#[test]
fn parse_double_binop() -> Result<()> {
    let mut state = ParserState::new("1+2+3");
    let expr = state.parse_expression()?;
    match expr.expr {
        ExpressionKind::Binop(op) => {
            assert_eq!(op.lhs.expr, ExpressionKind::Literal(Literal::Integer(1)));
            match op.rhs.expr {
                ExpressionKind::Binop(op2) => {
                    assert_eq!(op2.lhs.expr, ExpressionKind::Literal(Literal::Integer(2)));
                }
                _ => assert!(false)
            }
        }
        _ => assert!(false)
    }
    Ok(())
}

#[test]
fn lex_double_peek_at_end() -> Result<()> {
    let mut state = ParserState::new("a = 1 + 2");
    for i in 0..5 {
        state.get_next_token()?;
    };
    let y = state.peek_next_token()?;
    let z = state.peek_next_token()?;
    assert_eq!(y, z);
    Ok(())
}

#[test]
fn parse_guarded_decl() -> Result<()> {
    let mut state = ParserState::new("f n | n > 5 = 10");
    let decl = state.parse_declaration()?;
    Ok(())
}

#[test]
fn test_implicit_visitor() -> Result<()> {
    let mut state = ParserState::new("a * b + e");
    let expr = state.parse_expression()?;
    let mut visitor = UsedImplicits::new();
    expr.visit(&mut visitor);
    assert!(visitor.vars.contains(&Name::name("a")));
    assert!(visitor.vars.contains(&Name::name("b")));
    assert!(!visitor.vars.contains(&Name::name("e")));
    Ok(())
}

#[test]
fn parse_implicit_lambda() -> Result<()> {
    let mut state = ParserState::new("{ a*b*c }");
    let expr = state.parse_expression()?;
    dbg!(expr);
    Ok(())
}

#[test]
fn empty_test_skeleton() -> Result<()> {
    let mut state = ParserState::new("");
    Ok(())
}