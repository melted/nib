use crate::{ ast::{self, ExpressionNode, FunClause, Literal, Operator, PatternNode}, common::{Name, Result}};

use super::{ParserState, lexer::TokenValue};


/// General help functions for parsing
impl<'a> ParserState<'a> {
    pub(super) fn expect(&mut self, t: TokenValue) -> Result<()> {
        let next = self.get_next_token()?;
        if t != next.value {
            self.error(&format!("expected {:?}, got {:?}", t, next.value))
        } else {
            Ok(())
        }
    }

    pub(super) fn is_next(&mut self, t: TokenValue) -> Result<bool> {
        let next = self.peek_next_token()?;
        if t == next.value {
            self.get_next_token()?; // Swallow token
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(super) fn peek_identifier(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Identifier(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn peek_operator(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Operator(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn peek_literal(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        Ok(next.value.is_literal())
    }

    pub(super) fn peek_next(&mut self, t: TokenValue) -> Result<bool> {
        let next = self.peek_next_token()?;
        if t == next.value {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(super) fn optional_token(&mut self, t: TokenValue) -> Result<()> {
        self.is_next(t)?;
        Ok(())
    }

    pub(super) fn parse_some<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> Result<T>,
        until_pred: impl Fn(TokenValue) ->bool 
    ) -> Result<Vec<T>> {
        let mut output = Vec::new();
        while until_pred(self.peek_next_token()?.value) {
            let res = inner_parser(self)?;
            output.push(res);
        }
        Ok(output)
    }

    pub(super) fn parse_some1<T> (
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> Result<T>,
        until_pred: impl Fn(TokenValue) ->bool 
    ) -> Result<Vec<T>> {
        let mut output = Vec::new();
        let first = inner_parser(self)?;
        output.push(first);
        while until_pred(self.peek_next_token()?.value) {
            let res = inner_parser(self)?;
            output.push(res);
        }
        Ok(output)
    }

    pub(super) fn parse_separated_by<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> Result<T>,
        separator: TokenValue,
    ) -> Result<Vec<T>> {
        let mut output = Vec::new();
        output.push(inner_parser(self)?);
        while self.is_next(separator.clone())? {
            output.push(inner_parser(self)?);
        }
        Ok(output)
    }

    pub(super) fn try_parse<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Option<T>> {
        let checkpoint = self.position();
        match inner_parser(self) {
            Ok(res) => Ok(Some(res)),
            Err(_err) => {
                if checkpoint <= self.position() {
                    self.rewind_lexer(checkpoint);
                } else {
                    let p = self.position();
                    panic!("rewinding forwards! {checkpoint} {p}");
                }
                Ok(None)
            }
        }
    }

    pub(super) fn parse_name(&mut self) -> Result<Name> {
        match self.parse_name_or_operator()? {
            NameOrOperator::Name(name) => Ok(name),
            _ => self.error("Expected a name, not an operator")
        }
    }

    pub(super) fn parse_operator(&mut self) -> Result<Operator> {
        let NameOrOperator::Operator(op) = self.parse_name_or_operator()? else {
            return self.error("Expected an operator, not a name");
        };
        Ok(op)
    }

    pub(super) fn parse_name_or_operator(&mut self) -> Result<NameOrOperator> {
        let first = self.get_next_token()?;
        let mut id = match first.value {
            TokenValue::Identifier(id) => id,
            TokenValue::Operator(id) => {
                return Ok(NameOrOperator::Operator(ast::Operator::Plain(id)))
            },
            _ => {
                return self.error(&format!("Expected identifier or operator, got {:?}", &first.value));
            }
        };
        let mut path = Vec::new();
        while self.is_next(TokenValue::Period)? {
            path.push(id);
            let next = self.get_next_token()?;
            id = match next.value {
                TokenValue::Identifier(id) => id,
                TokenValue::Operator(id) => {
                    return Ok(NameOrOperator::Operator(ast::Operator::Qualified(path, id)))
                },
                _ => {
                    return self.error(&format!("Expected identifier or operator, got {:?}", &next.value));
                }
            };
        }
        let ret = if path.is_empty() { 
                            Name::Plain(id)
                        } else {
                            Name::Qualified(path, id)
                        };
        Ok(NameOrOperator::Name(ret))
    }

    pub(super) fn parse_literal(&mut self) -> Result<Literal> {
        let token = self.get_next_token()?;
        match token.value {
            TokenValue::Nil => Ok(Literal::Nil),
            TokenValue::False => Ok(Literal::Bool(false)),
            TokenValue::True => Ok(Literal::Bool(true)),
            TokenValue::Integer(n) => Ok(Literal::Integer(n)),
            TokenValue::Float(x) => Ok(Literal::Real(x)),
            TokenValue::Char(ch) => Ok(Literal::Char(ch)),
            TokenValue::String(s) => Ok(Literal::String(s)),
            TokenValue::Hash => {
                let tok = self.get_next_token()?;
                match tok.value  {
                    TokenValue::Identifier(name) => {
                        Ok(Literal::Symbol(name))
                    },
                    TokenValue::LeftBracket => {
                        let bytes = self.parse_separated_by(&mut Self::parse_byte,TokenValue::Comma)?;
                        self.expect(TokenValue::RightBracket)?;
                        Ok(Literal::Bytearray(bytes))
                    },
                    _ => self.error("Expected either a symbol or bytearray after #")
                }
            },
            _ => {
                self.error(&format!("Expected a literal got token {token:?}"))
            }
        }
    }

    pub(super) fn parse_byte(&mut self) -> Result<u8> {
        if let TokenValue::Integer(b) = self.peek_next_token()?.value {
            self.get_next_token()?;
            let v:u8 =b.try_into().map_err(|_e|self.new_error(&format!("Invalid bytearray literal: {b:?}")))?;
            Ok(v)
        } else {
            let tok = self.get_next_token()?;
            self.error(&format!("Illegal token {tok:?} in bytearray literal"))
        }
    }

    pub(super) fn fun_clause(&mut self, args:Vec<PatternNode>, guard: Option<ExpressionNode>, body:ExpressionNode) -> FunClause {
        self.counter += 1;
        FunClause { id: self.counter, args, guard, body }
    }
}

pub enum NameOrOperator {
    Name(Name),
    Operator(ast::Operator)
}