
use crate::{ast::{self, Binding, Binop, Cond, Expression, Literal, Operator}, common::{Error, Result}};

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

    pub(super) fn is_next_identifier(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Identifier(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_operator(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Operator(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_symbol(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Symbol(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_integer(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Integer(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_float(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Float(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_char(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Char(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_string(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::String(_) => Ok(true),
            _ => Ok(false)
        }
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
    ) -> Result<Vec<T>> {
        let mut output = Vec::new();
        while let Some(res) = self.try_parse(inner_parser)? {
            output.push(res);
        }
        Ok(output)
    }

    pub(super) fn parse_some1<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut output = Vec::new();
        let first = inner_parser(self)?;
        output.push(first);
        while let Some(res) = self.try_parse(inner_parser)? {
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
        let start = self.token_start;
        match inner_parser(self) {
            Ok(res) => Ok(Some(res)),
            Err(_err) => {
                self.token_start = start;
                Ok(None)
            }
        }
    }

    pub(super) fn parse_name(&mut self) -> Result<NameOrOperator> {
        let first = self.get_next_token()?;
        let mut id = match first.value {
            TokenValue::Identifier(id) => id,
            TokenValue::Operator(id) => {
                return Ok(NameOrOperator::Operator(ast::Operator::Plain(id)))
            }
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
                }
                _ => {
                    return self.error(&format!("Expected identifier or operator, got {:?}", &next.value));
                }
            };
        }
        let ret = if path.is_empty() { 
                            ast::Name::Plain(id)
                        } else {
                            ast::Name::Qualified(path, id)
                        };
        Ok(NameOrOperator::Name(ret))
    }

    pub(super) fn parse_literal(&mut self) -> Result<Literal> {
        todo!()
    }

    pub(super) fn parse_unit_nil(&mut self) -> Result<Literal> {
        self.expect(TokenValue::LeftParen)?;
        self.expect(TokenValue::RightParen)?;
        Ok(Literal::Nil)
    }

    pub(super) fn literal_expression(&mut self, lit: ast::Literal) -> ast::Expression {
        self.counter += 1;
        ast::Expression {
            id: self.counter,
            expr: ast::ExpressionKind::Literal(lit)
        }
    }

    pub(super) fn var_expression(&mut self, var: ast::Name) -> ast::Expression {
        self.counter += 1;
        ast::Expression {
            id: self.counter,
            expr: ast::ExpressionKind::Var(var)
        }
    }

    pub(super) fn array_expression(&mut self, vals: Vec<ast::Expression>) -> ast::Expression {
        self.counter += 1;
        ast::Expression {
            id: self.counter,
            expr: ast::ExpressionKind::Array(vals)
        }
    }

    pub(super) fn app_expression(&mut self, f: Expression, arg: Expression) -> ast::Expression {
        self.counter += 1;
        ast::Expression {
            id: self.counter,
            expr: ast::ExpressionKind::App(Box::new(f), Box::new(arg))
        }
    }

    pub(super) fn lambda_expression(&mut self, clauses: Vec<ast::FunClause>) -> ast::Expression {
        self.counter += 1;
        ast::Expression {
            id: self.counter,
            expr: ast::ExpressionKind::Lambda(clauses)
        }
    }

    pub(super) fn binop_expression(&mut self, op: Operator, lhs: Expression, rhs: Expression) -> ast::Expression {
        self.counter += 1;
        ast::Expression {
            id: self.counter,
            expr: ast::ExpressionKind::Binop(
                Binop {
                    op: op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs)
                }
            )
        }
    }

    pub(super) fn where_expression(&mut self, expr: Expression, bindings: Vec<Binding>) -> ast::Expression {
        self.counter += 1;
        ast::Expression {
            id: self.counter,
            expr: ast::ExpressionKind::Where(Box::new(expr), bindings)
        }
    }

    pub(super) fn cond_expression(&mut self, pred: Expression, on_true: Expression, on_false: Expression) -> ast::Expression {
        self.counter += 1;
        ast::Expression {
            id: self.counter,
            expr: ast::ExpressionKind::Cond(
                Cond {
                    pred: Box::new(pred),
                    on_true: Box::new(on_true),
                    on_false: Box::new(on_false)
                }
            )
        }
    }
}

pub enum NameOrOperator {
    Name(ast::Name),
    Operator(ast::Operator)
}