use crate::ast::{Expression, Literal, Operator};
use crate::common::Result;
use crate::parser::helpers::NameOrOperator;
use crate::parser::lexer::TokenValue;
use super::ParserState;



impl<'a> ParserState<'a> {
    pub(super) fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_inner_expression(0)
    }

    pub(super) fn parse_inner_expression(&mut self, min_pred:i32) -> Result<Expression> {
        let indent = self.indent();
        self.indent_stack.push(indent);
        let mut lhs = self.parse_left_expression()?;
        loop {
            let tok = self.peek_next_token()?;
            if self.next_indent() <= indent {
                dbg!(indent);
                return Ok(lhs);
            }
            let result = match tok.value {
                TokenValue::Where => { 
                    if min_pred < 2 {
                        self.parse_where_expression(lhs)?
                    } else {
                        break;
                    }
                },
                TokenValue::Operator(op) => {
                    if min_pred < 6 {
                        self.get_next_token()?;
                        self.parse_binop_expression(lhs, Operator::Plain(op))?
                    } else {
                        break;
                    }
                },
                TokenValue::FatRightArrow => {
                    if min_pred < 4 {
                        self.parse_cond_expression(lhs)?
                    } else {
                        break;
                    }
                },
                TokenValue::Identifier(_) => {
                    match self.parse_name_or_operator()? {
                        NameOrOperator::Name(name) if min_pred < 9 => {
                            let var = self.var_expression(name);
                            self.app_expression(lhs, var)
                        },
                        NameOrOperator::Operator(op) if min_pred < 6 => {
                            self.parse_binop_expression(lhs, op)?
                        }
                        _ => {
                            break;
                        }
                    }
                },
                TokenValue::Eof => break,
                _ => {
                    if min_pred < 9 {
                        let expr = self.try_parse(&mut |s|s.parse_inner_expression(9))?;
                        match expr {
                            Some(e) => self.app_expression(lhs, e),
                            None => {
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }
            };
            lhs = result;
        };
        self.indent_stack.pop();
        Ok(lhs)
    }


    pub(super) fn parse_left_expression(&mut self) -> Result<Expression> {
        let tok = self.peek_next_token()?;
        match tok.value {
            x if x.is_literal() => {
                let lit = self.parse_literal()?;
                Ok(self.literal_expression(lit))
            },
            TokenValue::LeftBrace => self.parse_lambda_expression(),
            TokenValue::LeftBracket => self.parse_array_expression(),
            TokenValue::LeftParen => self.parse_paren_expression(),
            TokenValue::Identifier(_) => {
                let name = self.parse_name()?;
                Ok(self.var_expression(name))
            },
            _ => {
                self.error(&format!("Illegal token in left expression {tok:?}"))
            }
        }
    }

    fn semicolon_or_newline(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;

        match next.value {
            TokenValue::Semicolon => {
                self.expect(TokenValue::Semicolon)?;
                Ok(true)
            }, 
            TokenValue::Eof => Ok(false),
            _ if next.on_new_line => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn parse_where_expression(&mut self, lhs:Expression) -> Result<Expression> {
        self.expect(TokenValue::Where)?;
        let mut bindings = Vec::new();
        loop {
            let binding = self.parse_binding()?;
            bindings.push(binding);
            if !self.semicolon_or_newline()? {
                break;
            }
        }
        Ok(self.where_expression(lhs, bindings))
    }

    pub(super) fn parse_binop_expression(&mut self, lhs:Expression, op:Operator) -> Result<Expression> {
        let rhs = self.parse_inner_expression(6)?;
        Ok(self.binop_expression(op, lhs, rhs))
    }

    pub(super) fn parse_cond_expression(&mut self, lhs:Expression) -> Result<Expression> {
        let on_true = self.parse_inner_expression(5)?;
        if self.semicolon_or_newline()? {
            let on_false = self.parse_inner_expression(5)?;
            Ok(self.cond_expression(lhs, on_true, on_false))
        } else {
            self.error("Missing else expression in => expression")
        }
    }

    pub(super) fn parse_lambda_expression(&mut self) -> Result<Expression> {
        self.expect(TokenValue::LeftBrace)?;
        let mut clauses = Vec::new();
        loop {
            let args = self.parse_some1(&mut Self::parse_pattern)?;
            self.expect(TokenValue::RightArrow)?;
            let expr = self.parse_expression()?;
            clauses.push(self.fun_clause(args, expr));
            if !self.semicolon_or_newline()? {
                break;
            }
        }
        self.expect(TokenValue::RightBrace)?;
        Ok(self.lambda_expression(clauses))
    }

    pub(super) fn parse_array_expression(&mut self) -> Result<Expression> {
        self.expect(TokenValue::LeftBracket)?;
        let exprs = self.parse_separated_by(&mut Self::parse_expression, TokenValue::Comma)?;
        self.expect(TokenValue::RightBracket)?;
        Ok(self.array_expression(exprs))
    }

    pub(super) fn parse_paren_expression(&mut self) -> Result<Expression> {
        self.expect(TokenValue::LeftParen)?;
        let token = self.peek_next_token()?;
        let exp = match token.value {
            TokenValue::RightParen => {
                self.get_next_token()?;
                self.literal_expression(Literal::Nil)
            },
            TokenValue::Operator(op) => {
                self.get_next_token()?;
                self.expect(TokenValue::RightParen)?;
                self.var_expression(crate::ast::Name::Plain(op))
            },
            _ => {
                let exp = self.parse_inner_expression(0)?;
                self.expect(TokenValue::RightParen)?;
                exp
            }
        };
        Ok(exp)
    }
}