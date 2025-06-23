use crate::ast::{Expression, Literal, Operator};
use crate::common::Result;
use crate::parser::helpers::NameOrOperator;
use crate::parser::lexer::TokenValue;
use super::ParserState;



impl<'a> ParserState<'a> {
    pub(super) fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_inner_expression(0, false)
    }

    pub(super) fn parse_inner_expression(&mut self, min_pred:i32, in_paren:bool) -> Result<Expression> {
        let indent = if in_paren { -1 } else { self.indent() }; // TODO: maybe it's reasonable that
                                                                     // paren expressions have to indent too?
        self.indent_stack.push(indent);
        let mut lhs = self.parse_left_expression()?;
        loop {
            let tok = self.peek_next_token()?;
            if self.next_indent() <= indent {
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
                        self.parse_binop_expression(lhs, Operator::Plain(op))?
                    } else {
                        break;
                    }
                },
                TokenValue::RightArrow => {
                    if min_pred < 4 {
                        self.parse_cond_expression(lhs)?
                    } else {
                        break;
                    }
                },
                _ => {
                    if self.is_next_identifier()? {
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
                    } else if min_pred < 9 {
                        let expr = self.try_parse(&mut |s|s.parse_inner_expression(9, false))?;
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

    pub(super) fn parse_where_expression(&mut self, lhs:Expression) -> Result<Expression> {
        todo!()
    }

    pub(super) fn parse_binop_expression(&mut self, lhs:Expression, op:Operator) -> Result<Expression> {
        todo!()
    }

    pub(super) fn parse_cond_expression(&mut self, lhs:Expression) -> Result<Expression> {
        todo!()
    }

    pub(super) fn parse_lambda_expression(&mut self) -> Result<Expression> {
        todo!()
    }

    pub(super) fn parse_array_expression(&mut self) -> Result<Expression> {
        todo!()
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
                let exp = self.parse_inner_expression(0, true)?;
                self.expect(TokenValue::RightParen)?;
                exp
            }
        };
        Ok(exp)
    }
}