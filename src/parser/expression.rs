use std::collections::HashSet;
use std::ops::Index;

use crate::ast::{AstVisitor, Binding, Binop, Cond, Expression, ExpressionKind, FunClause, Literal, Name, Operator, Pattern};
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
        self.indent_stack.push(indent); // TODO: Is the stack needed? It's not used now.
        // Also, it may become corrupted on error returns.
        let mut lhs = self.parse_left_expression()?;
        loop {
            let tok = self.peek_next_token()?;
            if self.next_indent() <= indent {
                self.indent_stack.pop();
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
                TokenValue::LeftBrace | TokenValue::LeftParen | TokenValue::LeftBracket => {
                    let expr = self.parse_left_expression()?;
                    self.app_expression(lhs, expr)
                },
                _ if tok.value.is_literal() => {
                    let expr = self.parse_left_expression()?;
                    self.app_expression(lhs, expr)
                }, 
                _ => {
                    break;
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
            TokenValue::Operator(op) if op == "-" => {
                // Special rule for prefix -, it's sugar for negate.
                Ok(self.var_expression(Name::name("negate")))
            }
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
        let indent = *self.indent_stack.last().unwrap_or(&0);
        loop {
            if self.next_indent() <= indent {
                break;
            }
            let binding = self.parse_binding()?;
            bindings.push(binding);
            if !self.semicolon_or_newline()? {
                break;
            }
        }
        Ok(self.where_expression(lhs, bindings))
    }

    pub(super) fn parse_binop_expression(&mut self, lhs:Expression, op:Operator) -> Result<Expression> {
        let rhs = self.parse_inner_expression(5)?;
        Ok(self.binop_expression(op, lhs, rhs))
    }

    pub(super) fn parse_cond_expression(&mut self, lhs:Expression) -> Result<Expression> {
        let on_true = self.parse_inner_expression(3)?;
        if self.semicolon_or_newline()? {
            let on_false = self.parse_inner_expression(3)?;
            Ok(self.cond_expression(lhs, on_true, on_false))
        } else {
            self.error("Missing else expression in => expression")
        }
    }

    pub(super) fn parse_fun_args(&mut self) -> Result<(Vec<Pattern>, Option<Expression>)> {
        let args = self.parse_some1(&mut Self::parse_pattern, |t| t != TokenValue::Bar && t != TokenValue::RightArrow && t != TokenValue::Equals)?;
        let guard = if self.is_next(TokenValue::Bar)? {
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok((args, guard))
    }

    pub(super) fn parse_lambda_expression(&mut self) -> Result<Expression> {
        if let Some(exp) = self.try_parse(&mut Self::parse_full_lambda_expression)? {
            Ok(exp)
        } else {
            self.parse_implicit_lambda_expression()
        }
    }

    pub(super) fn parse_full_lambda_expression(&mut self) -> Result<Expression> {
        self.expect(TokenValue::LeftBrace)?;
        let mut clauses = Vec::new();
        loop {
            let (args, guard) = self.parse_fun_args()?;

            self.expect(TokenValue::RightArrow)?;
            let expr = self.parse_expression()?;
            clauses.push(self.fun_clause(args, guard, expr));
            if !self.semicolon_or_newline()? || self.peek_next(TokenValue::RightBrace)? {
                break;
            }
        }
        self.expect(TokenValue::RightBrace)?;
        Ok(self.lambda_expression(clauses))
    }


    pub(super) fn parse_implicit_lambda_expression(&mut self) -> Result<Expression> {
        self.expect(TokenValue::LeftBrace)?;
        let expr = self.parse_expression()?;
        self.expect(TokenValue::RightBrace)?;
        let mut visitor = UsedImplicits::new();
        expr.visit(&mut visitor);
        let mut vars : Vec<Name> = visitor.vars.into_iter().collect();
        vars.sort();
        let mut pats: Vec<Pattern> = vars.into_iter().map(|n| self.var_pattern(n)).collect();
        if pats.is_empty() {
            pats.push(self.wildcard_pattern());
        }
        let clause = self.fun_clause(pats, None, expr);
        Ok(self.lambda_expression(vec![clause]))
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

    pub(super) fn literal_expression(&mut self, lit: Literal) -> Expression {
        self.counter += 1;
        Expression {
            id: self.counter,
            expr: ExpressionKind::Literal(lit)
        }
    }

    pub(super) fn var_expression(&mut self, var: Name) -> Expression {
        self.counter += 1;
        Expression {
            id: self.counter,
            expr: ExpressionKind::Var(var)
        }
    }

    pub(super) fn array_expression(&mut self, vals: Vec<Expression>) -> Expression {
        self.counter += 1;
        Expression {
            id: self.counter,
            expr: ExpressionKind::Array(vals)
        }
    }

    pub(super) fn app_expression(&mut self, f: Expression, arg: Expression) -> Expression {
        self.counter += 1;
        Expression {
            id: self.counter,
            expr: ExpressionKind::App(Box::new(f), Box::new(arg))
        }
    }

    pub(super) fn lambda_expression(&mut self, clauses: Vec<FunClause>) -> Expression {
        self.counter += 1;
        Expression {
            id: self.counter,
            expr: ExpressionKind::Lambda(clauses)
        }
    }

    pub(super) fn binop_expression(&mut self, op: Operator, lhs: Expression, rhs: Expression) -> Expression {
        self.counter += 1;
        Expression {
            id: self.counter,
            expr: ExpressionKind::Binop(
                Binop {
                    op: op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs)
                }
            )
        }
    }

    pub(super) fn where_expression(&mut self, expr: Expression, bindings: Vec<Binding>) -> Expression {
        self.counter += 1;
        Expression {
            id: self.counter,
            expr: ExpressionKind::Where(Box::new(expr), bindings)
        }
    }

    pub(super) fn cond_expression(&mut self, pred: Expression, on_true: Expression, on_false: Expression) -> Expression {
        self.counter += 1;
        Expression {
            id: self.counter,
            expr: ExpressionKind::Cond(
                Cond {
                    pred: Box::new(pred),
                    on_true: Box::new(on_true),
                    on_false: Box::new(on_false)
                }
            )
        }
    }
}

#[derive(Debug)]
pub(super) struct UsedImplicits {
    pub vars : HashSet<Name>
}

impl UsedImplicits {
    pub(super) fn new() -> Self {
        UsedImplicits { vars: HashSet::new() }
    }
}

impl AstVisitor for UsedImplicits {
    fn on_expression(&mut self, expr: &Expression) -> bool {
        match &expr.expr {
            ExpressionKind::Var(name) => {
                let v = name.to_string();
                if v == "a" || v == "b" || v == "c" || v == "d" {
                    self.vars.insert(name.clone());
                }
            }
            _ => {}
        };
        true
    }
}