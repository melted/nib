use std::collections::HashSet;

use crate::ast::{AstVisitor, Binding, Binop, Cond, ExpressionNode, Expression, FunClause, Literal, Operator, PatternNode};
use crate::common::{Location, Name, Result};
use crate::parser::lexer::TokenValue;
use super::ParserState;

impl<'a> ParserState<'a> {
    pub(super) fn parse_expression(&mut self) -> Result<ExpressionNode> {
        self.parse_inner_expression(0)
    }

    pub(super) fn parse_inner_expression(&mut self, min_pred:i32) -> Result<ExpressionNode> {
        let indent = self.indent();
        let last = *self.indent_stack.last().unwrap_or(&0);
        let start = self.next_position();
        if indent < last {
            return self.error("Sub-expression violates layout by being to the left of parent expression");
        }
        self.indent_stack.push(indent);
        let mut lhs = match self.parse_left_expression() {
            Ok(exp) => {
                let pos = self.position();
                self.metadata.locations.insert(exp.id, Location::at(start, pos));
                exp
            }, 
            err@Err(_) => {
                self.indent_stack.pop();
                return err;
            }
        };

        loop {
            let tok = match self.peek_next_token() {
                Ok(t) => t,
                Err(err) => {
                    self.indent_stack.pop();
                    return Err(err);
                }
            };
            if self.next_indent() <= indent {
                self.indent_stack.pop();
                return Ok(lhs);
            }
            let result = match tok.value {
                TokenValue::Where => { 
                    if min_pred < 2 {
                        self.parse_where_expression(lhs)
                    } else {
                        break;
                    }
                },
                TokenValue::Operator(op) => {
                    if min_pred < 6 {
                        self.get_next_token()?;
                        self.parse_binop_expression(lhs, Operator::Plain(op))
                    } else {
                        break;
                    }
                },
                TokenValue::FatRightArrow => {
                    if min_pred < 4 {
                        self.parse_cond_expression(lhs)
                    } else {
                        break;
                    }
                },
                TokenValue::Period => self.parse_projection_expression(lhs),
                TokenValue::Identifier(_) => {
                        if min_pred < 9 {
                            let rhs = self.parse_inner_expression(9)?;
                            Ok(self.app_expression(lhs, rhs))
                        } else {
                            break;
                        }
                },
                TokenValue::Eof => break,
                TokenValue::LeftBrace | TokenValue::LeftParen | TokenValue::LeftBracket if min_pred < 9 => {
                    let expr = self.parse_left_expression()?;
                    Ok(self.app_expression(lhs, expr))
                },
                _ if tok.value.is_literal() && min_pred < 9 => {
                    let expr = self.parse_left_expression()?;
                    Ok(self.app_expression(lhs, expr))
                }, 
                _ => {
                    break;
                }
            };
            match result {
                Err(_) => {
                    self.indent_stack.pop();
                    return result;
                }
                Ok(exp) => {
                    let pos = self.position();
                    self.metadata.locations.insert(exp.id, Location::at(start, pos));
                    lhs = exp;
                }
            }
        };
        self.indent_stack.pop();
        Ok(lhs)
    }

    pub(super) fn parse_left_expression(&mut self) -> Result<ExpressionNode> {
        let tok = self.peek_next_token()?;
        match tok.value {
            x if x.is_literal() => {
                let lit = self.parse_literal()?;
                Ok(self.literal_expression(lit))
            },
            TokenValue::LeftBrace => self.parse_lambda_expression(),
            TokenValue::LeftBracket => self.parse_array_expression(),
            TokenValue::LeftParen => self.parse_paren_expression(),
            TokenValue::Identifier(name) => {
                self.get_next_token()?;
                let name = Name::name(&name);
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

    pub(super) fn parse_where_expression(&mut self, lhs:ExpressionNode) -> Result<ExpressionNode> {
        self.expect(TokenValue::Where)?;
        let mut bindings = Vec::new();
        let indent = *self.indent_stack.last().unwrap_or(&0);
        loop {
            if self.next_indent() <= indent {
                break;
            }
            let mut binding = self.parse_binding()?;
            if let Some(mut last) = bindings.last_mut() {
                if !self.merge_same_binding(&mut last, &mut binding) {
                    bindings.push(binding);
                }
            } else {
                bindings.push(binding);
            }
            if !self.semicolon_or_newline()? {
                break;
            }
        }
        Ok(self.where_expression(lhs, bindings))
    }

    pub(super) fn parse_binop_expression(&mut self, lhs:ExpressionNode, op:Operator) -> Result<ExpressionNode> {
        let rhs = self.parse_inner_expression(5)?;
        Ok(self.binop_expression(op, lhs, rhs))
    }

    pub(super) fn parse_cond_expression(&mut self, lhs:ExpressionNode) -> Result<ExpressionNode> {
        self.expect(TokenValue::FatRightArrow)?;
        let on_true = self.parse_inner_expression(3)?;
        if self.semicolon_or_newline()? {
            let on_false = self.parse_inner_expression(3)?;
            Ok(self.cond_expression(lhs, on_true, on_false))
        } else {
            self.error("Missing else expression in => expression")
        }
    }

    pub(super) fn parse_fun_args(&mut self) -> Result<(Vec<PatternNode>, Option<ExpressionNode>)> {
        let mut args = Vec::new();
        args.push(self.parse_pattern()?);
        loop {
            let t = self.peek_next_token()?.value;
            match t {
                TokenValue::Bar | TokenValue::Equals | TokenValue::RightArrow => break,
                _ => args.push(self.parse_pattern()?)
            }
        }
        let guard = if self.is_next(TokenValue::Bar)? {
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok((args, guard))
    }

    pub(super) fn parse_lambda_expression(&mut self) -> Result<ExpressionNode> {
        if let Some(exp) = self.try_parse(&mut Self::parse_full_lambda_expression)? {
            Ok(exp)
        } else {
            self.parse_implicit_lambda_expression()
        }
    }

    pub(super) fn parse_full_lambda_expression(&mut self) -> Result<ExpressionNode> {
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


    pub(super) fn parse_implicit_lambda_expression(&mut self) -> Result<ExpressionNode> {
        self.expect(TokenValue::LeftBrace)?;
        let expr = self.parse_expression()?;
        self.expect(TokenValue::RightBrace)?;
        let mut visitor = UsedImplicits::new();
        expr.visit(&mut visitor);
        let mut vars : Vec<Name> = visitor.vars.into_iter().collect();
        vars.sort();
        let mut pats: Vec<PatternNode> = vars.into_iter().map(|n| self.var_pattern(n)).collect();
        if pats.is_empty() {
            pats.push(self.wildcard_pattern());
        }
        let clause = self.fun_clause(pats, None, expr);
        Ok(self.lambda_expression(vec![clause]))
    }

    pub(super) fn parse_array_expression(&mut self) -> Result<ExpressionNode> {
        self.expect(TokenValue::LeftBracket)?;
        let mut exprs = Vec::new();
        exprs.push(self.parse_expression()?);
        while self.is_next(TokenValue::Comma)? {
            exprs.push(self.parse_expression()?);
        }
        self.expect(TokenValue::RightBracket)?;
        Ok(self.array_expression(exprs))
    }

    pub(super) fn parse_projection_expression(&mut self, lhs: ExpressionNode) -> Result<ExpressionNode> {
        let mut projs = vec![lhs];
        while self.is_next(TokenValue::Period)? {
            let next = match self.peek_next_token()?.value {
                TokenValue::Identifier(id) => {
                    self.get_next_token()?;
                    self.literal_expression(Literal::Symbol(id))
                },
                _ => self.parse_inner_expression(10)?
            };
            projs.push(next);
        }
        Ok(self.projection_expression(projs))
    }

    pub(super) fn parse_paren_expression(&mut self) -> Result<ExpressionNode> {
        self.expect(TokenValue::LeftParen)?;
        let exp = self.parse_inner_expression(0)?;
        self.expect(TokenValue::RightParen)?;
        Ok(exp)
    }

    pub(super) fn literal_expression(&mut self, lit: Literal) -> ExpressionNode {
        self.counter += 1;
        ExpressionNode {
            id: self.counter,
            expr: Expression::Literal(lit)
        }
    }

    pub(super) fn var_expression(&mut self, var: Name) -> ExpressionNode {
        self.counter += 1;
        ExpressionNode {
            id: self.counter,
            expr: Expression::Var(var)
        }
    }

    pub(super) fn array_expression(&mut self, vals: Vec<ExpressionNode>) -> ExpressionNode {
        self.counter += 1;
        ExpressionNode {
            id: self.counter,
            expr: Expression::Array(vals)
        }
    }

    pub(super) fn app_expression(&mut self, mut f:  ExpressionNode, arg: ExpressionNode) -> ExpressionNode {
        self.counter += 1;
        match f.expr {
            Expression::App(ref mut args) => {
                args.push(arg);
                f
            },
            _ => {
                ExpressionNode {
                    id: self.counter,
                    expr: Expression::App(vec![f, arg])
                }
            }
        }
    }

    pub(super) fn lambda_expression(&mut self, clauses: Vec<FunClause>) -> ExpressionNode {
        self.counter += 1;
        ExpressionNode {
            id: self.counter,
            expr: Expression::Lambda(clauses)
        }
    }

    pub(super) fn binop_expression(&mut self, op: Operator, lhs: ExpressionNode, rhs: ExpressionNode) -> ExpressionNode {
        self.counter += 1;
        ExpressionNode {
            id: self.counter,
            expr: Expression::Binop(
                Binop {
                    op: op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs)
                }
            )
        }
    }

    pub(super) fn where_expression(&mut self, expr: ExpressionNode, bindings: Vec<Binding>) -> ExpressionNode {
        self.counter += 1;
        ExpressionNode {
            id: self.counter,
            expr: Expression::Where(Box::new(expr), bindings)
        }
    }

    pub(super) fn projection_expression(&mut self, exprs: Vec<ExpressionNode>) -> ExpressionNode {
        self.counter += 1;
        ExpressionNode {
            id: self.counter,
            expr: Expression::Projection(exprs)
        }
    }

    pub(super) fn cond_expression(&mut self, pred: ExpressionNode, on_true: ExpressionNode, on_false: ExpressionNode) -> ExpressionNode {
        self.counter += 1;
        ExpressionNode {
            id: self.counter,
            expr: Expression::Cond(
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
    fn on_expression(&mut self, expr: &ExpressionNode) -> bool {
        match &expr.expr {
            Expression::Var(name) => {
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