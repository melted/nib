use crate::{ast::{Binding, Declaration, ExpressionNode, FunBinding, Module, Name, OpBinding, OpClause, Operator, PatternNode, Pattern, Use, VarBinding}, common::Result, parser::{lexer::TokenValue, ParserState}};


impl<'a> ParserState<'a> {
    pub(super) fn parse_declarations(&mut self) -> Result<Vec<Declaration>> {
        let mut decls = Vec::new();
        loop {
            if self.is_next(TokenValue::Eof)? {
                break;
            }
            self.parse_add_declaration(&mut decls)?
        }
        Ok(decls)
    }

    pub(super) fn parse_declaration(&mut self) -> Result<Declaration> {
        let token = self.peek_next_token()?;
        match token.value {
            TokenValue::Module => self.parse_module_declaration(),
            TokenValue::Use => self.parse_use_declaration(),
            _ => {
                let binding = self.parse_binding()?;
                Ok(Declaration::Binding(binding))
            }
        }
    }

    pub(super) fn merge_same_binding(a: &mut Declaration, b:&mut Declaration) -> bool {
        match (a, b) {
            (Declaration::Binding(Binding::FunBinding(abind)), Declaration::Binding(Binding::FunBinding(bbind))) if abind.name == bbind.name => {
                abind.clauses.append(&mut bbind.clauses);
                true
            },            
            (Declaration::Binding(Binding::OpBinding(abind)), Declaration::Binding(Binding::OpBinding(bbind))) if abind.op == bbind.op => {
                abind.clauses.append(&mut bbind.clauses);
                true
            },
            _ => false
        }
    }

    pub(super) fn parse_add_declaration(&mut self, decls:&mut Vec<Declaration>) -> Result<()> {
        let mut decl = self.parse_declaration()?;
        let Some(mut last) = decls.last_mut() else {
            decls.push(decl);
            return Ok(());
        };
        if Self::merge_same_binding(&mut last, &mut decl) {
            Ok(())
        } else {
            decls.push(decl);
            Ok(())
        }
    }

    pub(super) fn parse_module_declaration(&mut self) -> Result<Declaration> {
        self.expect(TokenValue::Module)?;
        let name = self.parse_name()?;
        Ok(self.module_declaration(name))
    }

    pub(super) fn parse_use_declaration(&mut self) -> Result<Declaration> {
        self.expect(TokenValue::Use)?;
        let name = self.parse_name()?;
        Ok(self.use_declaration(name))
    }

    pub(super) fn parse_binding(&mut self) -> Result<Binding> {
        let initial = self.parse_pattern()?;
        if self.is_next(TokenValue::Equals)? {
            let rhs = self.parse_expression()?;
            Ok(self.var_binding(initial, rhs))
        } else {
            if self.peek_operator()? {
                let op = self.parse_operator()?;
                let rpat = self.parse_pattern()?;
                let guard = if self.is_next(TokenValue::Bar)? {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.expect(TokenValue::Equals)?;
                let rhs = self.parse_expression()?;
                Ok(self.op_binding(op, initial, rpat, guard, rhs))
            } else {
                if let Pattern::Var(name) = initial.pattern {
                    let (args, guard) = self.parse_fun_args()?;
                    self.expect(TokenValue::Equals)?;
                    let rhs = self.parse_expression()?;
                    Ok(self.fun_binding(name, args, guard, rhs))
                } else {
                    self.error("Binding pattern matches neither a var, fun or operator binding")
                }
            }
        }
    }

    pub(super) fn module_declaration(&mut self, name:Name) -> Declaration {
        self.counter += 1;
        Declaration::Module(Module {
            id: self.counter,
            name: name
        })
    }

    pub(super) fn use_declaration(&mut self, name:Name) -> Declaration {
        self.counter += 1;
        Declaration::Use(Use {
            id: self.counter,
            name: name
        })
    }

    pub(super) fn var_binding(&mut self, pat:PatternNode, rhs:ExpressionNode) -> Binding {
        self.counter += 1;
        Binding::VarBinding(VarBinding {
            id: self.counter,
            lhs: pat,
            rhs: rhs
        })
    }

    pub(super) fn fun_binding(&mut self, name:Name, args:Vec<PatternNode>, guard: Option<ExpressionNode>, rhs:ExpressionNode) -> Binding {
        let clauses = vec![self.fun_clause(args, guard, rhs)];
        self.counter += 1;
        Binding::FunBinding(FunBinding {
            id: self.counter,
            name:name,
            clauses:clauses
        })
    }

    pub(super) fn op_binding(&mut self, op: Operator, lpat:PatternNode, rpat:PatternNode, guard: Option<ExpressionNode>, rhs:ExpressionNode) -> Binding {
        self.counter += 1;
        let clauses = vec![OpClause { id: self.counter, lpat,rpat, guard, body: rhs }];
        self.counter += 1;
        Binding::OpBinding(OpBinding {
            id: self.counter,
            op:op,
            clauses:clauses
        })
    }
}