use std::any::Any;

use crate::{ast::{Binding, Declaration, ExpressionNode, FunBinding, Module, Name, OpBinding, OpClause, Operator, Pattern, PatternNode, Use, VarBinding}, common::{Location, Result}, parser::{lexer::TokenValue, ParserState}};


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
        if let Some(mut last) = decls.last_mut() {
            if Self::merge_same_binding(&mut last, &mut decl) {
                return Ok(());
            }
        }
        decls.push(decl);
        Ok(())
    }

    pub(super) fn parse_module_declaration(&mut self) -> Result<Declaration> {
        let start = self.next_position();
        self.expect(TokenValue::Module)?;
        let name = self.parse_name()?;
        let pos = self.position();
        // HACK: assumes id from counter
        self.metadata.locations.insert(self.counter + 1, Location::at(start, pos));
        Ok(self.module_declaration(name))
    }

    pub(super) fn parse_use_declaration(&mut self) -> Result<Declaration> {
        let start = self.next_position();
        self.expect(TokenValue::Use)?;
        let name = self.parse_name()?;
        let pos = self.position();
        // HACK: assumes id from counter
        self.metadata.locations.insert(self.counter + 1, Location::at(start, pos));
        Ok(self.use_declaration(name))
    }

    pub(super) fn parse_binding(&mut self) -> Result<Binding> {
        let start = self.next_position();
        let initial = self.parse_pattern()?;
        if self.is_next(TokenValue::Equals)? {
            let rhs = self.parse_expression()?;
            let bind = self.var_binding(initial, rhs);
            let pos = self.position();
            self.metadata.locations.insert(bind.id, Location::at(start, pos));
            Ok(Binding::VarBinding(bind))
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
                let bind = self.op_binding(op, initial, rpat, guard, rhs);
                let pos = self.position();
                self.metadata.locations.insert(bind.clauses[0].id, Location::at(start, pos));
                self.metadata.locations.insert(bind.id, Location::at(start, pos));
                Ok(Binding::OpBinding(bind))
            } else {
                if let Pattern::Var(name) = initial.pattern {
                    let (args, guard) = self.parse_fun_args()?;
                    self.expect(TokenValue::Equals)?;
                    let rhs = self.parse_expression()?;
                    let bind = self.fun_binding(name, args, guard, rhs);
                    let pos = self.position();
                    self.metadata.locations.insert(bind.clauses[0].id, Location::at(start, pos));
                    self.metadata.locations.insert(bind.id, Location::at(start, pos));
                    Ok(Binding::FunBinding(bind))
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

    pub(super) fn var_binding(&mut self, pat:PatternNode, rhs:ExpressionNode) -> VarBinding {
        self.counter += 1;
        VarBinding {
            id: self.counter,
            lhs: pat,
            rhs: rhs
        }
    }

    pub(super) fn fun_binding(&mut self, name:Name, args:Vec<PatternNode>, guard: Option<ExpressionNode>, rhs:ExpressionNode) -> FunBinding {
        let clauses = vec![self.fun_clause(args, guard, rhs)];
        self.counter += 1;
        FunBinding {
            id: self.counter,
            name:name,
            clauses:clauses
        }
    }

    pub(super) fn op_binding(&mut self, op: Operator, lpat:PatternNode, rpat:PatternNode, guard: Option<ExpressionNode>, rhs:ExpressionNode) -> OpBinding {
        self.counter += 1;
        let clauses = vec![OpClause { id: self.counter, lpat,rpat, guard, body: rhs }];
        self.counter += 1;
        OpBinding {
            id: self.counter,
            op:op,
            clauses:clauses
        }
    }
}