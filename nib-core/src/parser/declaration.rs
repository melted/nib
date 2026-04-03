use crate::{
    ast::{
        Binding, Declaration, ExpressionNode, FunBinding, OpBinding, OpClause, Operator, Pattern,
        PatternNode, VarBinding,
    },
    common::{Error, Location, Name, Node, Result},
    parser::{
        ParserState,
        lexer::{Token, TokenValue},
    },
};

impl<'a> ParserState<'a> {
    pub(super) fn parse_declarations(&mut self) -> Result<Vec<Declaration>> {
        let mut decls = Vec::new();
        loop {
            if let Ok(true) = self.is_next(TokenValue::Eof) {
                break;
            }
            match self.parse_add_declaration(&mut decls) {
                Ok(_) => {}
                Err(Error::Syntax { err: syntax_error }) => {
                    self.metadata.errors.push(syntax_error);
                    // Now recover
                    self.resync();
                    loop {
                        if let Some(res) = self.try_parse(&mut Self::parse_declaration)? {
                            decls.push(res);
                            break;
                        }
                        let tok = self.get_next_token()?;
                        if tok.value == TokenValue::Eof {
                            break;
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }
        Ok(decls)
    }

    pub(super) fn parse_declaration(&mut self) -> Result<Declaration> {
        let binding = self.parse_binding()?;
        Ok(Declaration::Binding(binding))
    }

    fn merge_locations(&mut self, id_a: Node, id_b: Node) {
        if let Some(bloc) = self.metadata.locations.remove(&id_b) {
            self.metadata
                .locations
                .entry(id_a)
                .and_modify(|e| e.end = bloc.end);
        }
    }

    pub(super) fn merge_same_declaration(
        &mut self,
        a: &mut Declaration,
        b: &mut Declaration,
    ) -> bool {
        match (a, b) {
            (Declaration::Binding(ab), Declaration::Binding(bb)) => self.merge_same_binding(ab, bb),
        }
    }

    pub(super) fn merge_same_binding(&mut self, a: &mut Binding, b: &mut Binding) -> bool {
        match (a, b) {
            (Binding::FunBinding(abind), Binding::FunBinding(bbind))
                if abind.name == bbind.name =>
            {
                abind.clauses.append(&mut bbind.clauses);
                self.merge_locations(abind.id, bbind.id);
                true
            }
            (Binding::OpBinding(abind), Binding::OpBinding(bbind)) if abind.op == bbind.op => {
                abind.clauses.append(&mut bbind.clauses);
                self.merge_locations(abind.id, bbind.id);
                true
            }
            _ => false,
        }
    }

    pub(super) fn parse_add_declaration(&mut self, decls: &mut Vec<Declaration>) -> Result<()> {
        let mut decl = self.parse_declaration()?;
        if let Some(last) = decls.last_mut()
            && self.merge_same_declaration(last, &mut decl)
        {
            return Ok(());
        }
        decls.push(decl);
        Ok(())
    }

    pub(super) fn parse_binding(&mut self) -> Result<Binding> {
        let start = self.next_position();
        let initial = self.parse_pattern()?;
        if self.is_next(TokenValue::Equals)? {
            let rhs = self.parse_expression()?;
            let bind = self.var_binding(initial, rhs);
            let pos = self.position();
            self.metadata
                .locations
                .insert(bind.id, Location::at(self.metadata.source_id, start, pos));
            Ok(Binding::VarBinding(bind))
        } else if self.peek_operator()? {
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
            self.metadata.locations.insert(
                bind.clauses[0].id,
                Location::at(self.metadata.source_id, start, pos),
            );
            self.metadata
                .locations
                .insert(bind.id, Location::at(self.metadata.source_id, start, pos));
            Ok(Binding::OpBinding(bind))
        } else if let Pattern::Var(name) = initial.pattern {
            let (args, guard) = self.parse_fun_args()?;
            self.expect(TokenValue::Equals)?;
            let rhs = self.parse_expression()?;
            let bind = self.fun_binding(name, args, guard, rhs);
            let pos = self.position();
            self.metadata.locations.insert(
                bind.clauses[0].id,
                Location::at(self.metadata.source_id, start, pos),
            );
            self.metadata
                .locations
                .insert(bind.id, Location::at(self.metadata.source_id, start, pos));
            Ok(Binding::FunBinding(bind))
        } else {
            self.error("Binding pattern matches neither a var, fun or operator binding")
        }
    }

    pub(super) fn var_binding(&mut self, pat: PatternNode, rhs: ExpressionNode) -> VarBinding {
        self.counter += 1;
        VarBinding {
            id: self.counter,
            lhs: pat,
            rhs,
        }
    }

    pub(super) fn fun_binding(
        &mut self,
        name: Name,
        args: Vec<PatternNode>,
        guard: Option<ExpressionNode>,
        rhs: ExpressionNode,
    ) -> FunBinding {
        let clauses = vec![self.fun_clause(args, guard, rhs)];
        self.counter += 1;
        FunBinding {
            id: self.counter,
            name,
            clauses,
        }
    }

    pub(super) fn op_binding(
        &mut self,
        op: Operator,
        lpat: PatternNode,
        rpat: PatternNode,
        guard: Option<ExpressionNode>,
        rhs: ExpressionNode,
    ) -> OpBinding {
        self.counter += 1;
        let clauses = vec![OpClause {
            id: self.counter,
            lpat,
            rpat,
            guard,
            body: rhs,
        }];
        self.counter += 1;
        OpBinding {
            id: self.counter,
            op,
            clauses,
        }
    }

    pub(super) fn resync(&mut self) {
        if let Some(&indent) = self.indent_stack.first() {
            while self.next_indent() >= indent {
                let _ = self
                    .get_next_token()
                    .unwrap_or(Token::from(TokenValue::Eof));
            }
            self.indent_stack.clear();
        }
    }
}
