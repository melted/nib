use crate::{ast::{Binding, Declaration}, common::Result, parser::{lexer::TokenValue, ParserState}};


impl<'a> ParserState<'a> {
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
        if let Some(name) = self.try_parse(&mut Self::parse_name)? {
            if self.is_next(TokenValue::Equals)? {
                let rhs = self.parse_expression()?;
                Ok(self.var_binding(crate::ast::Pattern::Var(name), rhs))
            } else {
                let args = self.parse_some1(&mut Self::parse_pattern)?;
                self.expect(TokenValue::Equals)?;
                let rhs = self.parse_expression()?;
                Ok(self.fun_binding(name, args, rhs)    )
            }

        } else {
            let pat = self.parse_pattern()?;
            if self.is_next(TokenValue::Equals)? {
                let rhs = self.parse_expression()?;
                Ok(self.var_binding(pat, rhs))
            } else {
                let op = self.parse_operator()?;
                let rpat = self.parse_pattern()?;
                self.expect(TokenValue::Equals)?;
                let rhs = self.parse_expression()?;
                Ok(self.op_binding(op, pat, rpat, rhs)  )
            }
        }
    }
}