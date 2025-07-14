use crate::{ast::{Literal, Pattern, PatternNode}, common::Location};
use super::{ ParserState, lexer::TokenValue };
use crate::common::{ Name, Result };

impl<'a> ParserState<'a> {
    pub(super) fn parse_pattern(&mut self) -> Result<PatternNode> {
        let start = self.next_position();
        let tok = self.peek_next_token()?;
        let lhs = match tok.value {
            TokenValue::Underscore => {
                self.get_next_token()?;
                self.wildcard_pattern()
            },
            x if x.is_literal() => {
                let lit = self.parse_literal()?;
                self.literal_pattern(lit)
            },
            TokenValue::Identifier(_) => {
                let name = self.parse_qualified_name()?;
                self.var_pattern(name)
            },
            TokenValue::Ellipsis(name) => {
                self.get_next_token()?;
                self.ellipsis_pattern(name.map(|n| Name::name(&n)))
            },
            TokenValue::LeftBracket => self.parse_array_pattern()?,
            TokenValue::LeftParen => self.parse_custom_pattern()?,
            _ => {
                return self.error(&format!("Invalid token {tok:?} at start of pattern"));
            }
        };

        if self.is_next(TokenValue::As)? {
            let tok = self.get_next_token()?;
            match tok.value {
                TokenValue::Identifier(name) => {
                    let pat = self.alias_pattern(lhs, Name::name(&name));
                    let pos = self.position();
                    self.metadata.locations.insert(pat.id, Location::at(start, pos));
                    Ok(pat)
                },
                _ => {
                    self.error(&format!("Expected identifier in alias pattern, got {tok:?}"))
                }
            }
        } else {
            let pos = self.position();
            self.metadata.locations.insert(lhs.id, Location::at(start, pos));
            Ok(lhs)
        }
    }

    pub(super) fn parse_array_pattern(&mut self) -> Result<PatternNode> {
        self.get_next_token()?;
        let mut pats = Vec::new();
        if !self.peek_next(TokenValue::RightBracket)? {
            pats.push(self.parse_pattern()?);
            while self.is_next(TokenValue::Comma)? {
                pats.push(self.parse_pattern()?);
            }
        }
        self.expect(TokenValue::RightBracket)?;
        Ok(self.array_pattern(pats))
    }

    pub(super) fn parse_custom_pattern(&mut self) -> Result<PatternNode> {
        self.expect(TokenValue::LeftParen)?;
        match self.peek_next_token()?.value {
            TokenValue::Identifier(_) => {
                let name = self.parse_qualified_name()?;
                let mut pats = Vec::new();
                while self.peek_next_token()?.value != TokenValue::RightParen {
                    pats.push(self.parse_pattern()?);
                }
                self.expect(TokenValue::RightParen)?;
                Ok(self.custom_pattern(name , pats))
            }
            _ => {
                self.error("Custom pattern must start with a name")
            }
        }
    }

    pub(super) fn alias_pattern(&mut self, pattern:PatternNode, alias:Name) -> PatternNode {
        self.counter += 1;
        PatternNode { id: self.counter, pattern: Pattern::Alias(Box::new(pattern), alias) }
    }

    pub(super) fn array_pattern(&mut self, patterns:Vec<PatternNode>) -> PatternNode {
        self.counter += 1;
        PatternNode { id: self.counter, pattern: Pattern::Array(patterns) }
    }

    pub(super) fn custom_pattern(&mut self, name:Name, patterns:Vec<PatternNode>) -> PatternNode {
        self.counter += 1;
        PatternNode { id: self.counter, pattern: Pattern::Custom(name, patterns) }
    }

    pub(super) fn var_pattern(&mut self, name:Name) -> PatternNode {
        self.counter += 1;
        PatternNode { id: self.counter, pattern: Pattern::Var(name) }
    }

    pub(super) fn literal_pattern(&mut self, lit:Literal) -> PatternNode {
        self.counter += 1;
        PatternNode { id: self.counter, pattern: Pattern::Literal(lit) }
    }

    pub(super) fn ellipsis_pattern(&mut self, name:Option<Name>) -> PatternNode {
        self.counter += 1;
        PatternNode { id: self.counter, pattern: Pattern::Ellipsis(name) }
    }

    pub(super) fn wildcard_pattern(&mut self) -> PatternNode {
        self.counter += 1;
        PatternNode { id: self.counter, pattern: Pattern::Wildcard }
    }
}