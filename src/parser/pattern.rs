use crate::ast::{Literal, Name, Pattern, PatternKind};
use super::{ ParserState, lexer::TokenValue };
use crate::common::Result;

impl<'a> ParserState<'a> {
    pub(super) fn parse_pattern(&mut self) -> Result<Pattern> {
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
            TokenValue::Identifier(name) => {
                self.get_next_token()?;
                let ellipsis = self.is_next(TokenValue::Ellipsis)?;
                let name = Name::name(&name);
                if ellipsis {
                    self.ellipsis_pattern(name)
                } else {
                    self.var_pattern(name)
                }
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
                    Ok(self.alias_pattern(lhs, Name::name(&name)))
                },
                _ => {
                    self.error(&format!("Expected identifier in alias pattern, got {tok:?}"))
                }
            }
        } else {
            Ok(lhs)
        }
    }

    pub(super) fn parse_array_pattern(&mut self) -> Result<Pattern> {
        self.get_next_token()?;
        let pats = self.parse_separated_by(&mut Self::parse_pattern, TokenValue::Comma)?;
        self.expect(TokenValue::RightBracket)?;
        Ok(self.array_pattern(pats))
    }

    pub(super) fn parse_custom_pattern(&mut self) -> Result<Pattern> {
        self.expect(TokenValue::LeftParen)?;
        match self.peek_next_token()?.value {
            TokenValue::RightParen => {
                self.expect(TokenValue::RightParen)?;
                Ok(self.literal_pattern(Literal::Nil))
            },
            TokenValue::Identifier(_) => {
                let name = self.parse_name()?;
                let pats = self.parse_some(&mut Self::parse_pattern)?;
                self.expect(TokenValue::RightParen)?;
                Ok(self.custom_pattern(name , pats))
            }
            _ => {
                self.error("Custom pattern must start with a name")
            }
        }
    }

    pub(super) fn alias_pattern(&mut self, pattern:Pattern, alias:Name) -> Pattern {
        self.counter += 1;
        Pattern { id: self.counter, pattern: PatternKind::Alias(Box::new(pattern), alias) }
    }

    pub(super) fn array_pattern(&mut self, patterns:Vec<Pattern>) -> Pattern {
        self.counter += 1;
        Pattern { id: self.counter, pattern: PatternKind::Array(patterns) }
    }

    pub(super) fn custom_pattern(&mut self, name:Name, patterns:Vec<Pattern>) -> Pattern {
        self.counter += 1;
        Pattern { id: self.counter, pattern: PatternKind::Custom(name, patterns) }
    }

    pub(super) fn var_pattern(&mut self, name:Name) -> Pattern {
        self.counter += 1;
        Pattern { id: self.counter, pattern: PatternKind::Var(name) }
    }

    pub(super) fn literal_pattern(&mut self, lit:Literal) -> Pattern {
        self.counter += 1;
        Pattern { id: self.counter, pattern: PatternKind::Literal(lit) }
    }

    pub(super) fn ellipsis_pattern(&mut self, name:Name) -> Pattern {
        self.counter += 1;
        Pattern { id: self.counter, pattern: PatternKind::Ellipsis(name) }
    }

    pub(super) fn wildcard_pattern(&mut self) -> Pattern {
        self.counter += 1;
        Pattern { id: self.counter, pattern: PatternKind::Wildcard }
    }
}