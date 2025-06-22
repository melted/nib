use crate::{ast::{Literal, Name, Pattern}, parser::helpers::NameOrOperator};
use super::{ ParserState, lexer::TokenValue };
use crate::common::Result;


impl<'a> ParserState<'a> {
    pub(super) fn parse_pattern(&mut self) -> Result<Pattern> {
        let tok = self.peek_next_token()?;
        let lhs = match tok.value {
            TokenValue::Underscore => {
                self.get_next_token()?;
                Pattern::Wildcard
            },
            TokenValue::Char(_) | TokenValue::String(_) |
            TokenValue::Float(_) | TokenValue::Integer(_) |
            TokenValue::Symbol(_) | TokenValue::HashLeftBracket => {
                let lit = self.parse_literal()?;
                Pattern::Literal(lit)
            },
            TokenValue::Identifier(name) => {
                self.get_next_token()?;
                let ellipsis = self.is_next(TokenValue::Ellipsis)?;
                if ellipsis {
                    Pattern::Ellipsis(Name::Plain(name))
                } else {
                    Pattern::Var(Name::Plain(name))
                }
            },
            TokenValue::LeftBracket => self.parse_array_pattern()?,
            TokenValue::LeftParen => self.parse_custom_pattern()?,
            _ => {
                return self.error(&format!("Invalid token {tok:?} at start of pattern"));
            }
        };

        if self.is_next(TokenValue::As)? {
            self.get_next_token()?;
            let tok = self.get_next_token()?;
            match tok.value {
                TokenValue::Identifier(name) => {
                    Ok(Pattern::Alias(Box::new(lhs), Name::Plain(name)))
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
        Ok(Pattern::Array(pats))
    }

    pub(super) fn parse_custom_pattern(&mut self) -> Result<Pattern> {
        self.get_next_token()?;
        match self.peek_next_token()?.value {
            TokenValue::RightParen => Ok(Pattern::Literal(Literal::Nil)),
            TokenValue::Identifier(_) => {
                let NameOrOperator::Name(n) = self.parse_name()? else {
                    return self.error("Custom pattern must start with a name");
                };
                let pats = self.parse_some(&mut Self::parse_pattern)?;
                Ok(Pattern::Custom(n, pats))
            }
            _ => {
                self.error("Custom pattern must start with a name")
            }
        }
    }
}