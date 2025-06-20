use crate::common::{Error, Result};

use super::{ParserState, lexer::TokenValue};


/// General help functions for parsing
impl<'a> ParserState<'a> {
        pub(super) fn expect(&mut self, t: TokenValue) -> Result<()> {
        let next = self.get_next_token()?;
        if t != next.value {
            Err(self.error(&format!("expected {:?}, got {:?}", t, next.value)))
        } else {
            Ok(())
        }
    }

    pub(super) fn is_next(&mut self, t: TokenValue) -> Result<bool> {
        let next = self.peek_next_token()?;
        if t == next.value {
            self.get_next_token()?; // Swallow token
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(super) fn is_next_identifier(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Identifier(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_operator(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Operator(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_symbol(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Symbol(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_integer(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Integer(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_float(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Float(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_char(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::Char(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn is_next_string(&mut self) -> Result<bool> {
        let next = self.peek_next_token()?;
        match next.value {
            TokenValue::String(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub(super) fn peek_next(&mut self, t: TokenValue) -> Result<bool> {
        let next = self.peek_next_token()?;
        if t == next.value {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(super) fn optional_token(&mut self, t: TokenValue) -> Result<()> {
        self.is_next(t)?;
        Ok(())
    }

    pub(super) fn parse_some<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut output = Vec::new();
        while let Some(res) = self.try_parse(inner_parser)? {
            output.push(res);
        }
        Ok(output)
    }

    pub(super) fn parse_some1<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut output = Vec::new();
        let first = inner_parser(self)?;
        output.push(first);
        while let Some(res) = self.try_parse(inner_parser)? {
            output.push(res);
        }
        Ok(output)
    }

    pub(super) fn parse_separated_by<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> Result<T>,
        separator: TokenValue,
    ) -> Result<Vec<T>> {
        let mut output = Vec::new();
        output.push(inner_parser(self)?);
        while self.is_next(separator.clone())? {
            output.push(inner_parser(self)?);
        }
        Ok(output)
    }

    pub(super) fn try_parse<T>(
        &mut self,
        inner_parser: &mut impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Option<T>> {
        let start = self.token_start;
        match inner_parser(self) {
            Ok(res) => Ok(Some(res)),
            Err(_err) => {
                self.token_start = start;
                Ok(None)
            }
        }
    }

}