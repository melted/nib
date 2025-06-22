use std::iter::Peekable;
use std::str::CharIndices;

use crate::common::{Error, Location, Result};
use crate::ast::{Metadata, Node};
use crate::parser::lexer::{Token, TokenValue};

mod declaration;
mod expression;
mod helpers;
pub(crate) mod lexer;
mod pattern;
mod tests;



pub fn parse(code: &str) -> Result<()> {
    let state = ParserState::new(code);
    todo!()
}

pub fn lex(code: &str) -> Result<Vec<Token>> {
    let mut state = ParserState::new(code);
    let mut tokens = Vec::new();
    loop {
        let tok = state.get_next_token()?;
        println!("{:?}", &tok);
        if tok == TokenValue::Eof {
            tokens.push(tok);
            break;
        }
        tokens.push(tok);
    }
    Ok(tokens)
}

pub fn dump_lex(code: &str) -> Result<()> {
    let tokens = lex(code)?;
    for t in tokens {
        println!("{:?}", t);
    }
    Ok(())
}

struct ParserState<'a> {
    metadata: Metadata,
    src: &'a str,
    chars: Peekable<CharIndices<'a>>,
    token_start: usize,
    pos: usize,
    stashed_token: Option<Token>,
    indent_stack: Vec<i32>,
    counter: Node
}

impl<'a> ParserState<'a> {
    fn new(code: &'a str) -> ParserState<'a> {
        ParserState { metadata: Metadata::new(None),
            src: code,
            chars: code.char_indices().peekable(),
            token_start: 0,
            pos: 0,
            stashed_token: None,
            indent_stack: Vec::new(),
            counter: 0 }
    }

    pub(self) fn new_error(&self, msg: &str) -> Error {
        Error::Syntax {
            msg: msg.to_string(),
            loc: Location::Offset {
                start: self.token_start,
                end: self.pos,
            },
        }
    }

    pub(self) fn error<T>(&self, msg: &str) -> Result<T> {
        Err(self.new_error(msg))
    }

    pub(self) fn indent(&self) -> i32 {
        let last_newline = self.metadata.newlines.last().unwrap_or(&0);
        (self.pos - last_newline) as i32
    }
}

