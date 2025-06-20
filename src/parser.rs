use std::iter::Peekable;
use std::str::CharIndices;

use crate::common::{Error, Location, Result};
use crate::ast::{Metadata, Node};
use crate::parser::lexer::{Token, TokenValue};

mod lexer;


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
    counter: Node
}

impl<'a> ParserState<'a> {
    fn new(code: &'a str) -> ParserState<'a> {
        ParserState { metadata: Metadata::new(None),
            src: code,
            chars: code.char_indices().peekable(),
            token_start: 0,
            pos: 0,
            counter: 0 }
    }

    pub(self) fn error(&self, msg: &str) -> Error {
        Error::Syntax {
            msg: msg.to_string(),
            loc: Location::Offset {
                start: self.token_start,
                end: self.pos,
            },
        }
    }
}

