use std::iter::Peekable;
use std::str::CharIndices;

use crate::common::{Error, Location, Result};
use crate::ast::{Declaration, Expression, Metadata, Node};
use crate::parser::lexer::{Token, TokenValue};

mod declaration;
mod expression;
mod helpers;
pub(crate) mod lexer;
mod pattern;
mod tests;


pub fn parse_declarations(code: &str) -> Result<Vec<Declaration>> {
    let mut state = ParserState::new(code);
    let decls = state.parse_declarations()?;
    // TODO: create a better return value. One that allows for incremental parsing
    Ok(decls)
}


pub fn parse_expression(code: &str) -> Result<Expression> {
    let mut state = ParserState::new(code);
    state.parse_expression()
}

pub fn lex(code: &str) -> Result<Vec<Token>> {
    let mut state = ParserState::new(code);
    let mut tokens = Vec::new();
    loop {
        let tok = state.get_next_token()?;
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

pub fn dump_prog(code: &str) -> Result<()> {
    let decls = parse_declarations(code)?;
    for d in decls {
        println!("{:?}", d);
    }
    Ok(())
}

struct ParserState<'a> {
    metadata: Metadata,
    src: &'a str,
    chars: Peekable<CharIndices<'a>>,
    token_start: usize,
    pos: usize,
    next_token: usize,
    indent_stack: Vec<i32>,
    tokens: Vec<Token>,
    on_new_line: bool,
    counter: Node
}

impl<'a> ParserState<'a> {
    fn new(code: &'a str) -> ParserState<'a> {
        ParserState { metadata: Metadata::new(None),
            src: code,
            chars: code.char_indices().peekable(),
            token_start: 0,
            pos: 0,
            next_token: 0,
            indent_stack: Vec::new(),
            tokens: Vec::new(),
            on_new_line: true,
            counter: 0
        }
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

