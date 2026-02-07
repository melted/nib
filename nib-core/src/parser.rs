use std::iter::Peekable;
use std::mem;
use std::str::CharIndices;

use crate::ast::{ExpressionNode, Module};
use crate::common::{Error, Location, Metadata, Node, Result, SyntaxError};
use crate::parser::lexer::{Token, TokenValue};

mod declaration;
mod expression;
mod helpers;
pub(crate) mod lexer;
mod pattern;
mod tests;

pub fn parse_declarations(module: &mut Module) -> Result<()> {
    let src = mem::take(&mut module.metadata.source);
    let mut state = ParserState::new(&src, &mut module.metadata);
    let decls = state.parse_declarations()?;
    state.metadata.last_id = state.counter;
    module.declarations = decls;
    if state.metadata.errors.is_empty() {
        module.metadata.source = src;
        Ok(())
    } else {
        let count = state.metadata.errors.len();
        for err in &state.metadata.errors {
            let (line, col) = state.metadata.linecol(&err.loc);
            log::error!("{} at line {}, col {}", err.msg, line, col);
        }
        module.metadata.source = src;
        Err(Error::runtime_error(&format!("{} syntax errors", count)))
    }
}

pub fn parse_expression(code: &str) -> Result<ExpressionNode> {
    let mut meta = Metadata::empty();
    let mut state = ParserState::new(code, &mut meta);
    state.parse_expression()
}

/// Is the code a binding?
/// If there is an equal sign, and a `where` doesn't come before, it is a binding.
/// This can used to decide whether to parse the code as a declaration or an expression
/// in a repl.
pub fn is_binding(code: &str) -> Result<bool> {
    let tokens = lex(code)?;
    let tok = tokens
        .iter()
        .find(|t| t.value == TokenValue::Equals || t.value == TokenValue::Where);
    match tok.map(|t| &t.value) {
        Some(TokenValue::Equals) => Ok(true),
        _ => Ok(false),
    }
}

pub fn lex(code: &str) -> Result<Vec<Token>> {
    let mut meta = Metadata::empty();
    let mut state = ParserState::new(code, &mut meta);
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
    let mut module = Module::new(None, code);
    parse_declarations(&mut module)?;
    for d in module.declarations {
        println!("{}", d);
    }
    Ok(())
}

struct ParserState<'a> {
    metadata: &'a mut Metadata,
    src: &'a str,
    chars: Peekable<CharIndices<'a>>,
    token_start: usize,
    pos: usize,
    offset: usize,
    indent_stack: Vec<i32>,
    stashed_token: Option<Token>,
    on_new_line: bool,
    counter: Node,
}

impl<'a> ParserState<'a> {
    fn new(code: &'a str, metadata: &'a mut Metadata) -> ParserState<'a> {
        let mut state = ParserState {
            metadata,
            src: code,
            chars: code.char_indices().peekable(),
            token_start: 0,
            pos: 0,
            offset: 0,
            indent_stack: Vec::new(),
            stashed_token: None,
            on_new_line: true,
            counter: 0,
        };
        if state.src.starts_with("#!") {
            let start = state.src.find("\n").unwrap_or(state.src.len());
            state.chars = state.src[start..].char_indices().peekable();
            state.offset = start;
            state.metadata.newlines.push(start);
        }
        state
    }

    pub(self) fn new_error(&self, msg: &str) -> Error {
        Error::from(SyntaxError {
            msg: msg.to_string(),
            loc: Location::at(self.metadata.source_id, self.token_start, self.position()), // TODO: extent of AST element
        })
    }

    pub(self) fn error<T>(&self, msg: &str) -> Result<T> {
        Err(self.new_error(msg))
    }

    pub(self) fn indent(&self) -> i32 {
        for nl in self.metadata.newlines.iter().rev() {
            if self.position() > *nl {
                return (self.position() - *nl) as i32;
            }
        }
        self.position() as i32
    }

    pub(self) fn position(&self) -> usize {
        self.pos + self.offset
    }

    pub(self) fn next_position(&mut self) -> usize {
        self.peek_next_token().map_or(0, |x| x.location.start())
    }

    pub(self) fn adjust_offset(&mut self, offset: usize) {
        self.offset = offset;
        self.pos = 0;
    }
}
