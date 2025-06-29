use unicode_categories::UnicodeCategories;

use crate::common::Error;
use crate::common::Location;
use crate::common::Result;

impl<'a> super::ParserState<'a> {
    pub(super) fn peek_next_token(&mut self) -> Result<Token> {
        let tok = self.get_next_token()?;
        if tok.value != TokenValue::Eof {
            self.stashed_token = Some(tok.clone());
        }
        self.pos = tok.location.start - self.offset;
        self.token_start = self.position();
        Ok(tok)
    }

    pub(super) fn get_next_token(&mut self) -> Result<Token> {
        if let Some(tok) = self.stashed_token.take() {
            return Ok(tok)
        }
        while let Some((p, c)) = self.chars.peek() {
            let ch = *c;
            self.pos = *p;
            self.token_start = self.position();
            let tok = match ch {
                '\n' => {
                    let last = *self.metadata.newlines.last().unwrap_or(&0);
                    if self.position() > last {
                        self.metadata.newlines.push(self.position());
                    }
                    self.on_new_line = true;
                    self.next();
                    None
                },
                _ if ch.is_whitespace() => {
                    self.next();
                    None
                },
                '(' => {
                    self.next();
                    if let Some((_, c)) = self.chars.peek() {
                        let c = *c;
                        if !self.check_prefix("//") && operator_char(c) {
                            let op = self.read_operator()?;
                            let nt = match op.value {
                                TokenValue::Operator(name) => self.token(TokenValue::Identifier(format!("({name})"))),
                                _ => return self.lex_error(&format!("Expected operator token, got {:?}", op))
                            };
                            if  let Some((_, ')')) = self.chars.peek() {
                                self.next();
                                Some(nt)
                            } else {
                                return self.lex_error("Unterminated parens operator");
                            }
                        } else if c == ')' {
                            self.next();
                            Some(self.token(TokenValue::Nil))
                        } else {
                            Some(self.token(TokenValue::LeftParen))
                        }
                    } else {
                        Some(self.token(TokenValue::LeftParen))
                    }
                },
                ')' => Some(self.simple_token(TokenValue::RightParen)),
                '[' => Some(self.simple_token(TokenValue::LeftBracket)),
                ']' => Some(self.simple_token(TokenValue::RightBracket)),
                '{' => Some(self.simple_token(TokenValue::LeftBrace)),
                '}' => Some(self.simple_token(TokenValue::RightBrace)),
                '`' => {
                    self.next();
                    let TokenValue::Identifier(id) = self.read_identifier()?.value else {
                        return self.lex_error("Expected valid identifier in '`' operator");
                    };
                    if let Some((_, '`')) = self.chars.peek() {
                        self.next();
                        Some(self.token(TokenValue::Operator(id)))
                    } else {
                        return self.lex_error("Unterminated '`' operator");
                    }
                },
                ',' => Some(self.simple_token(TokenValue::Comma)),
                ';' => Some(self.simple_token(TokenValue::Semicolon)),
                '.' => {
                    if self.check_prefix("...") {
                        self.advance(3);
                        Some(self.token(TokenValue::Ellipsis))
                    } else {
                        Some(self.simple_token(TokenValue::Period))
                    }
                },
                '#' => {
                    self.next();
                    match self.chars.peek() {
                        Some((_, ch)) if identifier_initial_char(*ch) => {
                            let tok = self.read_identifier()?;
                            if let TokenValue::Identifier(id) = tok.value {
                                Some(self.token(TokenValue::Symbol(id)))
                            } else {
                                return self.lex_error("not a valid symbol");
                            }
                        },
                        Some((_, ch)) if *ch == '[' => {
                            self.next();
                            Some(self.token(TokenValue::HashLeftBracket))
                        },
                        Some((_, ch)) if *ch == '(' => {
                            self.next();
                            if !self.check_prefix("//") {
                                let op = self.read_operator()?;
                                let nt = match op.value {
                                    TokenValue::Operator(name) => self.token(TokenValue::Symbol(format!("({name})"))),
                                    _ => return self.lex_error(&format!("Expected operator token, got {:?}", op))
                                };
                                if  let Some((_, ')')) = self.chars.peek() {
                                    self.next();
                                    Some(nt)
                                } else {
                                    return self.lex_error("Unterminated parens symbol operator");
                                }
                            } else {
                                return self.lex_error("# is an illegal operator char");
                            }
                        },
                        _ => {
                            return self.lex_error("# is an illegal operator char");
                        }
                    }
                },
                '/' => {
                    if self.check_prefix("//") {
                        self.snarf(|c| *c != '\n')?;
                        None
                    } else {
                        Some(self.read_operator()?)
                    }
                },
                '"' => Some(self.read_string()?),
                '\'' => Some(self.read_char()?),
                _ if ch.is_numeric() => Some(self.read_number(false)?),
                _ if identifier_initial_char(ch) => Some(self.read_identifier()?),
                _ if operator_char(ch) =>  Some(self.read_operator()?),
                _ => {
                    self.lex_error(format!("Lexing failed at illegal char: {}", ch).as_str())?;
                    None
                }
            };

            if let Some(t) = tok {
                return Ok(t);
            }
        }
        Ok(self.token(TokenValue::Eof))
    }

    pub(super) fn rewind_lexer(&mut self, checkpoint:usize) {
        println!("rewinding from {} to {}", self.position(), checkpoint);
        if checkpoint < self.position() {
            self.chars = self.src[checkpoint..].char_indices().peekable();
            self.adjust_offset(checkpoint);
            self.stashed_token = None;
            let new_pos = self.position();
            loop {
                let last_nl = self.metadata.newlines.last().unwrap_or(&0);
                if self.position() < *last_nl {
                    self.metadata.newlines.pop();
                } else {
                    println!("checking {}-{} for whitespace {} {}",*last_nl, self.position(), self.pos, self.offset);
                    self.on_new_line = self.src[*last_nl..self.position()].chars().all(char::is_whitespace);
                    break;
                }
            }
        }
    }

    pub(super) fn token_indent(&self, token:Token) -> i32 {
        let start = token.location.start;
        for i in self.metadata.newlines.iter().rev() {
            if *i <= start {
                return (start - i) as i32
            } 
        }
        start as i32
    }

    pub(super) fn next_indent(&mut self) -> i32 {
        let tok = self.peek_next_token();
        match tok {
            Ok(tok) => self.token_indent(tok),
            _ => 0
        } 
    }

    fn read_identifier(&mut self) -> Result<Token> {
        let first = self.position();
        if let Ok(last) = self.snarf(|c| identifier_char(*c)) {
            let id = &self.src[first..last];
            match id {
                "module" => Ok(self.token(TokenValue::Module)),
                "nil" => Ok(self.token(TokenValue::Nil)),
                "use" => Ok(self.token(TokenValue::Use)),
                "do" => Ok(self.token(TokenValue::Do)),
                "true" => Ok(self.token(TokenValue::True)),
                "false" => Ok(self.token(TokenValue::False)),
                "where" => Ok(self.token(TokenValue::Where)),
                "_" => Ok(self.token(TokenValue::Underscore)),
                _ => {
                    Ok(self.token(TokenValue::Identifier(id.to_string())))
                }
            }
        } else {
            self.lex_error("Expected a valid identifier")
        }
    }

    fn read_operator(&mut self) -> Result<Token> {
        let first = self.position();
        if let Ok(last) = self.snarf(|c| operator_char(*c)) {
            let id = &self.src[first..last];
            match id {
                "|" => Ok(self.token(TokenValue::Bar)),
                "=" => Ok(self.token(TokenValue::Equals)),
                "@" => Ok(self.token(TokenValue::As)),
                "->" => Ok(self.token(TokenValue::RightArrow)),
                "=>" => Ok(self.token(TokenValue::FatRightArrow)),
                "-" =>  Ok(self.token(TokenValue::Operator(id.to_string()))),
                _ => {
                    Ok(self.token(TokenValue::Operator(id.to_string())))
                }
            }
        } else {
            self.lex_error("Expected a valid operator")
        }
    }

    fn read_string(&mut self) -> Result<Token> {
        let mut result = String::new();
        self.next();
        while let Some((_p, c)) = self.next() {
            match c {
                '"' => {
                    return Ok(self.token(TokenValue::String(result)));
                }
                '\\' => {
                    let c = self.read_escape()?;
                    result.push(c);
                }
                _ => result.push(c),
            }
        }
        self.lex_error("unterminated string")
    }


    fn read_char(&mut self) -> Result<Token> {
        self.next();
        if let Some((_, c)) = self.next() {
            let ch = match c {
                '\\' => self.read_escape()?,
                c => c,
            };
            if self.check_prefix("'") {
                self.advance(1);
                return Ok(self.token(TokenValue::Char(ch)));
            }
        }
        self.lex_error("missing ' at end of char literal")
    }

    fn read_escape(&mut self) -> Result<char> {
        let ch = match self.peek()? {
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0c',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0b',
            '\\' => '\\',
            '\"' => '"',
            '\'' => '\'',
            'x' => {
                return self.get_codepoint(char::is_ascii_hexdigit, 16);
            },
            c if c.is_ascii_digit() => {
                return self.get_codepoint(char::is_ascii_digit, 10);
            },
            _ => {
                return self.lex_error("Invalid escape in literal");
            }
        };
        self.advance(1);
        Ok(ch)
    }


    fn get_codepoint(&mut self, pred: impl Fn(&char) -> bool, radix: u32) -> Result<char> {
        let start = self.position();
        let stop = self.snarf(pred)?;
        let code = match u32::from_str_radix(&self.src[start..stop], radix) {
            Ok(c) => c,
            Err(_) => return self.lex_error("Invalid codepoint escape"),
        };
        if let Some(ch) = char::from_u32(code) {
            Ok(ch)
        } else {
            self.lex_error("Invalid unicode codepoint in escape")
        }
    }

    fn read_number(&mut self, neg:bool) -> Result<Token> {
        let sign = if neg { -1 } else { 1 };
        if self.check_prefix("0x") || self.check_prefix("0X") {
            self.advance(2);
            let start = self.position();
            let stop = self.snarf(char::is_ascii_hexdigit)?;
            let bigint = match i64::from_str_radix(&self.src[start..stop], 16) {
                Ok(c) => c,
                Err(_) => return self.lex_error("Invalid hex numeral"),
            };
            return Ok(self.token(TokenValue::Integer(sign*bigint)));
        }
        let mut stop = self.snarf(char::is_ascii_digit)?;
        let res = self.peek();
        if let Ok(mut next) = res {
            if next == '.' || next == 'e' || next == 'E' {
                if next == '.' {
                    self.advance(1);
                    stop = self.snarf(char::is_ascii_digit)?;
                    next = self.peek()?
                }
                if next == 'e' || next == 'E' {
                    self.advance(1);
                    let sign = self.peek()?;
                    if sign == '+' || sign == '-' {
                        self.advance(1);
                    }
                    stop = self.snarf(char::is_ascii_digit)?;
                }
                let float = match self.src[self.token_start..stop].parse::<f64>() {
                    Ok(c) => c,
                    Err(_) => return self.lex_error("Invalid float literal"),
                };
                return Ok(self.token(TokenValue::Float(if neg { -float } else { float })));
            }
        }
        let int = match i64::from_str_radix(&self.src[self.token_start..stop], 10) {
            Ok(c) => c,
            Err(_) => return self.lex_error("Invalid integer literal"),
        };
        Ok(self.token(TokenValue::Integer(sign*int)))
    }

    fn snarf(&mut self, pred: impl Fn(&char) -> bool) -> Result<usize> {
        let next = self.chars.peek();
        match next {
            None => return self.lex_error("Unexpected end of input"),
            Some((_, ch)) if !pred(ch) => return self.lex_error("Nothing to snarf"),
            _ => self.advance(1),
        };
        while let Some((_, ch)) = self.chars.peek() {
            if pred(ch) {
                self.next();
            } else {
                break;
            }
        }
        Ok(self.position())
    }

    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    fn check_prefix(&self, what: &str) -> bool {
        self.src[self.position()..].starts_with(what)
    }

    fn peek(&mut self) -> Result<char> {
        if let Some((_, ch)) = self.chars.peek() {
            Ok(*ch)
        } else {
            self.lex_error("Unexpected end of file")
        }
    }

    fn next(&mut self) -> Option<(usize, char)> {
        let item = self.chars.next();
        if let Some((p, _)) = self.chars.peek() {
            self.pos = *p;
        } else {
            self.pos = if let Some((p, _)) = item {
                p + 1
            } else {
                self.pos
            }
        }
        item
    }

    fn simple_token(&mut self, token: TokenValue) -> Token {
        self.next();
        self.token(token)
    }

    fn lex_error<T>(&self, msg: &str) -> Result<T> {
        Err(Error::Syntax {
            msg: msg.to_string(),
            loc: self.location_current_token(),
        }
        .into())
    }

    fn token(&mut self, token: TokenValue) -> Token {
        let nl = self.on_new_line;
        self.on_new_line = false;
        Token {
            value: token,
            location: self.location_current_token(),
            on_new_line: nl
        }
    }

    fn location_current_token(&self) -> Location {
        // TODO: give InContext if applicable
        Location::at(self.token_start, self.position())
    }
}

fn identifier_initial_char(ch:char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn identifier_char(ch:char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

fn forbidden_operator_char(ch:char) -> bool {
    ch == '(' || ch == ')' ||
    ch == '[' || ch == ']' ||
    ch == '{' || ch == '}' ||
    ch == '.' || ch == ',' ||
    ch == ';' || ch == '#' ||
    ch == '`'
}

fn operator_char(ch:char) -> bool {
    (ch.is_symbol() || ch.is_ascii_punctuation()) && !forbidden_operator_char(ch)
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Identifier(String),
    Symbol(String),
    Operator(String),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    // punctuation
    Comma,
    Period,
    Semicolon,
    // Brackets
    LeftParen,
    RightParen,
    HashLeftBracket, // #[
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    // Keywords
    As,
    At,
    Do,
    If,
    Use,
    Module,
    Nil,
    Where,
    True,
    False,
    // Symbols
    Underscore,
    Ellipsis,
    Equals,
    Backslash,
    RightArrow,
    FatRightArrow,
    Bar,
    // Layout
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub location: Location,
    pub on_new_line: bool,
}

impl Token {
    pub(super) fn new(value: TokenValue) -> Token {
        Token {
            value,
            location: Location::at(0, 0),
            on_new_line: false
        }
    }

}

impl TokenValue {
    pub(super) fn is_literal(&self) -> bool {
        match self {
            TokenValue::Char(_) | TokenValue::String(_) |
            TokenValue::Float(_) | TokenValue::Integer(_) |
            TokenValue::Symbol(_) | TokenValue::HashLeftBracket|
            TokenValue::False | TokenValue::True | TokenValue::Nil => true,
            _ => false
        }
    }
}

impl From<TokenValue> for Token {
    fn from(val: TokenValue) -> Self {
        Token::new(val)
    }
}

impl PartialEq<Token> for Token {
    fn eq(&self, other: &Token) -> bool {
        self.value == other.value
    }
}

impl PartialEq<TokenValue> for Token {
    fn eq(&self, other: &TokenValue) -> bool {
        self.value == *other
    }
}
