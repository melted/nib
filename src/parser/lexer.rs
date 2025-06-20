use crate::common::Error;
use crate::common::Location;
use crate::common::Result;

impl<'a> super::ParserState<'a> {
    pub(super) fn get_next_token(&mut self) -> Result<Token> {
        while let Some((p, c)) = self.chars.peek() {
            self.token_start = *p;
            let tok = match *c {
                '\n' => {
                    Some(self.handle_newline()?)
                },
                _ if c.is_whitespace() => {
                    self.next();
                    None
                },
                '(' => Some(self.simple_token(TokenValue::LeftParen)),
                ')' => Some(self.simple_token(TokenValue::RightParen)),
                '[' => Some(self.simple_token(TokenValue::LeftBracket)),
                ']' => Some(self.simple_token(TokenValue::RightBracket)),
                '`' => None,
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
                    if let Some((_, ch)) = self.chars.peek() {
                        if identifier_initial_char(*ch) {
                            let tok = self.read_identifier()?;
                            if let TokenValue::Identifier(id) = tok.value {
                                Some(self.token(TokenValue::Symbol(id)))
                            } else {
                                return self.lex_error("not a valid symbol");
                            }
                        } else {
                            Some(self.token(TokenValue::Operator("#".to_string())))
                        }
                    } else {
                        Some(self.token(TokenValue::Operator("#".to_string())))
                    }
                },
                '/' => {
                    if self.check_prefix("//") {
                        self.snarf(|c| *c != '\n')?;
                        None
                    } else {
                        Some(self.token(TokenValue::Operator("/".to_string())))
                    }
                },
                '"' => Some(self.read_string()?),
                '\'' => Some(self.read_char()?),
                _ if c.is_numeric() => Some(self.read_number()?),
                _ if identifier_initial_char(*c) => {
                    Some(self.read_identifier()?)
                },
                _ => {
                    let ch = *c;
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

    fn read_identifier(&mut self) -> Result<Token> {
        let first = self.pos;
        if let Ok(last) = self.snarf(|c| identifier_char(*c)) {
            let id = &self.src[first..last];
            match id {
                "as" => Ok(self.token(TokenValue::As)),
                "module" => Ok(self.token(TokenValue::Module)),
                "use" => Ok(self.token(TokenValue::Use)),
                "do" => Ok(self.token(TokenValue::Do)),
                "_" => Ok(self.token(TokenValue::Underscore)),
                _ => {
                    Ok(self.token(TokenValue::Identifier(id.to_string())))
                }
            }
        } else {
            self.lex_error("Expected a valid identifier")
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
        let start = self.pos;
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

    fn read_number(&mut self) -> Result<Token> {
        if self.check_prefix("0x") || self.check_prefix("0X") {
            self.advance(2);
            let start = self.pos;
            let stop = self.snarf(char::is_ascii_hexdigit)?;
            let bigint = match i64::from_str_radix(&self.src[start..stop], 16) {
                Ok(c) => c,
                Err(_) => return self.lex_error("Invalid hex numeral"),
            };
            return Ok(self.token(TokenValue::Integer(bigint)));
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
                let float = match self.src[self.token_start..stop].parse() {
                    Ok(c) => c,
                    Err(_) => return self.lex_error("Invalid float literal"),
                };
                return Ok(self.token(TokenValue::Float(float)));
            }
        }
        let int = match i64::from_str_radix(&self.src[self.token_start..stop], 16) {
            Ok(c) => c,
            Err(_) => return self.lex_error("Invalid integer literal"),
        };
        Ok(self.token(TokenValue::Integer(int)))
    }

    fn handle_newline(&mut self) -> Result<Token> {
        self.chars.next();
        let mut indent = 0;
        while let Some((_, ch)) = self.chars.peek() {
            match ch {
                ' ' => {
                    indent+=1;
                    self.next();
                },
                '\t' => {
                    indent += 8;
                    self.next();
                },
                '\r' | '\n' => {
                    indent = 0;
                    self.next();
                },
                _ => {
                    return Ok(self.token(TokenValue::Indent(indent)));
                }
            }
        }
        Ok(self.token(TokenValue::Eof))
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
        Ok(self.pos)
    }

    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    fn check_prefix(&self, what: &str) -> bool {
        self.src[self.pos..].starts_with(what)
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
        Token {
            value: token,
            location: self.location_current_token(),
        }
    }

    fn location_current_token(&self) -> Location {
        // TODO: give InContext if applicable
        Location::Offset {
            start: self.token_start,
            end: self.pos,
        }
    }
}

fn identifier_initial_char(ch:char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn identifier_char(ch:char) -> bool {
    ch.is_alphanumeric() || ch == '_'
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
    LeftParen,
    RightParen,
    Comma,
    Period,
    Semicolon,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    As,
    Do,
    If,
    Use,
    Module,
    Where,
    Underscore,
    Ellipsis,
    Equals,
    Backslash,
    RightArrow,
    At,
    Indent(usize),
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub location: Location,
}

impl Token {
    pub(super) fn new(value: TokenValue) -> Token {
        Token {
            value,
            location: Location::Unlocated,
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
