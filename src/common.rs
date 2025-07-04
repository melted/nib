use std::{collections::HashMap, fmt::Display, io};

use thiserror::Error;


#[derive(Debug, Clone, PartialEq)]
pub struct Metadata {
    pub file: Option<String>,
    pub trivia: Vec<Annotation>,
    pub annotations: HashMap<Node, Annotation>,
    pub locations: HashMap<Node, Location>,
    pub newlines: Vec<usize>,
    pub last_id: Node
}


impl Metadata {
    pub fn new(file: Option<String>) -> Self {
         Metadata {
            file: file, 
            trivia: Vec::new(),
            annotations: HashMap::new(),
            locations: HashMap::new(),
            newlines: Vec::new(),
            last_id: 0
        }
    }
}

pub type Node = u32;

#[derive(Debug, Clone, PartialEq)]
pub enum Annotation {
    OtherPragma(String),
    Doc(String),
    Comment(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Location {
    pub start:usize,
    pub end:usize
}

impl Location {
    pub fn at(start:usize, end:usize) -> Self {
        Location { start, end }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "offset {}-{}", self.start, self.end)?;
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Syntax error: {} at {}", msg, loc)]
    Syntax { msg: String, loc: Location }, 
    #[error("Error: {}", err)]
    General { err: anyhow::Error }
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<anyhow::Error> for Error {
    fn from(value: anyhow::Error) -> Self {
        Error::General { err: value }
    }
}

impl From<Error> for io::Error {
    fn from(value: Error) -> io::Error {
        io::Error::other(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Name {
    Qualified(Vec<String>, String),
    Plain(String)
}

impl Name {
    pub fn to_string(&self) -> String {
        match self {
            Name::Qualified(path, name) => {
                let mut str = String::new();
                for s in path {
                    str.push_str(s);
                    str.push_str(".");
                }
                str.push_str(name);
                str
            },
            Name::Plain(name) => name.clone()
        }
    }

    pub fn name(n : &str) -> Self {
        let mut parts : Vec<&str> = n.split(".").collect();
        if parts.len() == 1 {
            Name::Plain(parts[0].to_string())
        } else {
            let base = parts.pop().unwrap();
            let path = parts.iter().map(|s| s.to_string()).collect();
            Name::Qualified(path, base.to_string())
        }
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())?;
        Ok(())
    }
}
