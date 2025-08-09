use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    io,
};

use anyhow::anyhow;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct Metadata {
    pub file: Option<String>,
    pub trivia: Vec<Annotation>,
    pub annotations: HashMap<Node, Annotation>,
    pub locations: HashMap<Node, Location>,
    pub using: HashSet<Name>,
    pub base_name: Option<Name>,
    pub newlines: Vec<usize>,
    pub last_id: Node,
}

impl Metadata {
    pub fn new(file: Option<String>) -> Self {
        Metadata {
            file: file,
            trivia: Vec::new(),
            annotations: HashMap::new(),
            locations: HashMap::new(),
            using: HashSet::new(),
            base_name: None,
            newlines: Vec::new(),
            last_id: 0,
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
    pub start: usize,
    pub end: usize,
}

impl Location {
    pub fn at(start: usize, end: usize) -> Self {
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
    #[error(transparent)]
    General { err: anyhow::Error },
    #[error("Runtime error: {}", msg)]
    Runtime { msg: String, loc: Option<Location> },
    #[error("Desugaring error: {}", msg)]
    Desugar { msg: String, loc: Option<Location> },
    #[error("Error: {}", msg)]
    NibPanic { msg: String },
    #[error("Exit {}", exit_code)]
    NibExit { exit_code: i32 },
}

pub type Result<T> = std::result::Result<T, Error>;

impl Error {
    pub fn runtime_error(msg: &str) -> Error {
        Error::Runtime {
            msg: msg.to_owned(),
            loc: None,
        }
    }
}

impl From<anyhow::Error> for Error {
    fn from(value: anyhow::Error) -> Self {
        Error::General { err: value }
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Error::General {
            err: anyhow!(value),
        }
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
    Plain(String),
}

impl Name {
    pub fn string(&self) -> String {
        match self {
            Name::Qualified(path, name) => {
                let mut str = String::new();
                for s in path {
                    str.push_str(s);
                    str.push_str(".");
                }
                str.push_str(name);
                str
            }
            Name::Plain(name) => name.clone(),
        }
    }

    pub fn name(n: &str) -> Self {
        let mut parts: Vec<&str> = n.split(".").collect();
        if parts.len() == 1 {
            Name::Plain(parts[0].to_string())
        } else {
            let base = parts.pop().unwrap();
            let path = parts.iter().map(|s| s.to_string()).collect();
            Name::Qualified(path, base.to_string())
        }
    }

    pub fn append(path: &Name, base: &Name) -> Result<Name> {
        match (path, base) {
            (Name::Qualified(path, last), Name::Plain(b)) => {
                let mut p = path.clone();
                p.push(last.clone());
                Ok(Name::Qualified(p, b.clone()))
            }
            (Name::Plain(parent), Name::Plain(b)) => {
                Ok(Name::Qualified(vec![parent.clone()], b.clone()))
            }
            (Name::Qualified(leader, end_leader), Name::Qualified(path, name)) => {
                let mut p = leader.clone();
                p.push(end_leader.clone());
                p.append(&mut path.clone());
                Ok(Name::Qualified(p, name.clone()))
            }
            (Name::Plain(a), Name::Qualified(path, b)) => {
                let mut np = vec![a.clone()];
                np.append(&mut path.clone());
                Ok(Name::Qualified(np, b.clone()))
            }
        }
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string())?;
        Ok(())
    }
}
