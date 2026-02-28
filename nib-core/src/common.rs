use anyhow::anyhow;
use libffi::middle::Cif;
use std::num::{NonZeroU32, TryFromIntError};
use std::sync::LazyLock;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    io,
};
use thiserror::Error;

pub fn next_source_id() -> u32 {
    unsafe {
        static mut LOCAL_VAL: LazyLock<u32> = LazyLock::new(|| 0);
        *LOCAL_VAL += 1;
        *LOCAL_VAL
    }
}

#[derive(Debug, Clone)]
pub struct Metadata {
    pub file: Option<String>,
    pub source_id: u32,
    pub source: String,
    pub errors: Vec<SyntaxError>,
    pub trivia: Vec<Annotation>,
    pub annotations: HashMap<Node, Annotation>,
    pub locations: HashMap<Node, Location>,
    pub using: HashSet<Name>,
    pub base_name: Option<Name>,
    pub newlines: Vec<usize>,
    pub last_id: Node,
}

impl Metadata {
    pub fn empty() -> Self {
        Self::new(None, "")
    }

    pub fn new(file: Option<String>, code: &str) -> Self {
        Metadata {
            file,
            source_id: next_source_id(),
            source: code.to_owned(),
            errors: Vec::new(),
            trivia: Vec::new(),
            annotations: HashMap::new(),
            locations: HashMap::new(),
            using: HashSet::new(),
            base_name: None,
            newlines: Vec::new(),
            last_id: 0,
        }
    }

    pub fn linecol(&self, loc: &Location) -> (usize, usize) {
        let target = loc.start as usize;
        if self.newlines.is_empty() {
            return (0, target);
        }
        let line = match self.newlines.binary_search(&target) {
            Ok(l) | Err(l) => l,
        };
        let col = if line == 0 {
            target
        } else {
            target - self.newlines[line - 1]
        };
        (line, col)
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
    pub source: u32,
    pub start: u32,
    pub end: u32,
}

impl Location {
    pub fn empty() -> Self {
        Location::at(0, 0, 0)
    }

    pub fn at(source: u32, start: usize, end: usize) -> Self {
        Location {
            source,
            start: start as u32,
            end: end as u32,
        }
    }

    pub fn start(&self) -> usize {
        self.start as usize
    }

    pub fn end(&self) -> usize {
        self.end as usize
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "offset {}-{}", self.start, self.end)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxError {
    pub msg: String,
    pub loc: Location,
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Syntax error: {} at {}", err.msg, err.loc)]
    Syntax { err: SyntaxError },
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

impl From<SyntaxError> for Error {
    fn from(value: SyntaxError) -> Self {
        Error::Syntax { err: value }
    }
}

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

impl From<TryFromIntError> for Error {
    fn from(value: TryFromIntError) -> Self {
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
    Qualified(Vec<Symbol>, Symbol),
    Plain(Symbol),
}

impl From<&Name> for Name {
    fn from(value: &Name) -> Self {
        value.clone()
    }
}

impl From<Symbol> for Name {
    fn from(value: Symbol) -> Self {
        Name::str(value.as_str())
    }
}

impl TryFrom<&Vec<Symbol>> for Name {
    fn try_from(path: &Vec<Symbol>) -> Result<Name> {
        match path.len() {
            0 => Err(Error::Syntax { err: SyntaxError { msg: "name need a non-empty path".to_string(), loc: Location::empty() } }),
            1 => Ok(Name::Plain(path[0])),
            n => Ok(Name::Qualified(path[0..n-1].to_vec(), path[n-1]))
        }
    }
    
    type Error=Error;
}

impl Name {
    pub fn string(&self) -> String {
        match self {
            Name::Qualified(path, name) => {
                let mut str = String::new();
                for s in path {
                    str.push_str(s.as_str());
                    str.push('.');
                }
                str.push_str(name.as_str());
                str
            }
            Name::Plain(name) => name.as_str().to_owned(),
        }
    }

    pub fn str(n: &str) -> Self {
        let mut parts: Vec<&str> = n.split(".").collect();
        if parts.len() == 1 {
            Name::Plain(Symbol::from(parts[0]))
        } else {
            let base = parts.pop().unwrap();
            let path = parts.into_iter().map(Symbol::from).collect();
            Name::Qualified(path, Symbol::from(base))
        }
    }

    pub fn sym(s: &Symbol) -> Self {
        Name::Plain(*s)
    }

    pub fn top(&self) -> Symbol {
        match self {
            Name::Qualified(items, n) => *items.first().unwrap_or(n),
            Name::Plain(n) => *n,
        }
    }

    pub fn tail(&self) -> Vec<Symbol> {
        match self {
            Name::Qualified(path, n) => {
                let mut tail = Vec::new();
                tail.extend_from_slice(&path[1..]);
                tail.push(*n);
                tail
            }
            Name::Plain(n) => vec![*n],
        }
    }

    pub fn path(&self) -> Vec<Symbol> {
        match self {
            Name::Qualified(path, _) => path.clone(),
            Name::Plain(global_symbol) => vec![],
        }
    }

    pub fn base(&self) -> Symbol {
        match self {
            Name::Qualified(_, base) | Name::Plain(base) => *base,
        }
    }

    pub fn append(path: &Name, base: &Name) -> Result<Name> {
        match (path, base) {
            (Name::Qualified(path, last), Name::Plain(b)) => {
                let mut p = path.clone();
                p.push(*last);
                Ok(Name::Qualified(p, *b))
            }
            (Name::Plain(parent), Name::Plain(b)) => {
                Ok(Name::Qualified(vec![*parent], *b))
            }
            (Name::Qualified(leader, end_leader), Name::Qualified(path, name)) => {
                let mut p = leader.clone();
                p.push(*end_leader);
                p.append(&mut path.clone());
                Ok(Name::Qualified(p, *name))
            }
            (Name::Plain(a), Name::Qualified(path, b)) => {
                let mut np = vec![*a];
                np.append(&mut path.clone());
                Ok(Name::Qualified(np, *b))
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

pub fn align_int(value: usize, alignment: usize) -> usize {
    (value + (alignment - 1)) & !(alignment - 1)
}

pub type Symbol = symbol_table::GlobalSymbol;

pub fn sym(s: &str) -> Symbol {
    Symbol::from(s)
}

pub fn symbol_id(symbol: &Symbol) -> u32 {
    NonZeroU32::from(*symbol).get()
}

pub fn get_symbol(id: u32) -> Symbol {
    Symbol::from(NonZeroU32::new(id).unwrap())
}

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum CType {
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Pointer,
    Void,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub cif: Cif,
    pub arg_types: Vec<CType>,
    pub ret_type: CType,
}
