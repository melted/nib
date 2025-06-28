use std::{fmt::Display, io};

use thiserror::Error;

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
