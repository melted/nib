use std::fmt::Display;

use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Location {
    Offset { start: usize, end: usize },
    Unlocated,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Offset { start, end } => {
                write!(f, "offset {}-{}", start, end)?;
                Ok(())
            }
            Location::Unlocated => {
                write!(f, "unknown location")?;
                Ok(())
            }
        }
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
