use std::fmt;
use std::error;
use std::result;
use crate::lexer::Location;
use crate::lexer::Token;

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
#[non_exhaustive]
pub enum ErrorKind {
    // Parser receives an unexpected token.
    UnexpectedToken,
    // Parser receives an empty term.
    EmptyTerm,
    // Parser is unable to find a combinator or symbol.
    UnbalancedParens,
    // Parser finds unbalanced parentheses.
    NotFound,
    // Interpreter hits recursion limit.
    RecursionLimit,
    // Invalid interpreter file.
    InvalidFile,
}

impl ErrorKind {
    fn as_str(&self) -> &'static str {
        match self {
            ErrorKind::UnexpectedToken => "unexpected token",
            ErrorKind::EmptyTerm => "empty term",
            ErrorKind::UnbalancedParens => "unbalanced parentheses",
            ErrorKind::NotFound => "undefined binding",
            ErrorKind::RecursionLimit => "recursion limit hit",
            ErrorKind::InvalidFile => "invalid file",
        }
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,    // Type of error.
    message: String,    // Message to report to user.
    location: Location, // Location within REPL or file.
    width: usize,       // Width or size of the error in chars.
}

impl Error {
    pub fn new(kind: ErrorKind, message: impl Into<String>, location: Location, width: usize) -> Error {
        Error { 
            kind, 
            message: message.into(), 
            location, 
            width 
        }
    }

    pub fn from_token(kind: ErrorKind, message: impl Into<String>, token: Token) -> Error {
        Error {
            kind,
            message: message.into(),
            location: token.location,
            width: token.text.len()
        }
    }
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let arrows = "^".repeat(self.width);
        match self.location.clone() {
            Location::File { path, row, col, line } => {
                let width = row.to_string().len();
                writeln!(f, "error: {}:{}:{} => {}", path.to_str().unwrap(), row, col, self.kind.as_str())?;
                writeln!(f, "{:>width$} |", "")?;
                writeln!(f, "{} |     {}", row, line)?;
                writeln!(f, "{:>width$} |     {}{} {}", "", " ".repeat(col), arrows, self.message)
            }
            Location::Repl { cursor, .. } => {
                write!(f, "{}{} {}", " ".repeat(cursor), arrows, self.message)
            }
        }
    }
}