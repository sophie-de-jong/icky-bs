use std::fmt;
use std::error;
use crate::lexer::Location;

#[derive(Debug)]
pub struct SKIError {
    location: Location,
    text: String,
    width: usize,
}

pub type SKIResult<T> = Result<T, SKIError>;

impl SKIError {
    pub fn new(text: impl Into<String>, location: Location, width: usize) -> SKIError {
        SKIError { text: text.into(), location, width }
    }
}

impl error::Error for SKIError {}

impl fmt::Display for SKIError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.location.clone() {
            Location::File { path, row, col, line } => {
                write!(f, "error in {}:{}:{}\n{}\n{}{} {}", path.to_str().unwrap(), row, col, line, " ".repeat(col), "^".repeat(self.width), self.text)
            }
            Location::Repl { col, row: _ } => {
                write!(f, "{}{} {}", " ".repeat(col), "^".repeat(self.width), self.text)
            }
        }
    }
}