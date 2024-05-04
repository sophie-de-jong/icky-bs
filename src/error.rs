use std::fmt;
use std::error;
use crate::lexer::Loc;

#[derive(Debug)]
pub struct SKIError {
    loc: Loc,
    text: String,
    width: usize,
}

pub type SKIResult<T> = Result<T, SKIError>;

impl SKIError {
    pub fn new(text: impl Into<String>, loc: Loc, width: usize) -> SKIError {
        SKIError { text: text.into(), loc, width }
    }
}

impl error::Error for SKIError {}

impl fmt::Display for SKIError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.loc.clone() {
            Loc::File { path, row, col, line } => {
                write!(f, "error in {}:{}:{}\n{}\n{}{} {}", path.to_str().unwrap(), row, col, line, " ".repeat(col), "^".repeat(self.width), self.text)
            }
            Loc::Repl { col, row: _ } => {
                write!(f, "{}{} {}", " ".repeat(col), "^".repeat(self.width), self.text)
            }
        }
    }
}