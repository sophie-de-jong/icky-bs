use std::fmt;
use std::error;

#[derive(Debug)]
pub struct SKIError {
    pub cursor: usize,
    width: usize,
    text: String,
}

pub type SKIResult<T> = Result<T, SKIError>;

impl SKIError {
    pub fn new(text: &str, cursor: usize, width: usize) -> SKIError {
        SKIError { text: text.to_string(), cursor, width }
    }

    pub fn get_cursor(&self) -> String {
        format!("{}{}", " ".repeat(self.cursor), "^".repeat(self.width))
    }
}

impl error::Error for SKIError {}

impl fmt::Display for SKIError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.get_cursor(), self.text)
    }
}