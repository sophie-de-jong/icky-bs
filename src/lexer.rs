use std::path::PathBuf;
use crate::error::*;

#[derive(Debug, Clone)]
pub enum Location {
    File {
        line: String,
        col: usize,
        row: usize,
        path: PathBuf
    },
    Repl {
        line: String,
        col: usize
    }
}

impl Location {
    pub fn column(&self) -> usize {
        match self {
            Location::File { path: _, line: _, row: _, col } => *col,
            Location::Repl { line: _, col } => *col,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    Combinator,
    Symbol,
    String,

    Link,
    Repr,
    Source,
    Debug,
    Quit,

    OpenParen,
    CloseParen,
    ColonEquals,
    Eol,

    InvalidChar,
    InvalidString,
    InvalidCommand,
}

impl TokenKind {
    pub fn human_name(&self) -> &'static str {
        match self {
            TokenKind::Combinator => "a combinator",
            TokenKind::Symbol => "a symbol",
            TokenKind::String => "a string",
            TokenKind::Link => "/link",
            TokenKind::Repr => "/repr",
            TokenKind::Source => "/source",
            TokenKind::Debug => "/debug",
            TokenKind::Quit => "/quit",
            TokenKind::OpenParen => "open paren",
            TokenKind::CloseParen => "close paren",
            TokenKind::ColonEquals => "an assignment",
            TokenKind::Eol => "end of line",
            TokenKind::InvalidChar => "invalid character",
            TokenKind::InvalidString => "unclosed string",
            TokenKind::InvalidCommand => "invalid command"
        }
    }

    fn from_command(text: &str) -> Option<TokenKind> {
        match text {
            "/link"   => Some(TokenKind::Link),
            "/repr"   => Some(TokenKind::Repr),
            "/source" => Some(TokenKind::Source),
            "/debug"  => Some(TokenKind::Debug),
            "/quit"   => Some(TokenKind::Quit),
            _ => None
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub location: Location
}

pub struct Lexer {
    index: usize,
    chars: Vec<char>,
    current: Option<Token>,
    file_path: Option<PathBuf>,
    line: usize, 
    bol: usize,
}

impl Lexer {
    pub fn new(chars: Vec<char>, file_path: Option<PathBuf>) -> Lexer {
        let mut lexer = Lexer {
            current: None,
            chars,
            file_path,
            line: 1, // Line numbers in files start at 1.
            index: 0,
            bol: 0,
        };
        lexer.advance();
        lexer
    }

    pub fn is_exhausted(&self) -> bool {
        self.index >= self.chars.len()
    }

    pub fn current_index(&self) -> usize {
        self.index
    }

    pub fn current_line(&self) -> String {
        let mut eol = self.bol;
        while eol < self.chars.len() && self.chars[eol] != '\n' {
            eol += 1;
        }
        self.chars[self.bol..eol].iter().collect()
    }

    pub fn location(&self) -> Location {
        match &self.file_path {
            Some(file_path) => Location::File {
                path: file_path.clone(),
                line: self.current_line(),
                row: self.line,
                col: self.index - self.bol
            },
            None => Location::Repl {
                line: self.current_line(),
                col: self.index - self.bol
            },
        }
    }

    pub fn current(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    pub fn advance(&mut self) -> Option<Token> {
        self.drop_whitespace();
        let location = self.location();

        if let Some(ch) = self.drop_char() {
            let mut text = ch.to_string();
            let token = match ch {
                '\n' => Token { kind: TokenKind::Eol,       text, location },
                '(' => Token { kind: TokenKind::OpenParen,  text, location },
                ')' => Token { kind: TokenKind::CloseParen, text, location },
                ':' => if let Some(ch) = self.drop_char_if(|ch| ch == '=') {
                    text.push(ch);
                    Token { kind: TokenKind::ColonEquals, text, location }
                } else {
                    Token { kind: TokenKind::InvalidChar, text, location }
                },
                '/' => if self.drop_char_if(|ch| ch == '/').is_some() {
                    self.drop_line();
                    return self.advance()
                } else {
                    while let Some(ch) = self.drop_char_if(is_ident_char) {
                        text.push(ch)
                    }
                    Token { 
                        kind: TokenKind::from_command(&text).unwrap_or(TokenKind::InvalidCommand),
                        text, 
                        location 
                    }
                }
                '"' => {
                    text.clear();
                    while let Some(ch) = self.drop_char_if(|ch| ch != '"' && ch != '\n') {
                        text.push(ch)
                    }
                    Token {
                        kind: if self.drop_char_if(|x| x == '"').is_some() {
                            TokenKind::String
                        } else {
                            TokenKind::InvalidString
                        },
                        text,
                        location
                    }
                }
                _ => {
                    while let Some(ch) = self.drop_char_if(is_ident_char) {
                        text.push(ch)
                    }

                    if is_valid_combinator(&text) {
                        Token { kind: TokenKind::Combinator, text, location }
                    } else if is_valid_symbol(&text) {
                        Token { kind: TokenKind::Symbol,     text, location }
                    } else {
                        Token { kind: TokenKind::InvalidChar,    text, location }
                    }
                }
            };

            self.current.replace(token)
        } else {
            self.current.take()
        }
    }

    pub fn expect_end(&mut self) -> Result<()> {
        match self.advance() {
            None => Ok(()),
            Some(token) => if token.kind == TokenKind::Eol {
                Ok(())
            } else {
                let message = format!("expected nothing, found {}", token.kind.human_name());
                Err(Error::from_token(ErrorKind::UnexpectedToken, message, token))
            }
        }
    }

    pub fn expect_token(&mut self, kind: TokenKind) -> Result<Token> {
        if let Some(token) = self.advance() {
            if kind == token.kind {
                return Ok(token)
            }

            let message = format!("expected {}, but got {}", kind.human_name(), token.kind.human_name());
            Err(Error::from_token(ErrorKind::UnexpectedToken, message, token))
        } else {
            let message = format!("expected {}, but got nothing", kind.human_name());
            Err(Error::new(ErrorKind::UnexpectedToken, message, self.location(), 1))
        }
    }

    pub fn expect_tokens(&mut self, kinds: &[TokenKind]) -> Result<Token> {
        if let Some(token) = self.advance() {
            for kind in kinds {
                if *kind == token.kind {
                    return Ok(token)
                }
            }

            let message = format!("expected {}, but got {}", list_from(kinds), token.kind.human_name());
            Err(Error::from_token(ErrorKind::UnexpectedToken, message, token))
        } else {
            let message = format!("expected {}, but got nothing", list_from(kinds));
            Err(Error::new(ErrorKind::UnexpectedToken, message, self.location(), 1))
        }
    }

    fn drop_char_if(&mut self, pred: impl FnOnce(char) -> bool) -> Option<char> {
        self.chars.get(self.index).cloned().and_then(|ch| {
            if pred(ch) {
                self.drop_char()
            } else {
                None
            }
        })
    }

    fn drop_char(&mut self) -> Option<char> {
        self.chars.get(self.index).map(|ch| {
            self.index += 1;
            if *ch == '\n' {
                self.bol = self.index;
                self.line += 1;
            }
            *ch
        })
    }

    fn drop_line(&mut self) {
        while self.drop_char_if(|ch| ch != '\n').is_some() {}
        self.drop_char();
    }

    fn drop_whitespace(&mut self) {
        while self.drop_char_if(|ch| ch.is_whitespace() && ch != '\n').is_some() {}
    }
}

fn is_ident_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

fn is_valid_combinator(combinator: &str) -> bool {
    combinator.chars().all(|ch| matches!(ch, 'A'..='Z' | '0'..='9' | '_'))
}

fn is_valid_symbol(symbol: &str) -> bool {
    symbol.chars().all(|ch| matches!(ch, 'a'..='z' | '_'))
}

fn list_from(kinds: &[TokenKind]) -> String {
    let mut list = String::new();
    for (i, kind) in kinds.iter().enumerate() {
        if i != 0 && i + 1 < kinds.len() {
            list.push_str(", ");
        } else if i + 1 == kinds.len() {
            list.push_str(", or ");
        }
        list.push_str(kind.human_name())
    }
    list
}
