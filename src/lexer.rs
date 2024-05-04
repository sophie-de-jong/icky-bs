use core::fmt;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum Loc {
    File {
        path: PathBuf,
        line: String,
        row: usize,
        col: usize
    },
    Repl {
        row: usize,
        col: usize
    }
}

impl Loc {
    fn get_column(&self) -> usize {
        match self {
            Loc::File { path: _, line: _, row: _, col } => *col,
            Loc::Repl { row: _, col } => *col,
        }
    }

    pub fn width_from(&self, other: &Loc) -> usize {
        self.get_column().abs_diff(other.get_column())
    }
}

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    Ident,

    OpenParen,
    CloseParen,
    ColonEquals,
    
    Eol,
    Invalid,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::OpenParen => write!(f, "`(`"),
            TokenKind::CloseParen => write!(f, "`)`"),
            TokenKind::ColonEquals => write!(f, "`:=`"),
            TokenKind::Eol => write!(f, "end of line"),
            TokenKind::Invalid => write!(f, "invalid")
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub loc: Loc
}

impl Token {
    pub fn is_kind(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn has_text_that(&self, pred: impl FnOnce(&str) -> bool) -> bool {
        pred(&self.text)
    }
}

pub struct Lexer {
    chars: Vec<char>,
    file_path: Option<PathBuf>,
    line: usize, 
    index: usize,
    bol: usize,
}

impl Lexer {
    pub fn new(chars: Vec<char>, file_path: Option<PathBuf>) -> Lexer {
        Lexer {
            chars,
            file_path,
            line: 1, // Line numbers in files start at 1.
            index: 0,
            bol: 0,
        }
    }

    pub fn is_exhausted(&self) -> bool {
        self.index > self.chars.len()
    }

    pub fn current_line(&self) -> String {
        let mut eol = self.bol;
        while eol < self.chars.len() && self.chars[eol] != '\n' {
            eol += 1;
        }
        self.chars[self.bol..eol].iter().collect()
    }

    pub fn loc(&self) -> Loc {
        match &self.file_path {
            Some(file_path) => Loc::File {
                path: file_path.clone(),
                line: self.current_line(),
                row: self.line,
                col: self.index - self.bol
            },
            None => Loc::Repl {
                row: self.line,
                col: self.index - self.bol
            },
        }
    }

    pub fn drop_comments(&mut self) {
        loop {
            // Drop any whitespace characters
            while self.drop_char_if(|ch| ch.is_whitespace()).is_some() {}

            // Check for comment.
            if self.drop_char_if(|ch| ch == '#').is_some() {
                self.drop_line()
            } else {
                break
            }
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

    pub fn next_token(&mut self) -> Option<Token> {
        self.drop_whitespace();
        let loc = self.loc();

        if let Some(ch) = self.drop_char() {
            let mut text = ch.to_string();
            let token = match ch {
                '\n' => Token { kind: TokenKind::Eol,       text, loc },
                '(' => Token { kind: TokenKind::OpenParen,  text, loc },
                ')' => Token { kind: TokenKind::CloseParen, text, loc },
                '#' => {
                    self.drop_line();
                    return self.next_token()
                }
                ':' => if let Some(ch) = self.drop_char_if(|ch| ch == '=') {
                    text.push(ch);
                    Token { kind: TokenKind::ColonEquals, text, loc }
                } else {
                    Token { kind: TokenKind::Invalid,     text, loc }
                },
                _ => if is_ident_char(ch) {
                    while let Some(ch) = self.drop_char_if(is_ident_char) {
                        text.push(ch)
                    }
                    Token { kind: TokenKind::Ident,   text, loc }
                } else {
                    Token { kind: TokenKind::Invalid, text, loc }
                }
            };

            Some(token)
        } else {
            None
        }
    }
}

fn is_ident_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}
