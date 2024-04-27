use std::{iter::Peekable, str::Chars};

pub enum Token {
    Ident(String),
    LeftParen,
    RightParen,
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    cursor: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.chars().peekable(),
            cursor: 0
        }
    }

    // Keeps returning tokens until comment or EOL is reached.
    pub fn next_token(&mut self) -> Option<Token> {
        // Skip any whitespace.
        while self.chars.next_if(|ch| char::is_whitespace(*ch)).is_some() {
            self.cursor += 1;
        }
        
        self.cursor += 1;
        match self.chars.next()? {
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            ';' => None,
            other => {
                let mut ident = String::from(other);
                while let Some(ch) = self.chars.next_if(|ch| !matches!(ch, '(' | ')' | ';')) {
                    ident.push(ch);
                    self.cursor += 1;
                }
                Some(Token::Ident(ident))
            }
        }
    }

    pub fn get_cursor(&self) -> u8 {
        self.cursor
    }

    pub fn is_valid_combinator(combinator: &str) -> bool {
        for ch in combinator.chars() {
            if !matches!(ch, 'A'..='Z' | '0'..='9' | '_') {
                return false
            }
        }
        true
    }
    
    pub fn is_valid_variable(variable: &str) -> bool {
        for ch in variable.chars() {
            if !matches!(ch, 'a'..='z' | '_') {
                return false
            }
        }
        true
    }
}
