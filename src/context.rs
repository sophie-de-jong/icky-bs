use crate::error::{SKIError, SKIResult};
use crate::expr::{is_valid_combinator, is_valid_symbol, Expr};
use crate::lexer::{Lexer, TokenKind};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::io::{self, Write};

const INPUT_PROMPT: &str = "skibc>";
const OUTPUT_PROMPT: &str = " =>";
const MAX_RECURSION_DEPTH: usize = 1024;

#[derive(Debug, Default)]
pub struct Context {
    bindings: HashMap<String, Expr>,
}

impl Context {
    pub fn new(imports: Vec<PathBuf>) -> Context {
        let mut context = Context::default();
        for file_path in imports {
            context.interpret_file(file_path);
        }
        context
    }

    pub fn has_variable(&self, name: &str) -> bool {
        self.bindings.contains_key(name)   
    }

    pub fn get_variable(&self, name: &str) -> Option<Expr> {
        self.bindings.get(name).cloned()
    }

    pub fn interpret_file(&mut self, file_path: PathBuf) {
        let source = fs::read_to_string(file_path.clone()).unwrap();
        let mut lexer = Lexer::new(source.chars().collect(), Some(file_path));
        while !lexer.is_exhausted() {
            lexer.drop_comments();
            if let Err(err) = self.evaluate_line(&mut lexer) {
                eprintln!("{}", err);
                std::process::exit(1);
            }
        }
    }

    pub fn start_repl(&mut self) {
        let mut line = String::new();

        loop {
            print!("{} ", INPUT_PROMPT);
            io::stdout().flush().unwrap();

            if io::stdin().read_line(&mut line).unwrap() <= 2 {
                break
            }
    
            let mut lexer = Lexer::new(line.trim().chars().collect(), None);
            if let Err(err) = self.evaluate_line(&mut lexer) {
                eprintln!("{}{}", " ".repeat(INPUT_PROMPT.len() + 1), err);
            }

            line.clear();
        }
    }

    pub fn evaluate_line(&mut self, lexer: &mut Lexer) -> SKIResult<()> {
        if lexer.current_line().contains(":=") {
            let mut symbols = Vec::new();
            let name;

            // Get combinator name.
            let token = lexer.next_token().unwrap();
            if token.is_kind(TokenKind::Ident) && token.has_text(is_valid_combinator) {
                name = token.text
            } else {
                return Err(SKIError::new("expected combinator name", token.loc, token.text.len()))
            };

            // Get symbols.
            while let Some(token) = lexer.next_token() {
                if token.is_kind(TokenKind::ColonEquals) {
                    break
                } else if token.is_kind(TokenKind::Ident) && token.has_text(is_valid_symbol) {
                    symbols.push(token.text);
                } else {
                    return Err(SKIError::new("expected `:=`", token.loc, token.text.len()));
                }
            }

            let mut expr = Expr::parse(lexer, self, Some(&symbols))?;
            expr.remove_symbols(symbols);
            self.bindings.insert(name, expr);
        } else {
            let mut expr = Expr::parse(lexer, self, None)?;
            expr.reduce(self, MAX_RECURSION_DEPTH);
            println!("{} {}", OUTPUT_PROMPT, expr);
        }

        Ok(())
    }   
}
