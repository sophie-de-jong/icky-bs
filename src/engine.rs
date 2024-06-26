use crate::error::*;
use crate::expr::*;
use crate::lexer::*;
use std::collections::HashMap;
use std::path::PathBuf;

pub const FILE_EXTENSION: &str = "ib";
const MAX_RECURSION_DEPTH: usize = 1024;

#[derive(Debug)]
pub struct Definition {
    repr: String,
    args: usize,
    expr: Expr,
    source: Option<PathBuf>
}

pub enum Command {
    DefineExpr { name: Token, symbols: Vec<String>, definition: Definition },
    Print { expr: Expr, location: Location },
    Nothing,

    // Slash commands.
    Link { file_path: Token },    // /link
    Repr { combinator: Token },   // /repr
    Source { combinator: Token }, // /source
    Debug,                        // /debug
    Quit                          // /quit
}

impl Command {
    pub fn parse(lexer: &mut Lexer, context: &mut Context) -> Result<Command> {
        let Some(token) = lexer.current() else {
            return Ok(Command::Nothing)
        };

        let result = match token.kind {
            TokenKind::Link => {
                lexer.advance();
                let file_path = lexer.expect_token(TokenKind::String)?;
                let path = PathBuf::from(&file_path.text);
                if path.extension().is_some_and(|ext| ext == FILE_EXTENSION) && path.exists() {
                    Ok(Command::Link { file_path })
                } else {
                    Err(Error::new(
                        ErrorKind::InvalidFile, 
                        format!("file does not exist or does not end with .{}", FILE_EXTENSION),
                        file_path.location,
                        file_path.text.len() + 2
                    ))
                }
            }
            TokenKind::Repr => {
                lexer.advance();
                let combinator = lexer.expect_token(TokenKind::Combinator)?;
                let name = combinator.text.as_str();
                if context.has_variable(name) || matches!(name, "I" | "K" | "S" | "B" | "C" | "Y") {
                    Ok(Command::Repr { combinator })
                } else {
                    Err(Error::from_token(ErrorKind::NotFound, "combinator not found", combinator))
                }
            }
            TokenKind::Source => {
                lexer.advance();
                let combinator = lexer.expect_token(TokenKind::Combinator)?;
                let name = combinator.text.as_str();
                if context.has_variable(name) || matches!(name, "I" | "K" | "S" | "B" | "C" | "Y") {
                    Ok(Command::Source { combinator })
                } else {
                    Err(Error::from_token(ErrorKind::NotFound, "combinator not found", combinator))
                }
            }
            TokenKind::Debug => {
                lexer.advance();
                Ok(Command::Debug)
            }
            TokenKind::Quit => {
                lexer.advance();
                Ok(Command::Quit)
            }
            TokenKind::Eol => {
                Ok(Command::Nothing)
            }
            TokenKind::Force => {
                lexer.advance();
                if lexer.current().map_or(true, |token| token.kind == TokenKind::Eol) {
                    let kinds = [
                        TokenKind::Symbol, 
                        TokenKind::Combinator, 
                        TokenKind::OpenParen, 
                        TokenKind::CloseParen,
                    ];
                    lexer.expect_tokens(&kinds)?;
                }   
                Command::parse_and_eval_expr(lexer, context, true)
            }
            TokenKind::Symbol    | TokenKind::Combinator | 
            TokenKind::OpenParen | TokenKind::CloseParen => if lexer.current_line().contains(":=") {
                let mut symbols = Vec::new();
                let mut args = String::new();
        
                // Parse name.
                let name = lexer.expect_token(TokenKind::Combinator)?;
        
                // Parse symbols.
                loop {
                    let token = lexer.expect_tokens(&[TokenKind::ColonEquals, TokenKind::Symbol])?;
                    if token.kind == TokenKind::ColonEquals {
                        break
                    } else {
                        args.push(' ');
                        args.push_str(&token.text);
                        symbols.push(token.text);
                    }
                }
        
                // Parse expression.
                let expr = Expr::parse(lexer, context, Some(&symbols), Some(&name.text))?;
                let repr = format!("{}{} -> {}", &name.text, args, expr.to_string().replace('#', &name.text));
                let definition = Definition {
                    source: context.current_file.clone(),
                    args: symbols.len(),
                    repr,
                    expr
                };
        
                Ok(Command::DefineExpr { name, symbols, definition })
            } else {
                Command::parse_and_eval_expr(lexer, context, false)
            }
            _ => lexer.expect_tokens(&[
                TokenKind::Symbol,
                TokenKind::Combinator,
                TokenKind::OpenParen,
                TokenKind::CloseParen,
                TokenKind::Force,
                TokenKind::Link,
                TokenKind::Repr,
                TokenKind::Source,
                TokenKind::Debug,
                TokenKind::Quit,
                TokenKind::Eol
            ]).map(|_| Command::Nothing)
        };

        lexer.expect_end()?;
        result
    }

    fn parse_and_eval_expr(lexer: &mut Lexer, context: &mut Context, force_eval: bool) -> Result<Command> {
        let mut depth = MAX_RECURSION_DEPTH;
        let width = lexer.current_line().len() - 1;
        let location = lexer.location_bol();

        let mut expr = Expr::parse(lexer, context, None, None)?;
        expr.evaluate(context, &mut depth, force_eval);

        if depth == 0 {
            Err(Error::new(
                ErrorKind::RecursionLimit, 
                "max recursion depth reached with this expression", 
                location,
                width,
            ))
        } else {
            Ok(Command::Print { expr, location })
        }
    }
}

#[derive(Debug, Default)]
pub struct Context {
    pub quit: bool,
    pub current_file: Option<PathBuf>,
    bindings: HashMap<String, Definition>,
}

impl Context {
    pub fn new(current_file: Option<PathBuf>) -> Context {
        Context {
            quit: false,
            bindings: HashMap::new(),
            current_file
        }
    }

    pub fn has_variable(&self, name: &str) -> bool {
        self.bindings.contains_key(name)   
    }

    pub fn get_definition(&self, name: &str) -> &Definition {
        self.bindings.get(name).expect("bindings must contain the variable")
    }

    pub fn get_variable(&self, name: &str) -> Expr {
        self.get_definition(name).expr.clone()
    }

    pub fn get_required_args(&self, name: &str) -> usize {
        self.get_definition(name).args
    }

    pub fn get_source(&self, name: &str) -> Option<&PathBuf> {
        self.get_definition(name).source.as_ref()
    }

    pub fn get_repr(&self, name: &str) -> &str {
        &self.get_definition(name).repr
    }

    pub fn process_file(&mut self, file_path: PathBuf) {
        let saved_file = self.current_file.clone();
        self.current_file = Some(file_path.clone());
        let source = std::fs::read_to_string(file_path.clone()).unwrap();

        let mut lexer = Lexer::new(source.chars().collect(), Some(file_path));
        while !lexer.is_exhausted() && !self.quit {
            match Command::parse(&mut lexer, self) {
                Ok(command) => self.process_command(command),
                Err(err) => {
                    eprintln!("{}", err);
                    std::process::exit(1);
                }
            }
        }

        self.current_file = saved_file;
    }

    pub fn process_command(&mut self, command: Command) {
        match command {
            Command::Nothing => (),
            Command::Link { file_path } => {
                let file_path = PathBuf::from(file_path.text);
                self.process_file(file_path)
            }
            Command::Repr { combinator } => {
                match combinator.text.as_str() {
                    "I" => println!("I x -> x"),
                    "K" => println!("K x y -> x"),
                    "S" => println!("S f g x -> f x ( g x )"),
                    "B" => println!("B f g x -> f ( g x )"),
                    "C" => println!("C f g x -> f x g"),
                    "Y" => println!("Y f -> f (Y f)"),
                    name => println!("{}", self.get_repr(name))
                }
            }
            Command::Source { combinator } => {
                match combinator.text.as_str() {
                    "I" | "K" | "S" | "B" | "C" | "Y" => println!("[built-in]"),
                    name => if let Some(source) = self.get_source(name) {
                        println!("{}", source.to_str().unwrap())
                    } else {
                        println!("[repl session]")
                    }
                }
            }
            Command::Debug => {
                println!("{:?}", &self.bindings)
            }
            Command::Quit => {
                self.quit = true
            }
            Command::DefineExpr { name, symbols, mut definition } => {
                for symbol in symbols.iter().rev() {
                    definition.expr.remove_symbol(symbol);
                }

                if definition.expr.contains_symbol("#") {
                    definition.expr.remove_symbol("#");
                    definition.expr = Expr::Term(vec![definition.expr, Expr::Combinator(Combinator::Y)])
                }
                self.bindings.insert(name.text, definition);
            }
            Command::Print { expr, location } => {
                match location {
                    Location::File { line, .. } => {
                        println!("{} => {}", line.trim(), expr)
                    }
                    Location::Repl { .. } => {
                        println!(" => {}", expr)
                    }
                }
            }
        }
    }   
}
