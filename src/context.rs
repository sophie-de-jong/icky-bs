use crate::error::*;
use crate::expr::*;
use crate::lexer::*;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::io::{self, Write};

const INPUT_PROMPT: &str = "skibc>";
const OUTPUT_PROMPT: &str = " =>";
const MAX_RECURSION_DEPTH: usize = 1024;

#[derive(Debug, Default)]
pub struct Context {
    bindings: HashMap<String, (Expr, usize)>,
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

    pub fn get_variable(&self, name: &str) -> Expr {
        self.bindings.get(name).map(|(var, _)| var.clone()).expect("bindings must contain the variable")
    }

    pub fn get_required_args(&self, name: &str) -> usize {
        self.bindings.get(name).map(|(_, args)| *args).expect("bindings must contain the variable")
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

            if io::stdin().read_line(&mut line).unwrap() == 0 {
                break
            }
    
            let mut lexer = Lexer::new(line.chars().collect(), None);
            if let Err(err) = self.evaluate_line(&mut lexer) {
                eprintln!("{}{}", " ".repeat(INPUT_PROMPT.len() + 1), err);
            }

            line.clear();
        }
    }

    pub fn evaluate_line(&mut self, lexer: &mut Lexer) -> SKIResult<()> {
        if lexer.current_line().contains(":=") {
            let mut assignment = Assignment::parse(lexer, self)?;
            assignment.compile();
            let args = assignment.required_args();
            self.bindings.insert(assignment.name, (assignment.expr, args));
        } else {
            let start_loc = lexer.loc();
            let length = lexer.current_line_length();
            let mut expr = Expr::parse(lexer, self, None)?;
            let mut depth = MAX_RECURSION_DEPTH;
            expr.reduce(self, &mut depth);

            if depth == 0 {
                let width = length - start_loc.column() - 1;
                return Err(SKIError::new("max recursion depth reached", start_loc, width));
            }

            println!("{} {}", OUTPUT_PROMPT, expr);
        }

        Ok(())
    }   
}
