mod context;
mod expr;
mod lexer;
mod error;

use std::path::{Path, PathBuf};
use std::env::Args;
use std::io::{self, Write};
use crate::lexer::Lexer;
use crate::context::Context;

const FILE_EXTENSION: &str = "skibc";
const INPUT_PROMPT: &str = "skibc>";

fn start_repl(mut context: Context) {
    let mut line = String::new();

    loop {
        print!("{} ", INPUT_PROMPT);
        io::stdout().flush().unwrap();

        if io::stdin().read_line(&mut line).unwrap() == 0 {
            break
        }

        let mut lexer = Lexer::new(line.chars().collect(), None);
        if let Err(err) = context.evaluate_line(&mut lexer) {
            eprintln!("{}{}", " ".repeat(INPUT_PROMPT.len() + 1), err);
        }

        line.clear();
    }
}

#[derive(Default)]
struct Config {
    file_path: Option<PathBuf>,
    imports: Vec<PathBuf>,
}

impl Config {
    fn build(args: &mut Args) -> Result<Config, String> {
        args.next().expect("expected program name in arguments");
        let mut config = Config::default();

        if let Some(arg) = args.next() {
            if arg.starts_with("--") {
                config.process_arg(&arg, args)?;
            } else {
                let interpret_path = Config::build_path(&arg)?;
                config.file_path = Some(interpret_path);

                if let Some(arg) = args.next() {
                    config.process_arg(&arg, args)?;
                }
            }
        }
        
        Ok(config)
    }

    fn process_arg(&mut self, arg: &str, args: &mut Args) -> Result<(), String> {
        match arg {
            "--load" => {
                for arg in &mut *args {
                    if arg.starts_with("--") {
                        return self.process_arg(&arg, args)
                    }

                    let import_path = Config::build_path(&arg)?;
                    self.imports.push(import_path)
                }
                Ok(())
            }
            bad_arg => Err(format!("unexpected argument `{}`", bad_arg)),
        }
    }

    fn build_path(arg: &str) -> Result<PathBuf, String> {
        let path = Path::new(arg);

        if path.extension().map_or(true, |ext| ext != "skibc") {
            Err(format!("file '{}' does not end in .{}", arg, FILE_EXTENSION))
        } else if !path.exists() {
            Err(format!("file '{}' does not exist in the current directory", arg))
        } else {
            Ok(path.to_path_buf())
        }
    }
}

fn main() -> Result<(), String> {
    let config = Config::build(&mut std::env::args())?;
    let mut context = Context::new(config.imports);

    if let Some(file_path) = config.file_path {
        context.interpret_file(file_path);
    } else {
        start_repl(context);
    }

    Ok(())
}
