mod context;
mod expr;
mod lexer;
mod error;

use std::path::{Path, PathBuf};
use std::env::Args;
use std::io::{self, Write};
use crate::lexer::Lexer;
use crate::context::*;

fn start_repl(mut context: Context) {
    let mut line = String::new();

    while !context.quit {
        print!("{}> ", FILE_EXTENSION);
        io::stdout().flush().unwrap();

        if io::stdin().read_line(&mut line).unwrap() == 0 {
            break
        }

        let mut lexer = Lexer::new(line.chars().collect(), None);
        let command = Command::parse(&mut lexer, &mut context);
        match command {
            Ok(command) => context.process_command(command),
            Err(err) => {
                eprintln!("{:>width$}{}", " ", err, width = FILE_EXTENSION.len() + 2);
            }
        }

        line.clear();
    }
}

fn interpret(mut context: Context, file_path: PathBuf) {
    context.process_file(file_path);
}

#[derive(Default)]
struct Config {
    file_path: Option<PathBuf>,
}

impl Config {
    fn build(args: &mut Args) -> Result<Config, String> {
        args.next().expect("expected program name in arguments");
        let mut config = Config::default();

        if let Some(arg) = args.next() {
            let interpret_path = Config::build_path(&arg)?;
            config.file_path = Some(interpret_path);
        }
        
        Ok(config)
    }

    fn build_path(arg: &str) -> Result<PathBuf, String> {
        let path = Path::new(arg);

        if path.extension().map_or(true, |ext| ext != FILE_EXTENSION) {
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
    let context = Context::new(config.file_path.clone());

    if let Some(file_path) = config.file_path {
        interpret(context, file_path);
    } else {
        start_repl(context);
    }

    Ok(())
}
