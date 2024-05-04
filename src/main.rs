mod context;
mod expr;
mod lexer;
mod error;

use std::path::{Path, PathBuf};
use std::env::Args;
use crate::context::Context;

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
            if &arg != "--load" {
                let interpret_path = Path::new(&arg);
                if is_valid_path(interpret_path) {
                    config.file_path = Some(interpret_path.to_path_buf());
                    args.next();
                } else {
                    return Err(format!("ERROR: `{}` does not exist or is not a file ending in .skibc", arg))
                }
            }
            for file_path in args {
                let import_path = Path::new(&file_path);
                if is_valid_path(import_path) {
                    config.imports.push(import_path.to_path_buf())
                } else {
                    return Err(format!("ERROR: `{}` does not exist or is not a file ending in .skibc", file_path))
                }
            }
        }
        
        Ok(config)
    }
}

fn is_valid_path(path: &Path) -> bool {
    path.exists() && path.extension().is_some_and(|ext| ext == "skibc")
}

fn main() -> Result<(), String> {
    let config = Config::build(&mut std::env::args())?;
    let mut context = Context::new(config.imports);

    if let Some(file_path) = config.file_path {
        context.interpret_file(file_path);
    } else {
        context.start_repl();
    }

    Ok(())
}
