mod env;
mod term;
mod lexer;
mod repl;
mod interpreter;

use anyhow::Result;
use std::env::args;
use repl::repl;
use interpreter::interpret;

fn main() -> Result<()> {
    let mut args = args();
    args.next(); // Skip current file name.

    if let Some(file_path) = args.next() {
        interpret(file_path)?;
    } else {
        repl()?
    }

    Ok(())
}
