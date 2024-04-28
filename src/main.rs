mod env;
mod term;
mod lexer;
mod repl;
mod interpreter;
mod error;

use std::env::args;
use repl::repl;
use interpreter::interpret;

fn main() {
    let mut args = args();
    args.next().expect("args should always contain a program name");

    if let Some(file_path) = args.next() {
        interpret(file_path);
    } else {
        repl()
    }
}
