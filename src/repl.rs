use std::io::{stdin, stdout, Write};
use anyhow::Result;
use crate::env::Env;
use crate::interpreter::interpret_line;

const INPUT_SYMBOL: &str     = "λ:";
const OUTPUT_SYMBOL: &str    = "=>";

pub fn repl() -> Result<()> {
    let mut env = Env::new();
    let mut line = String::new();

    loop {
        print!("{} ", INPUT_SYMBOL);
        stdout().flush()?;
        stdin().read_line(&mut line)?;
        if interpret_line(line.clone(), &mut env)? {
            println!("{} {}", OUTPUT_SYMBOL, env);
        }
        line.clear();
    }
}