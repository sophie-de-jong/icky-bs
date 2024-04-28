use std::io::{stdin, stdout, Write};
use crate::env::Env;
use crate::interpreter::interpret_line;

const INPUT_SYMBOL: &str     = "λ:";
const OUTPUT_SYMBOL: &str    = "=>";

pub fn repl() {
    let mut env = Env::new();
    let mut line = String::new();

    loop {
        print!("{} ", INPUT_SYMBOL);
        let _ = stdout().flush();
        stdin().read_line(&mut line).unwrap();

        match interpret_line(line.clone(), &mut env) {
            Ok(true) => println!("{} {}", OUTPUT_SYMBOL, env),
            Ok(false) => (),
            Err(err) => {
                let width = OUTPUT_SYMBOL.len();
                eprintln!("{:>width$}{}", " ", err)
            },
        }

        line.clear();
    }
}