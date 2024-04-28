use std::fs;
use crate::error::SKIResult;
use crate::env::Env;

pub const ASSIGN_DELIMETER: &str = ":=";

pub fn interpret(file_path: String) {
    let mut env = Env::new();
    let code = match fs::read_to_string(file_path.clone()) {
        Ok(code) => code,
        Err(_) => {
            eprintln!("FILE `{}` DOES NOT EXIST", file_path);
            std::process::exit(1);
        }
    };

    for (line_count, line) in code.lines().enumerate() {
        match interpret_line(line.trim().to_string(), &mut env) {
            Ok(true) => println!("[{}:{}] {}", file_path, line_count + 1, env),
            Ok(false) => (),
            Err(err) => {
                eprintln!("error: {}:{}:{}\n{}\n{}", file_path, line_count, err.cursor, line, err);
                std::process::exit(1);
            },
        }
    }
}

pub fn interpret_line(line: String, env: &mut Env) -> SKIResult<bool> {
    if line.starts_with(';') || line.is_empty() {
        Ok(false)
    } else if line.contains(ASSIGN_DELIMETER) {
        let (variable, assignment) = line.split_once(ASSIGN_DELIMETER).unwrap();
        let expr = env.parse(assignment)?;
        env.bind(variable.trim().to_string(), expr);
        Ok(false)
    } else {
        let expr = env.parse(line.as_str())?;
        env.update(expr)?;
        Ok(true)
    }
}
