use std::fs;
use anyhow::Result;

use crate::env::Env;

pub const ASSIGN_DELIMETER: &str = ":=";

pub fn interpret(file_path: String) -> Result<()> {
    let mut env = Env::new();
    for (line_count, line) in fs::read_to_string(file_path.clone())?.lines().enumerate() {
        if interpret_line(line.trim().to_string(), &mut env)? {
            println!("[{}:{}] {}", file_path, line_count + 1, env);
        }
    }
    Ok(())
}

pub fn interpret_line(line: String, env: &mut Env) -> Result<bool> {
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
