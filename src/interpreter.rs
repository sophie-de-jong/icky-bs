use std::fs;
use anyhow::Result;

use crate::env::Env;
use crate::parse::parse;

pub const ASSIGN_DELIMETER: &str = ":=";

pub fn interpret(file_path: String) -> Result<()> {
    let mut env = Env::new();
    for (line_count, line) in fs::read_to_string(file_path)?.lines().enumerate() {
        if interpret_line(line.trim().to_string(), &mut env)? {
            println!("[{}] {}", line_count + 1, env);
        }
    }
    Ok(())
}

pub fn interpret_line(line: String, env: &mut Env) -> Result<bool> {
    if line.starts_with(';') || line.is_empty() {
        Ok(false)
    } else if line.contains(ASSIGN_DELIMETER) {
        let (variable, assignment) = line.split_once(ASSIGN_DELIMETER).unwrap();
        let expr = parse(assignment.to_string())?;
        env.bind(variable.trim().to_string(), expr);
        Ok(false)
    } else {
        let expr = parse(line.clone())?;
        env.update(expr)?;
        Ok(true)
    }
}
