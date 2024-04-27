use crate::term::{Term, Combinator};
use anyhow::{anyhow, Result};

pub fn parse(mut expr_string: String) -> Result<Vec<Term>> {
    let mut expr = Vec::new();
    let mut paren_count: i8 = 0;
    expr_string = expr_string.replace('(', " ( ").replace(')', " ) ");

    for variable in expr_string.split_whitespace().rev() {
        let term = match variable {
            "I" => Term::Combinator(Combinator::I),
            "K" => Term::Combinator(Combinator::K),
            "S" => Term::Combinator(Combinator::S),
            "B" => Term::Combinator(Combinator::B),
            "C" => Term::Combinator(Combinator::C),
            "(" => {
                paren_count += 1;
                Term::LeftParen
            },
            ")" => {
                paren_count -= 1;
                Term::RightParen
            },
            ";" => break,
            ident => {
                if is_valid_combinator(ident) {
                    Term::Combinator(Combinator::Other(ident.to_string()))
                } else if is_valid_variable(ident) {
                    Term::Var(ident.to_string())
                } else {
                    return Err(anyhow!("unexpected term {}", ident))
                }
            },
        };
        expr.push(term);
    }

    if paren_count == 0 {
        Ok(expr)
    } else {
        Err(anyhow!("unbalanced parentheses"))
    }
}

fn is_valid_combinator(combinator: &str) -> bool {
    for ch in combinator.chars() {
        if !matches!(ch, 'A'..='Z' | '0'..='9' | '_') {
            return false
        }
    }
    true
}

fn is_valid_variable(variable: &str) -> bool {
    for ch in variable.chars() {
        if !matches!(ch, 'a'..='z' | '_') {
            return false
        }
    }
    true
}