use std::fmt;
use crate::context::Context;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::error::{SKIError, SKIResult};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Combinator {
    I,
    K,
    S,
    B,
    C,
    Y,
}

impl Combinator {
    fn required_args(&self) -> usize {
        match self {
            Combinator::I | Combinator::Y                 => 1,
            Combinator::K                                 => 2,
            Combinator::S | Combinator::B | Combinator::C => 3,
        }
    }

    fn apply_rule(&self, term: &mut Vec<Expr>) {
        match self {
            // I x -> x
            Combinator::I => (),
            // K x y -> x
            Combinator::K => {
                let x = term.pop().expect("missing x in K");
                term.pop().expect("missing y in K"); // Skip
                term.push(x);
            }
            // S f g x -> f x ( g x )
            Combinator::S => {
                let f = term.pop().expect("missing f in S");
                let g = term.pop().expect("missing g in S");
                let x = term.pop().expect("missing x in S");
                term.push(Expr::Term(vec![x.clone(), g]));
                term.push(x);
                term.push(f);
            }
            // B f g x -> f ( g x )
            Combinator::B => {
                let f = term.pop().expect("missing f in B");
                let g = term.pop().expect("missing g in B");
                let x = term.pop().expect("missing x in B");
                term.push(Expr::Term(vec![x, g]));
                term.push(f);
            }
            // C f g x -> f x g
            Combinator::C => {
                let f = term.pop().expect("missing f in C");
                let g = term.pop().expect("missing g in C");
                let x = term.pop().expect("missing x in C");
                term.push(g);
                term.push(x);
                term.push(f);
            }
            // Y f -> f (Y f)
            Combinator::Y => {
                let f = term.pop().expect("missing f in Y");
                term.push(Expr::Term(vec![f.clone(), Expr::Combinator(Combinator::Y)]));
                term.push(f);
            }
        }
    }
}

impl fmt::Display for Combinator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Combinator::I => write!(f, "I"),
            Combinator::K => write!(f, "K"),
            Combinator::S => write!(f, "S"),
            Combinator::B => write!(f, "B"),
            Combinator::C => write!(f, "C"),
            Combinator::Y => write!(f, "Y"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Combinator(Combinator), // Built-in combinator (i.e. S, K, I)
    Variable(String),       // User defined combinators, built-from existing ones (i.e. TRUE := K, FALSE := K I)
    Symbol(String),         // Free variable with no binding (i.e. f, x, g)
    Term(Vec<Expr>),        // Parenthesized expression (i.e. (K x y), (f (x y)))
}

impl Expr {
    pub fn parse(lexer: &mut Lexer, context: &mut Context, allowed_symbols: Option<&[String]>) -> SKIResult<Expr> {
        let mut term = Vec::new();
        let start_loc = lexer.loc();

        while let Some(token) = lexer.next_token() {
            if let TokenKind::Eol = token.kind {
                break
            }
            term.push(Expr::parse_term(token, lexer, context, allowed_symbols)?)
        }

        if term.is_empty() {
            let width = start_loc.width_from(&lexer.loc());
            Err(SKIError::new("term cannot be empty", start_loc, width))
        } else if term.len() == 1 {
            Ok(term.pop().unwrap())
        } else {
            term.reverse();
            Ok(Expr::Term(term))
        }
    }

    fn parse_term(token: Token, lexer: &mut Lexer, context: &mut Context, allowed_symbols: Option<&[String]>) -> SKIResult<Expr> {
        match token.kind {
            TokenKind::Ident if is_valid_symbol(&token.text) => {
                if allowed_symbols.map_or(true, |v| v.contains(&token.text)) {
                    Ok(Expr::Symbol(token.text))
                } else {
                    Err(SKIError::new("undefined symbol", token.loc, token.text.len()))
                }
            },
            TokenKind::Ident if is_valid_combinator(&token.text) => {
                match token.text.as_str() {
                    "I" => Ok(Expr::Combinator(Combinator::I)),
                    "K" => Ok(Expr::Combinator(Combinator::K)),
                    "S" => Ok(Expr::Combinator(Combinator::S)),
                    "B" => Ok(Expr::Combinator(Combinator::B)),
                    "C" => Ok(Expr::Combinator(Combinator::C)),
                    "Y" => Ok(Expr::Combinator(Combinator::Y)),
                    name => if context.has_variable(name) {
                        Ok(Expr::Variable(token.text))
                    } else {
                        Err(SKIError::new("combinator doesn't exist", token.loc, token.text.len()))
                    },
                }
            },
            TokenKind::Ident | TokenKind::Invalid => Err(SKIError::new("bad identifier", token.loc, token.text.len())),
            TokenKind::OpenParen => {
                let mut term = Vec::new();
                let mut found_close_paren = false;

                while let Some(token) = lexer.next_token() {
                    if token.is_kind(TokenKind::CloseParen) {
                        found_close_paren = true;
                        break
                    }
                    term.push(Expr::parse_term(token, lexer, context, allowed_symbols)?)
                }

                if !found_close_paren {
                    return Err(SKIError::new("expected closing paren", lexer.loc(), 1))
                }

                if term.is_empty() {
                    Err(SKIError::new("term cannot be empty", token.loc.clone(), token.loc.width_from(&lexer.loc())))
                } else if term.len() == 1 {
                    Ok(term.pop().unwrap())
                } else {
                    term.reverse();
                    Ok(Expr::Term(term))
                }
            },
            _ => Err(SKIError::new("unexpected token", token.loc, token.text.len()))
        }
    }

    pub fn reduce(&mut self, context: &mut Context, depth: usize) {
        if depth == 0 {
            panic!("max recursion depth for reduce")
        }

        match self {
            Expr::Symbol(_) | Expr::Combinator(_) => (),
            Expr::Variable(name) => {
                *self = context.get_variable(name).expect("bindings must contain the variable");
                self.reduce(context, depth - 1);
            }
            Expr::Term(term) => {
                // Handles the case where brackets are enclosed around
                // a single term, i.e. f (x) -> f x
                if term.len() == 1 {
                    *self = term.pop().unwrap();
                    return self.reduce(context, depth - 1);
                }

                match term.pop().expect("expression must contain at least one element") {
                    // If a term starts with another term, we can safely extract it
                    // without losing application order of combinators.
                    // (i.e. f ((K I) x y) -> f (K x y))
                    Expr::Term(mut subterm) => {
                        term.append(&mut subterm);
                        self.reduce(context, depth - 1);
                    }
                    // Always reduce variables down to their definitions.
                    Expr::Variable(name) => {
                        let expr = context.get_variable(&name).expect("bindings must contain the variable");
                        term.push(expr);
                        self.reduce(context, depth - 1);
                    }
                    Expr::Combinator(combinator) if combinator.required_args() <= term.len() => {
                        combinator.apply_rule(term);
                        self.reduce(context, depth - 1);
                    }
                    // A symbol or combinator that doesn't have enough arguments can safely simplify
                    // its arguments individually. 
                    // (i.e. S (I x) y -> S x y)
                    sym @ Expr::Combinator(_) | sym @ Expr::Symbol(_) => {
                        for expr in term.iter_mut() {
                            expr.reduce(context, depth - 1);
                        }
                        term.push(sym);
                    }
                }
            }
        }
    }

    // Compiles an expression down to a new expression that is logically equivalent to the old
    // one except without any symbols.
    pub fn remove_symbols(&mut self, mut symbols: Vec<String>) {
        while let Some(symbol) = symbols.pop() {
            self.remove_symbol(&symbol);
        }
    }

    fn remove_symbol(&mut self, symbol: &str) {
        match self {
            Expr::Symbol(symbol_2) if symbol == *symbol_2 => {
                *self = Expr::Combinator(Combinator::I)
            }
            Expr::Combinator(_) | Expr::Variable(_) | Expr::Symbol(_) => {
                *self = Expr::Term(vec![
                    self.clone(), 
                    Expr::Combinator(Combinator::K)
                ])
            }
            Expr::Term(term) => {
                if term.len() == 1 {
                    *self = term.pop().unwrap();
                    return self.remove_symbol(symbol)
                }

                let mut expr = Expr::Term(term.drain(1..).collect());                
                let first = term.first_mut().unwrap();

                if *first == Expr::Symbol(symbol.to_string()) && !expr.contains_symbol(symbol) {
                    term.pop();
                    term.push(expr);
                } else if !expr.contains_symbol(symbol) && first.contains_symbol(symbol) {
                    first.remove_symbol(symbol);
                    term.push(expr);
                    term.push(Expr::Combinator(Combinator::B));
                } else if expr.contains_symbol(symbol) && !first.contains_symbol(symbol) {
                    expr.remove_symbol(symbol);
                    term.push(expr);
                    term.push(Expr::Combinator(Combinator::C));
                } else {
                    first.remove_symbol(symbol);
                    expr.remove_symbol(symbol);
                    term.push(expr);
                    term.push(Expr::Combinator(Combinator::S));
                };
            }
        }
    }

    fn contains_symbol(&self, symbol: &str) -> bool {
        match self {
            Expr::Combinator(_) | Expr::Variable(_) => false,
            Expr::Symbol(symbol_2) => symbol == *symbol_2,
            Expr::Term(term) => {
                for expr in term {
                    if expr.contains_symbol(symbol) {
                        return true
                    }
                }
                false
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Combinator(combinator) => write!(f, "{}", combinator),
            Expr::Variable(name) | Expr::Symbol(name) => write!(f, "{}", name),
            Expr::Term(expr) => write!(f, "{}", term_to_string(expr)),
        }
    }
}

fn term_to_string(expr: &[Expr]) -> String {
    let mut result = String::new();
    for (index, term) in expr.iter().rev().enumerate() {
        match term {
            Expr::Combinator(combinator) => result.push_str(&combinator.to_string()),
            Expr::Variable(name) | Expr::Symbol(name) => result.push_str(name),
            Expr::Term(expr) => result.push_str(&format!("({})", term_to_string(expr))),
        }
        if index + 1 < expr.len() {
            result.push(' ')
        }
    }
    result
}

pub fn is_valid_combinator(combinator: &str) -> bool {
    for ch in combinator.chars() {
        if !matches!(ch, 'A'..='Z' | '0'..='9' | '_') {
            return false
        }
    }
    true
}

pub fn is_valid_symbol(symbol: &str) -> bool {
    for ch in symbol.chars() {
        if !matches!(ch, 'a'..='z' | '_') {
            return false
        }
    }
    true
}