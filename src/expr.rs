use std::fmt;
use std::rc::Rc;
use crate::context::Context;
use crate::lexer::{Lexer, TokenKind};
use crate::error::{Error, ErrorKind, Result};

// Enum representing the six built-in combinators which can be used
// to translate any expression of lambda calculus to combinatory logic
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Combinator {
    I, // I x     -> x
    K, // K x y   -> x
    S, // S f g x -> f x ( g x )
    B, // B f g x -> f ( g x )
    C, // C f g x -> f x g
    Y, // Y f     -> f (Y f)
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self) // Debug print is the same as display print.
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Combinator(Combinator), // Built-in combinator (i.e. S, K, I)
    Variable(Rc<str>),      // User defined combinators, built from existing ones (i.e. TRUE := K, FALSE := K I)
    Symbol(Rc<str>),        // Free variable with no binding (i.e. f, x, g)
    Term(Vec<Expr>),        // Parenthesized expression (i.e. (K x y), (f (x y)))
}

impl Expr {
    pub fn parse(lexer: &mut Lexer, context: &mut Context, allowed_symbols: Option<&[String]>) -> Result<Expr> {
        let mut term = Vec::new();

        while let Some(token) = lexer.current() {
            if let TokenKind::Eol = token.kind {
                break
            }
            term.push(Expr::parse_term(lexer, context, allowed_symbols)?);
        }

        if term.len() == 1 {
            Ok(term.pop().unwrap())
        } else {
            term.reverse();
            Ok(Expr::Term(term))
        }
    }

    fn parse_term(lexer: &mut Lexer, context: &mut Context, allowed_symbols: Option<&[String]>) -> Result<Expr> {
        let kinds = [
            TokenKind::Symbol, 
            TokenKind::Combinator, 
            TokenKind::OpenParen, 
            TokenKind::CloseParen,
            TokenKind::InvalidChar
        ];
        let start_index = lexer.current_index();
        let token = lexer.expect_tokens(&kinds)?;

        match token.kind {
            TokenKind::Symbol => {
                if allowed_symbols.map_or(true, |v| v.contains(&token.text)) {
                    Ok(Expr::Symbol(Rc::from(token.text)))
                } else {
                    Err(Error::from_token(
                        ErrorKind::NotFound, 
                        "symbol must be taken from assignment arguments",
                        token
                    ))
                }
            },
            TokenKind::Combinator => {
                match token.text.as_str() {
                    "I" => Ok(Expr::Combinator(Combinator::I)),
                    "K" => Ok(Expr::Combinator(Combinator::K)),
                    "S" => Ok(Expr::Combinator(Combinator::S)),
                    "B" => Ok(Expr::Combinator(Combinator::B)),
                    "C" => Ok(Expr::Combinator(Combinator::C)),
                    "Y" => Ok(Expr::Combinator(Combinator::Y)),
                    name => if context.has_variable(name) {
                        Ok(Expr::Variable(Rc::from(name)))
                    } else {
                        Err(Error::from_token(
                            ErrorKind::NotFound,
                            "combinator not found",
                            token
                        ))
                    },
                }
            },
            TokenKind::OpenParen => {
                let mut term = Vec::new();

                while let Some(token) = lexer.current() {
                    if token.kind == TokenKind::CloseParen {
                        lexer.advance();
                        break
                    }
                    term.push(Expr::parse_term(lexer, context, allowed_symbols)?)
                }

                if term.is_empty() {
                    let width = lexer.current_index() - start_index;
                    Err(Error::new(ErrorKind::EmptyTerm, "term cannot be empty", token.location, width))
                } else if term.len() == 1 {
                    Ok(term.pop().unwrap())
                } else {
                    term.reverse();
                    Ok(Expr::Term(term))
                }
            }
            TokenKind::CloseParen => Err(Error::from_token(ErrorKind::UnbalancedParens, "unexpected right paren", token)),
            _ => unreachable!()
        }
    }

    pub fn reduce(&mut self, context: &mut Context, depth: &mut usize) {
        if *depth == 0 {
            return
        }
        *depth -= 1;

        if let Expr::Term(term) = self {
            // Handles the case where brackets are enclosed around
            // a single term, i.e. f (x) -> f x
            if term.len() == 1 {
                *self = term.pop().unwrap();
                return self.reduce(context, depth);
            }

            match term.pop().expect("expression must contain at least one element") {
                // If a term starts with another term, we can safely extract it
                // without losing application order of combinators.
                // (i.e. f ((K I) x y) -> f (K I x y))
                Expr::Term(mut subterm) => {
                    term.append(&mut subterm);
                    self.reduce(context, depth);
                }
                // Variables and combinators will only reduce if they have enough arguments.
                Expr::Variable(name) if context.get_required_args(&name) <= term.len() => {
                    let expr = context.get_variable(&name);
                    term.push(expr);
                    self.reduce(context, depth);
                }
                Expr::Combinator(combinator) if combinator.required_args() <= term.len() => {
                    combinator.apply_rule(term);
                    self.reduce(context, depth);
                }
                // A symbol or combinator that doesn't have enough arguments can safely simplify
                // its arguments individually. 
                // (i.e. S (I x) y -> S x y)
                sym => {
                    for expr in term.iter_mut() {
                        expr.reduce(context, depth);
                    }
                    term.push(sym);
                }
            }
        }
    }

    // Compiles an expression down to a new expression that is logically equivalent to the old
    // one except without the given symbol.
    pub fn remove_symbol(&mut self, symbol: &str) {
        match self {
            Expr::Symbol(symbol_2) if symbol == symbol_2.as_ref() => {
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

                if *first == Expr::Symbol(Rc::from(symbol)) && !expr.contains_symbol(symbol) {
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
            Expr::Symbol(symbol_2) => symbol == symbol_2.as_ref(),
            Expr::Term(term) => term.iter().any(|expr| expr.contains_symbol(symbol)),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Combinator(combinator) => write!(f, "{}", combinator),
            Expr::Variable(name) | Expr::Symbol(name) => write!(f, "{}", name),
            Expr::Term(term) => write!(f, "{}", term_to_string(term)),
        }
    }
}

fn term_to_string(term: &[Expr]) -> String {
    let mut result = String::new();
    for (index, expr) in term.iter().rev().enumerate() {
        match expr {
            Expr::Combinator(combinator) => result.push_str(&combinator.to_string()),
            Expr::Variable(name) | Expr::Symbol(name) => result.push_str(name),
            Expr::Term(subterm) => result.push_str(&format!("({})", term_to_string(subterm))),
        }
        if index < term.len() - 1 {
            result.push(' ')
        }
    }
    result
}
