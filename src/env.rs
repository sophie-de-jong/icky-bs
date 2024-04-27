use crate::term::{Combinator, Term};
use crate::lexer::{Lexer, Token};
use anyhow::{Result, anyhow};
use std::collections::HashMap;
use std::fmt;

const MAX_EXPR_SIZE: usize = 256;

#[derive(Debug, Default)]
pub struct Env {
    bindings: HashMap<String, Vec<Term>>,
    expr: Vec<Term>,
}

impl Env {
    pub fn new() -> Env {
        Env::default()
    }

    pub fn bind(&mut self, variable: String, expr: Vec<Term>) {
        self.bindings.insert(variable, expr);
    }

    pub fn parse(&mut self, expr_string: &str) -> Result<Vec<Term>> {
        let mut expr = Vec::new();
        let mut lexer = Lexer::new(expr_string);
        let mut paren_count: i8 = 0;
    
        while let Some(token) = lexer.next_token() {
            let term = match token {
                Token::LeftParen => {
                    paren_count += 1;
                    Term::LeftParen
                },
                Token::RightParen => {
                    paren_count -= 1;
                    Term::RightParen
                },
                Token::Ident(combinator) if Lexer::is_valid_combinator(&combinator) => {
                    let combinator = match combinator.as_str() {
                        "I" => Combinator::I,
                        "K" => Combinator::K,
                        "S" => Combinator::S,
                        "B" => Combinator::B,
                        "C" => Combinator::C,
                        "Y" => Combinator::Y,
                        k => if self.bindings.contains_key(k) {
                            Combinator::Other(combinator)
                        } else {
                            return Err(anyhow!("no combinator `{}` exists", k))
                        },
                    };
                    Term::Combinator(combinator)
                },
                Token::Ident(variable) if Lexer::is_valid_variable(&variable) => Term::Var(variable),
                Token::Ident(bad_ident) => return Err(anyhow!("bad identifier `{}`", bad_ident)),
            };
            expr.push(term);

            if paren_count < 0 {
                return Err(anyhow!("unexpected right parenthesis [{}]", lexer.get_cursor()))
            }
        }
    
        if paren_count > 0 {
            Err(anyhow!("missing right parenthesis [{}]", lexer.get_cursor()))
        } else {
            expr.reverse();
            Ok(expr)
        }
    }

    pub fn update(&mut self, expr: Vec<Term>) -> Result<()> {
        self.expr = expr;
        self.reduce(true)
    }

    fn reduce(&mut self, collapse_parens: bool) -> Result<()> {
        let Some(term) = self.expr.pop() else {
            return Ok(())
        };

        if self.expr.len() >= MAX_EXPR_SIZE {
            return Err(anyhow!("max expression limit hit"));
        }

        match term {
            Term::Combinator(combinator) => self.reduce_combinator(combinator, collapse_parens),
            Term::Var(variable) => {
                self.reduce(false)?;
                self.expr.push(Term::Var(variable));
                Ok(())
            }
            Term::LeftParen => {
                self.reduce(true)?;

                // Find the index of the matching right parenthesis.
                let mut paren_index = self.expr.len() - 1;
                let mut n_args: u8 = 0;
                while let Some(term) = self.expr.get(paren_index) {
                    match term {
                        Term::Combinator(_) | Term::Var(_) => n_args += 1,
                        Term::LeftParen => {
                            paren_index -= 1;
                            while let Some(term) = self.expr.get(paren_index) {
                                if let Term::RightParen = term {
                                    n_args += 1;
                                    break
                                }
                                paren_index -= 1;
                            }
                        }
                        Term::RightParen => break,
                    }
                    paren_index -= 1;
                }

                // Calculate whether brackets need to be removed.
                if n_args == 1 || collapse_parens {
                    self.expr.remove(paren_index);
                    self.reduce(true)?;
                } else {
                    self.expr.push(Term::LeftParen);
                }

                Ok(())
            }
            Term::RightParen => {
                self.expr.push(Term::RightParen);
                Ok(())
            },
        }
    }

    fn reduce_combinator(&mut self, combinator: Combinator, collapse_parens: bool) -> Result<()> {
        match combinator {
            Combinator::I if self.expect_args(1) => return self.reduce(collapse_parens),
            Combinator::K if self.expect_args(2) => self.k_combinator(),
            Combinator::S if self.expect_args(3) => self.s_combinator(),
            Combinator::B if self.expect_args(3) => self.b_combinator(),
            Combinator::C if self.expect_args(3) => self.c_combinator(),
            Combinator::Y if self.expect_args(1) => self.y_combinator(),
            Combinator::Other(combinator) => {
                if let Some(expr) = self.bindings.get(&combinator) {
                    self.expr.append(&mut expr.clone());
                    return self.reduce(collapse_parens)
                } else {
                    return Err(anyhow!("no combinator {} found", combinator))
                }
            },
            _ => {
                self.expr.push(Term::Combinator(combinator));
                return Ok(())
            }
        }
        self.reduce(collapse_parens)
    }

    // K x y -> x
    fn k_combinator(&mut self) {
        let mut x = self.next_term();
        let _y = self.next_term(); // Skip.

        self.expr.append(&mut x);
    }

    // S f g x -> f x ( g x )
    fn s_combinator(&mut self) {
        let mut f = self.next_term();
        let mut g = self.next_term();
        let mut x = self.next_term();

        self.expr.push(Term::RightParen);
        self.expr.append(&mut x.clone());
        self.expr.append(&mut g);
        self.expr.push(Term::LeftParen);
        self.expr.append(&mut x);
        self.expr.append(&mut f);
    }

    // B f g x -> f ( g x )
    fn b_combinator(&mut self) {
        let mut f = self.next_term();
        let mut g = self.next_term();
        let mut x = self.next_term();

        self.expr.push(Term::RightParen);
        self.expr.append(&mut x);
        self.expr.append(&mut g);
        self.expr.push(Term::LeftParen);
        self.expr.append(&mut f);
    }

    // C f g x -> f x g
    fn c_combinator(&mut self) {
        let mut f = self.next_term();
        let mut g = self.next_term();
        let mut x = self.next_term();

        self.expr.append(&mut g);
        self.expr.append(&mut x);
        self.expr.append(&mut f);
    }

    // Y f -> f (Y f)
    fn y_combinator(&mut self) {
        let mut f = self.next_term();

        self.expr.push(Term::RightParen);
        self.expr.append(&mut f.clone());
        self.expr.push(Term::Combinator(Combinator::Y));
        self.expr.push(Term::LeftParen);
        self.expr.append(&mut f);
    }

    // Returns true if there are n different terms before a right parenthesis
    // or end of expression.
    fn expect_args(&self, mut n: u8) -> bool {
        let mut index = self.expr.len();
        while index > 0 && n > 0 {
            index -= 1;
            match self.expr.get(index).unwrap() {
                Term::Combinator(_) | Term::Var(_) => n -= 1,
                Term::LeftParen => {
                    let mut paren_count: u8 = 1;
                    index -= 1;
                    while let Some(term) = self.expr.get(index) {
                        match term {
                            Term::LeftParen => paren_count += 1,
                            Term::RightParen => paren_count -= 1,
                            _ => (),
                        }
                        if paren_count == 0 {
                            n -= 1;
                            break;
                        }
                        index -= 1;
                    }
                }
                Term::RightParen => return false,
            }
        };
        n == 0
    }

    fn next_term(&mut self) -> Vec<Term> {
        match self.expr.pop().expect("no term exists") {
            term @ Term::Combinator(_) | term @ Term::Var(_) => vec![term],
            term @ Term::LeftParen => {
                let mut result = vec![term];
                let mut paren_count: u8 = 1;
                while let Some(term) = self.expr.pop() {
                    match term {
                        Term::LeftParen => paren_count += 1,
                        Term::RightParen => paren_count -= 1,
                        _ => (),
                    }
                    result.push(term);
                    if paren_count == 0 {
                        break;
                    }
                }
                result.reverse();
                result
            }
            Term::RightParen => unreachable!()
        }
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for term in self.expr.iter().rev() {
            let string = match term {
                Term::Combinator(Combinator::I) => "I",
                Term::Combinator(Combinator::K) => "K",
                Term::Combinator(Combinator::S) => "S",
                Term::Combinator(Combinator::B) => "B",
                Term::Combinator(Combinator::C) => "C",
                Term::Combinator(Combinator::Y) => "Y",
                Term::Combinator(Combinator::Other(combinator)) => combinator,
                Term::Var(ident) => ident,
                Term::LeftParen => "(",
                Term::RightParen => ")",
            };
            write!(f, "{} ", string)?;
        }
        write!(f, "")
    }
}