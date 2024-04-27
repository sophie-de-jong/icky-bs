#[derive(Debug, Clone)]
pub enum Combinator {
    I,
    K,
    S,
    B,
    C,
    Y,
    Other(String)
}

#[derive(Debug, Clone)]
pub enum Term {
    Combinator(Combinator),
    Var(String),
    LeftParen,
    RightParen,
}
