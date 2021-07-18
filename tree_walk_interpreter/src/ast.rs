use crate::token::Token;
use std::fmt;

#[derive(PartialEq)]
pub(crate) enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

pub(crate) enum Expr<'e> {
    Binary(Box<Expr<'e>>, Token<'e>, Box<Expr<'e>>),
    Grouping(Box<Expr<'e>>),
    Literal(Literal),
    Unary(Token<'e>, Box<Expr<'e>>),
}


impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(x) => write!(f, "{}", x),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl<'e> fmt::Display for Expr<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Binary(box left, tok, box right) => {
                write!(f, "({} {} {})", tok.lexeme, left, right)
            }
            Expr::Grouping(box expr) => {
                write!(f, "(group {})", expr)
            }
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Unary(tok, box expr) => write!(f, "({} {})", tok.lexeme, expr),
        }
    }
}
