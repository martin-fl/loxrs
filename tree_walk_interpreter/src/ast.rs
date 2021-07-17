use crate::token::{Token, TokenType};
use std::fmt;

pub(crate) enum Expr<'e> {
    Binary(Box<Expr<'e>>, Token<'e>, Box<Expr<'e>>),
    Grouping(Box<Expr<'e>>),
    Literal(Literal<'e>),
    Unary(Token<'e>, Box<Expr<'e>>),
}

pub(crate) enum Literal<'lit> {
    Number(f64),
    String(&'lit str),
    True,
    False,
    Nil,
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
            Expr::Literal(lit) => match lit {
                Literal::Number(x) => write!(f, "{}", x),
                Literal::String(s) => write!(f, "{}", s),
                Literal::True => write!(f, "true"),
                Literal::False => write!(f, "false"),
                Literal::Nil => write!(f, "nil"),
            },
            Expr::Unary(tok, box expr) => write!(f, "({} {})", tok.lexeme, expr),
        }
    }
}
