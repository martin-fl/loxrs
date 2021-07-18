use crate::token::Token;
use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

#[derive(Debug)]
pub(crate) enum Expr<'e> {
    Assign(Token<'e>, Box<Expr<'e>>),
    Binary(Box<Expr<'e>>, Token<'e>, Box<Expr<'e>>),
    Grouping(Box<Expr<'e>>),
    Literal(Literal),
    Unary(Token<'e>, Box<Expr<'e>>),
    Variable(Token<'e>),
}

#[derive(Debug)]
pub(crate) enum Stmt<'s> {
    Expr(Expr<'s>),
    Print(Expr<'s>),
    Var(Token<'s>, Option<Expr<'s>>),
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
            Expr::Variable(tok) => write!(f, "(var {})", tok.lexeme),
            _ => unimplemented!(),
        }
    }
}

impl<'s> fmt::Display for Stmt<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Expr(expr) => {
                write!(f, "(expr {})", expr)
            }
            Stmt::Print(expr) => {
                write!(f, "(print {})", expr)
            }
            _ => unimplemented!(),
        }
    }
}
