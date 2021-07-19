use crate::token::Token;
use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

#[derive(Debug, Clone)]
pub enum Expr<'e> {
    Assign(Token<'e>, Box<Expr<'e>>),
    Binary(Box<Expr<'e>>, Token<'e>, Box<Expr<'e>>),
    Grouping(Box<Expr<'e>>),
    Literal(Literal),
    Logical(Box<Expr<'e>>, Token<'e>, Box<Expr<'e>>),
    Unary(Token<'e>, Box<Expr<'e>>),
    Variable(Token<'e>),
}

#[derive(Debug, Clone)]
pub enum Stmt<'s> {
    Block(Vec<Stmt<'s>>),
    Expr(Expr<'s>),
    If(Expr<'s>, Box<Stmt<'s>>, Option<Box<Stmt<'s>>>),
    Print(Expr<'s>),
    Var(Token<'s>, Option<Expr<'s>>),
    While(Expr<'s>, Box<Stmt<'s>>),
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
