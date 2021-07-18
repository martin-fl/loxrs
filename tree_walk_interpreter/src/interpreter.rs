use crate::ast::{Expr, Literal};
use crate::token::{Token,TokenType};

pub(crate) struct InterpreterError<'e> {
    pub(crate) token: Token<'e>,
    pub(crate) message: String,
}

impl<'e> InterpreterError<'e> {
    pub(crate) fn new(token: Token<'e>, message: &str) -> Self {
        Self { token, message: message.to_string() }
    }
}

pub(crate) struct Interpreter;

// TODO: Detail the interpreter errors
impl Interpreter {
    pub(crate) fn evaluate(tree: Expr<'_>) -> Result<Literal, InterpreterError> {
        use Literal::Number;
        match tree {
            Expr::Literal(lit) => Ok(lit),
            Expr::Grouping(box e) => Self::evaluate(e),
            Expr::Unary(op, box e) => {
                let e = Self::evaluate(e)?;
                match op.ty {
                    TokenType::Minus => if let Number(x) = e {
                        Ok(Number(-x))
                    } else {
                        Err(InterpreterError::new(op, "Expected a number"))
                    },
                    TokenType::Bang => match e {
                        Literal::True => Ok(Literal::False),
                        Literal::False | Literal::Nil => Ok(Literal::True),
                        _ => Err(InterpreterError::new(op, "Expected a boolean or nil")),
                    }
                    _ => Err(InterpreterError::new(op, "Operator is not unary"))
                }
            },
            Expr::Binary(box left, op, box right) => {
                let left = Self::evaluate(left)?;
                let right = Self::evaluate(right)?;
                
                match op.ty {
                    TokenType::Minus => if let (Number(x), Number(y)) = (&left,&right) {
                        Ok(Number(x-y))
                    } else {
                        Err(InterpreterError::new(op, "Expected two numbers"))
                    },
                    TokenType::Slash => if let (Number(x), Number(y)) = (&left,&right) {
                        Ok(Number(x/y))
                    } else {
                        Err(InterpreterError::new(op, "Expected two numbers"))
                    },
                    TokenType::Star => if let (Number(x), Number(y)) = (&left,&right) {
                        Ok(Number(x*y))
                    } else {
                        Err(InterpreterError::new(op, "Expected two numbers"))
                    },
                    TokenType::Plus => if let (Number(x), Number(y)) = (&left,&right) {
                        Ok(Number(x+y))
                    } else if let (Literal::String(x), Literal::String(y)) = (&left,&right) {
                        Ok(Literal::String(format!("{}{}", x, y)))
                    } else {
                        Err(InterpreterError::new(op, "Expected either two numbers or two strings"))
                    },
                    TokenType::Greater => if let (Number(x), Number(y)) = (&left,&right) {
                        match x > y {
                            true => Ok(Literal::True),
                            false => Ok(Literal::False),
                        }
                    } else {
                        Err(InterpreterError::new(op, "Expected two numbers"))
                    },
                    TokenType::GreaterEqual => if let (Number(x), Number(y)) = (&left,&right) {
                        match x >= y {
                            true => Ok(Literal::True),
                            false => Ok(Literal::False),
                        }
                    } else {
                        Err(InterpreterError::new(op, "Expected two numbers"))
                    },
                    TokenType::Less => if let (Number(x), Number(y)) = (&left,&right) {
                        match x < y {
                            true => Ok(Literal::True),
                            false => Ok(Literal::False),
                        }
                    } else {
                        Err(InterpreterError::new(op, "Expected two numbers"))
                    },
                    TokenType::LessEqual => if let (Number(x), Number(y)) = (&left,&right) {
                        match x <= y {
                            true => Ok(Literal::True),
                            false => Ok(Literal::False),
                        }
                    } else {
                        Err(InterpreterError::new(op, "Expected two numbers"))
                    },
                    TokenType::EqualEqual => match &left == &right {
                        true => Ok(Literal::True),
                        false => Ok(Literal::False),
                    },
                    TokenType::BangEqual => match &left != &right {
                        true => Ok(Literal::True),
                        false => Ok(Literal::False),
                    },
                    _ => Err(InterpreterError::new(op, "Unknown operator")),
                }
            },
        }
    }

    pub fn interpret(tree: Expr<'_>) -> Result<(), InterpreterError> {
        Interpreter::evaluate(tree).map(|lit| println!("{}", lit))
    }
}
