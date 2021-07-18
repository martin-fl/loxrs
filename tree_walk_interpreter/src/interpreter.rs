use crate::ast::{Expr, Literal, Stmt};
use crate::token::{Token, TokenType};

use std::collections::HashMap;

pub(crate) struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub(crate) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(crate) fn define(&mut self, name: String, value: Literal) {
        self.values.insert(name, value);
    }

    pub(crate) fn get<'a>(&self, name: Token<'a>) -> Result<Literal, InterpreterError<'a>> {
        self.values
            .get(name.lexeme)
            .ok_or_else(|| {
                InterpreterError::new(
                    name.clone(),
                    &format!("Undefined variable '{}'", name.lexeme),
                )
            })
            .cloned()
    }

    pub(crate) fn assign<'a>(&mut self, name: Token<'a>, value: Literal) -> Result<(), InterpreterError<'a>>{
        if self.values.contains_key(name.lexeme) {
            self.values.insert(name.lexeme.to_string(), value);
            return Ok(());
        }
        
        Err(InterpreterError::new(name.clone(), &format!("Undefined variable '{}'", name.lexeme)))
    }
}

#[derive(Clone)]
pub(crate) struct InterpreterError<'e> {
    pub token: Token<'e>,
    pub message: String,
}

impl<'e> InterpreterError<'e> {
    pub(crate) fn new(token: Token<'e>, message: &str) -> Self {
        Self {
            token,
            message: message.to_string(),
        }
    }
}

pub(crate) struct Interpreter {
    env: Environment,
}

// TODO: Detail the interpreter errors
impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    pub(crate) fn evaluate<'a>(&mut self, expr: Expr<'a>) -> Result<Literal, InterpreterError<'a>> {
        use Literal::Number;
        match expr {
            Expr::Literal(lit) => Ok(lit),
            Expr::Grouping(box e) => self.evaluate(e),
            Expr::Unary(op, box e) => {
                let e = self.evaluate(e)?;
                match op.ty {
                    TokenType::Minus => {
                        if let Number(x) = e {
                            Ok(Number(-x))
                        } else {
                            Err(InterpreterError::new(op, "Expected a number"))
                        }
                    }
                    TokenType::Bang => match e {
                        Literal::True => Ok(Literal::False),
                        Literal::False | Literal::Nil => Ok(Literal::True),
                        _ => Err(InterpreterError::new(op, "Expected a boolean or nil")),
                    },
                    _ => Err(InterpreterError::new(op, "Operator is not unary")),
                }
            }
            Expr::Binary(box left, op, box right) => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                match op.ty {
                    TokenType::Minus => {
                        if let (Number(x), Number(y)) = (&left, &right) {
                            Ok(Number(x - y))
                        } else {
                            Err(InterpreterError::new(op, "Expected two numbers"))
                        }
                    }
                    TokenType::Slash => {
                        if let (Number(x), Number(y)) = (&left, &right) {
                            Ok(Number(x / y))
                        } else {
                            Err(InterpreterError::new(op, "Expected two numbers"))
                        }
                    }
                    TokenType::Star => {
                        if let (Number(x), Number(y)) = (&left, &right) {
                            Ok(Number(x * y))
                        } else {
                            Err(InterpreterError::new(op, "Expected two numbers"))
                        }
                    }
                    TokenType::Plus => {
                        if let (Number(x), Number(y)) = (&left, &right) {
                            Ok(Number(x + y))
                        } else if let (Literal::String(x), Literal::String(y)) = (&left, &right) {
                            Ok(Literal::String(format!("{}{}", x, y)))
                        } else {
                            Err(InterpreterError::new(
                                op,
                                "Expected either two numbers or two strings",
                            ))
                        }
                    }
                    TokenType::Greater => {
                        if let (Number(x), Number(y)) = (&left, &right) {
                            match x > y {
                                true => Ok(Literal::True),
                                false => Ok(Literal::False),
                            }
                        } else {
                            Err(InterpreterError::new(op, "Expected two numbers"))
                        }
                    }
                    TokenType::GreaterEqual => {
                        if let (Number(x), Number(y)) = (&left, &right) {
                            match x >= y {
                                true => Ok(Literal::True),
                                false => Ok(Literal::False),
                            }
                        } else {
                            Err(InterpreterError::new(op, "Expected two numbers"))
                        }
                    }
                    TokenType::Less => {
                        if let (Number(x), Number(y)) = (&left, &right) {
                            match x < y {
                                true => Ok(Literal::True),
                                false => Ok(Literal::False),
                            }
                        } else {
                            Err(InterpreterError::new(op, "Expected two numbers"))
                        }
                    }
                    TokenType::LessEqual => {
                        if let (Number(x), Number(y)) = (&left, &right) {
                            match x <= y {
                                true => Ok(Literal::True),
                                false => Ok(Literal::False),
                            }
                        } else {
                            Err(InterpreterError::new(op, "Expected two numbers"))
                        }
                    }
                    TokenType::EqualEqual => match left == right {
                        true => Ok(Literal::True),
                        false => Ok(Literal::False),
                    },
                    TokenType::BangEqual => match left != right {
                        true => Ok(Literal::True),
                        false => Ok(Literal::False),
                    },
                    _ => Err(InterpreterError::new(op, "Unknown operator")),
                }
            }
            Expr::Variable(name) => self.env.get(name),
            Expr::Assign(name, box value) => {
                let value = self.evaluate(value)?;
                self.env.assign(name, value.clone())?;
                Ok(value)
            }
            _ => unimplemented!(),
        }
    }

    pub(crate) fn execute<'a>(&mut self, statement: Stmt<'a>) -> Result<(), InterpreterError<'a>> {
        match statement {
            Stmt::Expr(expr) => self.evaluate(expr).map(|_| ()),
            Stmt::Print(expr) => self.evaluate(expr).map(|lit| println!("{}", lit)),
            Stmt::Var(name, init) => {
                let value = match init {
                    Some(expr) => self.evaluate(expr)?,
                    None => Literal::Nil,
                };

                self.env.define(name.lexeme.to_string(), value);
                Ok(())
            }
        }
    }

    pub fn interpret<'a>(&mut self, statements: Vec<Stmt<'a>>) -> Result<(), InterpreterError<'a>> {
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }
}
