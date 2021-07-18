use crate::ast::{Expr, Literal, Stmt};
use crate::token::{Token, TokenType};

use std::collections::HashMap;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    pub(crate) enclosing: Option<Rc<RefCell<Environment>>>,
    pub(crate) values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn new_within(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.values.insert(name, value);
    }

    pub fn get<'a>(&self, name: Token<'a>) -> Result<Literal, InterpreterError<'a>> {
        if self.values.contains_key(name.lexeme) {
            self.values
                .get(name.lexeme)
                .ok_or_else(|| {
                    InterpreterError::new(
                        name.clone(),
                        &format!("Undefined variable '{}'", name.lexeme),
                    )
                })
                .cloned()
        } else if let Some(enclosing) = &self.enclosing {
            (**enclosing).borrow().get(name)
        } else {
            Err(InterpreterError::new(
                name.clone(),
                &format!("Undefined variable '{}'", name.lexeme),
            ))
        }
    }

    pub fn assign<'a>(
        &mut self,
        name: Token<'a>,
        value: Literal,
    ) -> Result<(), InterpreterError<'a>> {
        if self.values.contains_key(name.lexeme) {
            self.values.insert(name.lexeme.to_string(), value);
            Ok(())
        } else if let Some(enclosing) = &mut self.enclosing {
            (**enclosing).borrow_mut().assign(name, value)
        } else {
            Err(InterpreterError::new(
                name.clone(),
                &format!("Undefined variable '{}'", name.lexeme),
            ))
        }
    }
}

#[derive(Clone)]
pub struct InterpreterError<'e> {
    pub(crate) token: Token<'e>,
    pub(crate) message: String,
}

impl<'e> InterpreterError<'e> {
    pub fn new(token: Token<'e>, message: &str) -> Self {
        Self {
            token,
            message: message.to_string(),
        }
    }
}

pub struct Interpreter {
    pub(crate) env: Rc<RefCell<Environment>>,
}

// TODO: Detail the interpreter errors
impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn evaluate<'a>(&mut self, expr: Expr<'a>) -> Result<Literal, InterpreterError<'a>> {
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
            Expr::Variable(name) => (*self.env).borrow().get(name),
            Expr::Assign(name, box value) => {
                let value = self.evaluate(value)?;
                (*self.env).borrow_mut().assign(name, value.clone())?;
                Ok(value)
            }
        }
    }

    pub fn execute<'a>(&mut self, statement: Stmt<'a>) -> Result<(), InterpreterError<'a>> {
        match statement {
            Stmt::Block(statements) => {
                let previous = Rc::clone(&self.env);
                let env = Environment::new_within(Rc::clone(&self.env));
                self.env = Rc::new(RefCell::new(env));
                for statement in statements {
                    self.execute(statement)?;
                }
                (*self.env).swap(&*previous); 
                Ok(())
            }
            Stmt::Expr(expr) => self.evaluate(expr).map(|_| ()),
            Stmt::Print(expr) => self.evaluate(expr).map(|lit| println!("{}", lit)),
            Stmt::Var(name, init) => {
                let value = match init {
                    Some(expr) => self.evaluate(expr)?,
                    None => Literal::Nil,
                };

                (*self.env)
                    .borrow_mut()
                    .define(name.lexeme.to_string(), value);
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
