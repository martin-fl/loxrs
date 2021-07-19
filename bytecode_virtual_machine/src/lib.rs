pub mod chunk;
pub mod compiler;
pub mod scanner;
pub mod value;
pub mod vm;

use scanner::{Token, TokenType};

pub struct LoxError {
    message: &'static str,
    line: usize,
    token: Option<Token>,
}

impl LoxError {
    pub fn new(message: &'static str, line: usize) -> Self {
        Self {
            message,
            line,
            token: None,
        }
    }

    pub fn from_token(message: &'static str, token: Token) -> Self {
        Self {
            message,
            line: token.line,
            token: Some(token),
        }
    }
}

use std::fmt;

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {} Error", self.line)?;

        match self.token {
            Some(tok) => match tok.ty {
                TokenType::EOF => write!(f, " at end")?,
                _ => {}
            },
            None => {}
        }

        writeln!(f, ": {}", self.message)
    }
}

const DEBUG: bool = true;
