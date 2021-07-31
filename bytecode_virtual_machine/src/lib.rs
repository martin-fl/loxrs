#![feature(box_patterns)]

pub mod chunk;
pub mod compiler;
pub mod lexer;
pub mod value;
pub mod vm;
pub mod error;

use lexer::{Token, TokenType};

#[derive(Debug)]
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
        write!(f, "[line {}] Error", self.line)?;

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

const DEBUG: bool = false;

#[macro_export]
macro_rules! define_enum {
    ($name:ident, $($variant:ident = $byte:expr,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(u8)]
        pub enum $name {
            $($variant = $byte,)*
        }

        impl From<u8> for $name {
            fn from(byte: u8) -> Self {
                use $name::*;
                match byte {
                    $($byte => $variant,)*
                    _ => panic!("Unknown OpCode"),
                }
            }
        }
    };
}
