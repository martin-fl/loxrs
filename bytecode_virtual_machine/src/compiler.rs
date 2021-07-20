use crate::chunk::{Chunk, OpCode};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;
use crate::LoxError;
use crate::DEBUG;

use crate::define_enum;

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

// TODO split the compiler into a Parser and a Generator, instead of doing everything in it

pub struct Compiler<'c> {
    // Compiler related fields
    compiling_chunk: Rc<RefCell<Chunk>>,

    // Parsing related fields
    scanner: Scanner<'c>,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
}

// Compiling impls
impl<'c> Compiler<'c> {
    pub fn new(source: &'c str) -> Self {
        Self {
            compiling_chunk: Rc::new(RefCell::new(Chunk::new())),

            scanner: Scanner::new(source),
            current: Token::new(TokenType::EOF, 0, 0, 0),
            previous: Token::new(TokenType::EOF, 0, 0, 0),
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(&'c mut self, chunk: Chunk) -> Result<Chunk, LoxError> {
        self.compiling_chunk = Rc::new(RefCell::new(chunk));

        self.advance();
        self.expression();
        self.consume_until(TokenType::EOF, "Expected end of expression.");

        if self.had_error {
            return Err(LoxError::from_token(
                "Error during compilation.",
                self.current,
            ));
        }

        self.emit_return();
        if DEBUG && !self.had_error {
            self.current_chunk().deref().borrow().disassemble("code");
        }
        Ok((*self.compiling_chunk).take())
    }

    fn current_chunk(&self) -> Rc<RefCell<Chunk>> {
        Rc::clone(&self.compiling_chunk)
    }

    fn emit_byte(&mut self, byte: u8) {
        let line = self.previous.line;
        (*self.current_chunk()).borrow_mut().push_byte(byte, line)
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let constant = (*self.current_chunk()).borrow_mut().add_constant(value);
        if constant > std::u8::MAX as usize {
            self.report_error(LoxError::from_token(
                "Too many constants in one chunk.",
                self.current,
            ));
            0
        } else {
            constant as u8
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant as u8, constant);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return as u8)
    }
}

// Parsing impls
impl<'c> Compiler<'c> {
    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            match self.scanner.scan_token() {
                Ok(current) => {
                    self.current = current;
                    break;
                }
                Err(e) => self.report_error(e),
            }
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume_until(TokenType::RightParen, "Expected ')' after expression.");
    }

    fn number(&mut self) {
        let value = self.scanner.source
            [self.previous.start..(self.previous.start + self.previous.len)]
            .parse::<f64>()
            .expect("Couldn't parse as f64");
        self.emit_constant(Value::Number(value));
    }

    fn unary(&mut self) {
        let op_ty = self.previous.ty;

        self.parse_precedence(Precedence::Unary);

        match op_ty {
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            _ => return,
        }
    }

    fn binary(&mut self) {
        let op_ty = self.previous.ty;
        let rule = Self::get_rule(op_ty);
        self.parse_precedence((rule.prec as u8 + 1).into());

        match op_ty {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Substract as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
            _ => unreachable!(),
        }
    }

    fn literal(&mut self) {
        match self.previous.ty {
            TokenType::False => self.emit_byte(OpCode::False as u8),
            TokenType::Nil => self.emit_byte(OpCode::Nil as u8),
            TokenType::True => self.emit_byte(OpCode::True as u8),
            _ => unreachable!(),
        }
    }

    fn parse_precedence(&mut self, prec: Precedence) {
        self.advance();
        let prefix_rule = Self::get_rule(self.previous.ty).prefix;
        match prefix_rule {
            Some(rule) => {
                rule(self);
                while prec as u8 <= Self::get_rule(self.current.ty).prec as u8 {
                    self.advance();
                    let infix_rule = Self::get_rule(self.previous.ty).infix;
                    (infix_rule.unwrap())(self);
                }
            }
            None => {
                println!("{}, {:?}", &self.scanner.source[self.previous.start..(self.previous.start + self.previous.len)], self.previous);
                self.report_error(LoxError::from_token("Expected expression.", self.current));
                return;
            }
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn consume_until(&mut self, ty: TokenType, message: &'static str) {
        if self.current.ty == ty {
            self.advance();
        } else {
            self.report_error(LoxError::from_token(message, self.current));
        }
    }

    fn report_error(&mut self, e: LoxError) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprintln!("{}", e);
        self.had_error = true;
    }

    fn get_rule(op: TokenType) -> ParseRule<'c> {
        use TokenType::*;
        match op {
            LeftParen => ParseRule {
                prefix: Some(Box::new(Self::grouping)),
                infix: None,
                prec: Precedence::None,
            },
            Minus => ParseRule {
                prefix: Some(Box::new(Self::unary)),
                infix: Some(Box::new(Self::binary)),
                prec: Precedence::Term,
            },
            Plus => ParseRule {
                prefix: None,
                infix: Some(Box::new(Self::binary)),
                prec: Precedence::Term,
            },
            Slash | Star => ParseRule {
                prefix: None,
                infix: Some(Box::new(Self::binary)),
                prec: Precedence::Factor,
            },
            Number => ParseRule {
                prefix: Some(Box::new(Self::number)),
                infix: None,
                prec: Precedence::Factor,
            },
            True | False | Nil => ParseRule {
                prefix: Some(Box::new(Self::literal)),
                infix: None,
                prec: Precedence::None,
            },
            _ => ParseRule {
                prefix: None,
                infix: None,
                prec: Precedence::None,
            },
        }
    }
}

define_enum! {
    Precedence,
    None = 1,
    Assignment = 2,
    Or = 3,
    And = 4,
    Equality = 5,
    Comparison = 6,
    Term = 7,
    Factor = 8,
    Unary = 9,
    Call = 10,
    Primary = 11,
}

struct ParseRule<'r> {
    prefix: Option<Box<dyn Fn(&'_ mut Compiler<'r>) + 'r>>,
    infix: Option<Box<dyn Fn(&'_ mut Compiler<'r>) + 'r>>,
    prec: Precedence,
}
