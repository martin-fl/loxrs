use crate::chunk::{OpCode, Chunk};
use crate::scanner::{Scanner, Token, TokenType};
use crate::LoxError;

use std::rc::Rc;
use std::cell::RefCell;

pub struct Compiler<'c> {
    compiling_chunk: Rc<RefCell<Chunk>>,
    parser: Parser<'c>,
}

impl<'c> Compiler<'c> {
    pub fn new(source: &'c str) -> Self {
        Self {
            compiling_chunk: Rc::new(RefCell::new(Chunk::new())),
            parser: Parser::new(source)
        }
    }

    pub fn compile(&'c mut self, chunk: Chunk) -> Result<Chunk, LoxError> {
        self.compiling_chunk = Rc::new(RefCell::new(chunk));

        self.parser.advance();
        self.parser.expression();
        self.parser.consume_until(TokenType::EOF, "Expected end of expression.");

        if self.parser.had_error {
            return Err(LoxError::from_token(
                "Error during compilation.",
                self.parser.current,
            ));
        } 

        self.emit_return();
        Ok((*self.compiling_chunk).take())
    }

    fn current_chunk(&self) -> Rc<RefCell<Chunk>> {
        Rc::clone(&self.compiling_chunk)
    }

    fn emit_byte(&self, byte: u8) {
        let line = self.parser.previous.line;
        (*self.current_chunk()).borrow_mut().push_byte(byte, line)
    }

    fn emit_bytes(&self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_return(&self) {
        self.emit_byte(OpCode::Return as u8)
    }
}

struct Parser<'par> {
    scanner: Scanner<'par>,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
}

impl<'par> Parser<'par> {
    fn new(source: &'par str) -> Self {
        Self {
            scanner: Scanner::new(source),
            current: Token::new(TokenType::EOF, 0, 0, 0),
            previous: Token::new(TokenType::EOF, 0, 0, 0),
            had_error: false,
            panic_mode: false,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            match self.scanner.scan_token() {
                Ok(current) => {
                    self.current = current;
                    break;
                }
                Err(e) => {
                    self.report_error(e)
                }
            }
        }
    }

    fn expression(&mut self) -> Result<(), LoxError> {
        Ok(())
    }

    fn consume_until(&mut self, ty: TokenType, message: &'static str) {
        if self.current.ty == ty {
            self.advance();
        } else {
            self.report_error(LoxError::from_token(message, self.current));
        }
    }
    
    fn report_error(&mut self, e: LoxError) {
        if self.panic_mode { return; }
        self.panic_mode = true;
        eprintln!("{}", e);
        self.had_error = true;
    }


}
