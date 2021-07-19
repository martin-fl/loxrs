use crate::chunk::Chunk;
use crate::scanner::{Scanner, Token, TokenType};
use crate::LoxError;

pub struct Compiler {}

impl Compiler {
    pub fn compile(source: &str, chunk: &mut Chunk) -> Result<(), LoxError> {
        let mut scanner = Scanner::new(source);
        let mut parser = Parser::new(&mut scanner);
        parser.advance();
        parser.expression();
        parser.consume_until(TokenType::EOF, "Expected end of expression.");

        if parser.had_error {
            Err(LoxError::from_token(
                "Error during compilation.",
                parser.current,
            ))
        } else {
            Ok(())
        }
    }
}

struct Parser<'par> {
    scanner: &'par mut Scanner<'par>,
    current: Token,
    previous: Token,
    had_error: bool,
}

impl<'par> Parser<'par> {
    fn new(scanner: &'par mut Scanner<'par>) -> Self {
        Self {
            scanner,
            current: Token::new(TokenType::EOF, 0, 0, 0),
            previous: Token::new(TokenType::EOF, 0, 0, 0),
            had_error: false,
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
                    eprintln!("{}", e);
                    self.had_error = true;
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
            eprintln!("{}", LoxError::from_token(message, self.current));
            self.had_error = true;
        }
    }
}
