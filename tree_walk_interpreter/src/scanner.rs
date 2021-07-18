use crate::token::{Token, TokenType};

#[derive(Debug)]
pub(crate) struct ScannerError {
    pub line: usize,
    pub message: String,
}

pub(crate) struct Scanner<'scanner> {
    source: &'scanner str,
    source_chars: Vec<char>,
    tokens: Vec<Token<'scanner>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'scanner> Scanner<'scanner> {
    pub(crate) fn new(source: &'scanner str) -> Self {
        Self {
            source,
            source_chars: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    pub(crate) fn scan_tokens(mut self) -> Result<Vec<Token<'scanner>>, ScannerError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens.push(Token::new(TokenType::EOF, "", self.line));

        Ok(self.tokens)
    }

    fn scan_token(&mut self) -> Result<(), ScannerError> {
        let c = self.advance();

        use TokenType::*;
        match c {
            '(' => Ok(self.add_token(LeftParen)),
            ')' => Ok(self.add_token(RightParen)),
            '{' => Ok(self.add_token(LeftBrace)),
            '}' => Ok(self.add_token(RightBrace)),
            ',' => Ok(self.add_token(Comma)),
            '.' => Ok(self.add_token(Dot)),
            '-' => Ok(self.add_token(Minus)),
            '+' => Ok(self.add_token(Plus)),
            ';' => Ok(self.add_token(Semicolon)),
            '*' => Ok(self.add_token(Star)),

            '!' => {
                let tok = if self.current_is('=') {
                    BangEqual
                } else {
                    Bang
                };

                Ok(self.add_token(tok))
            }
            '=' => {
                let tok = if self.current_is('=') {
                    EqualEqual
                } else {
                    Equal
                };

                Ok(self.add_token(tok))
            }
            '<' => {
                let tok = if self.current_is('=') {
                    LessEqual
                } else {
                    Less
                };

                Ok(self.add_token(tok))
            }
            '>' => {
                let tok = if self.current_is('=') {
                    GreaterEqual
                } else {
                    Greater
                };

                Ok(self.add_token(tok))
            }
            '/' => {
                if self.current_is('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.current_is('*') {
                    while self.peek() != '*' && self.peek_next() != '/' {
                        self.advance();
                        if self.peek() == '\n' {
                            self.line += 1
                        };
                    }
                    self.advance();
                    self.advance();
                } else {
                    self.add_token(Slash);
                }
                Ok(())
            }

            ' ' | '\r' | '\t' => Ok(()),
            '\n' => Ok(self.line += 1),

            '"' => self.handle_string_literal(),
            n if n.is_ascii_digit() => self.handle_number_literal(),
            c if c.is_ascii_alphabetic() || c == '_' => self.handle_identifier(),

            _ => Err(ScannerError {
                line: self.line,
                message: "Unexpected character".to_string(),
            }),
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source_chars[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source_chars[self.current + 1]
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source_chars[self.current - 1]
    }

    fn current_is(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            false
        } else if self.source_chars[self.current] != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn handle_string_literal(&mut self) -> Result<(), ScannerError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(ScannerError {
                line: self.line,
                message: "Unterminated string".to_string(),
            });
        }

        self.advance();

        let value = &self.source[(self.start + 1)..(self.current - 1)];
        self.add_token(TokenType::String(value.to_string()));

        Ok(())
    }

    fn handle_number_literal(&mut self) -> Result<(), ScannerError> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.add_token(TokenType::Number(
            self.source[self.start..self.current]
                .parse::<f64>()
                .expect("Couldn't parse as f64"),
        ));

        Ok(())
    }

    fn handle_identifier(&mut self) -> Result<(), ScannerError> {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let ident = &self.source[self.start..self.current];
        self.add_token(TokenType::try_to_keyword(ident).unwrap_or(TokenType::Identifier));

        Ok(())
    }

    fn add_token(&mut self, ty: TokenType) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens.push(Token::new(ty, lexeme, self.line));
    }
}
