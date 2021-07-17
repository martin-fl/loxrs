use crate::interpreter::Interpreter;
use crate::token::{Token, TokenType};

pub(crate) struct Scanner<'scanner> {
    interpreter: &'scanner mut Interpreter,
    source: &'scanner str,
    source_chars: Vec<char>,
    tokens: Vec<Token<'scanner>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'scanner> Scanner<'scanner> {
    pub fn new(interpreter: &'scanner mut Interpreter, source: &'scanner str) -> Self {
        Self {
            interpreter,
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

    pub fn scan_tokens(&mut self) -> &[Token] {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::EOF, "", self.line));

        &self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        use TokenType::*;
        match c {
            '(' => self.add_token(LeftParen),
            ')' => self.add_token(RightParen),
            '{' => self.add_token(LeftBrace),
            '}' => self.add_token(RightBrace),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            '-' => self.add_token(Minus),
            '+' => self.add_token(Plus),
            ';' => self.add_token(Semicolon),
            '*' => self.add_token(Star),

            '!' => {
                let tok = if self.current_is('=') {
                    BangEqual
                } else {
                    Bang
                };

                self.add_token(tok);
            }
            '=' => {
                let tok = if self.current_is('=') {
                    EqualEqual
                } else {
                    Equal
                };

                self.add_token(tok);
            }
            '<' => {
                let tok = if self.current_is('=') {
                    LessEqual
                } else {
                    Less
                };

                self.add_token(tok);
            }
            '>' => {
                let tok = if self.current_is('=') {
                    GreaterEqual
                } else {
                    Greater
                };

                self.add_token(tok);
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
            }

            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,

            '"' => self.handle_string_literal(),
            n if n.is_ascii_digit() => self.handle_number_literal(),
            c if c.is_ascii_alphabetic() || c == '_' => self.handle_identifier(),

            _ => self.interpreter.error(self.line, "Unexpected character."),
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

    fn handle_string_literal(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.interpreter.error(self.line, "Unterminated string");
            return;
        }

        self.advance();

        let value = &self.source[(self.start + 1)..(self.current - 1)];
        self.add_token(TokenType::String(value));
    }

    fn handle_number_literal(&mut self) {
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
    }

    fn handle_identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let ident = &self.source[self.start..self.current];
        self.add_token(TokenType::try_to_keyword(ident).unwrap_or(TokenType::Identifier));
    }

    fn add_token(&mut self, ty: TokenType<'scanner>) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens.push(Token::new(ty, lexeme, self.line));
    }
}
