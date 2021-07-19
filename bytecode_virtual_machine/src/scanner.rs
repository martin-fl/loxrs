use crate::LoxError;

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    EOF,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub(crate) ty: TokenType,
    pub(crate) start: usize,
    pub(crate) len: usize,
    pub(crate) line: usize,
}

impl Token {
    pub fn new(ty: TokenType, start: usize, len: usize, line: usize) -> Self {
        Self {
            ty,
            start,
            len,
            line,
        }
    }
}

pub struct Scanner<'sca> {
    pub(crate) source: &'sca str,
    pub(crate) source_chars: Vec<char>,
    pub(crate) start: usize,
    pub(crate) current: usize,
    pub(crate) line: usize,
}

impl<'sca> Scanner<'sca> {
    pub fn new(source: &'sca str) -> Self {
        Self {
            source,
            source_chars: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Result<Token, LoxError> {
        self.skip_white_space();
        self.start = self.current;

        if self.is_at_end() {
            return Ok(self.make_token(TokenType::EOF));
        }

        let c = self.advance();

        match c {
            '(' => Ok(self.make_token(TokenType::LeftParen)),
            ')' => Ok(self.make_token(TokenType::RightParen)),
            '{' => Ok(self.make_token(TokenType::LeftBrace)),
            '}' => Ok(self.make_token(TokenType::RightBrace)),
            ';' => Ok(self.make_token(TokenType::Semicolon)),
            ',' => Ok(self.make_token(TokenType::Comma)),
            '.' => Ok(self.make_token(TokenType::Dot)),
            '-' => Ok(self.make_token(TokenType::Minus)),
            '+' => Ok(self.make_token(TokenType::Plus)),
            '/' => Ok(self.make_token(TokenType::Slash)),
            '*' => Ok(self.make_token(TokenType::Star)),

            '!' => {
                if self.advance_if_current_is('=') {
                    Ok(self.make_token(TokenType::BangEqual))
                } else {
                    Ok(self.make_token(TokenType::Bang))
                }
            }
            '=' => {
                if self.advance_if_current_is('=') {
                    Ok(self.make_token(TokenType::EqualEqual))
                } else {
                    Ok(self.make_token(TokenType::Equal))
                }
            }
            '>' => {
                if self.advance_if_current_is('=') {
                    Ok(self.make_token(TokenType::GreaterEqual))
                } else {
                    Ok(self.make_token(TokenType::Greater))
                }
            }
            '<' => {
                if self.advance_if_current_is('=') {
                    Ok(self.make_token(TokenType::LessEqual))
                } else {
                    Ok(self.make_token(TokenType::Less))
                }
            }

            '"' => self.make_string_token(),

            c if c.is_ascii_digit() => self.make_number_token(),

            c if c.is_ascii_alphabetic() || c == '_' => self.make_identifier_token(),

            _ => Err(LoxError::new("Unexpected character", self.line)),
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source_chars[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        self.current == self.source_chars.len()
    }

    fn advance_if_current_is(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn skip_white_space(&mut self) {
        loop {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' if self.peek_next() == '/' => {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                _ => {}
            };
        }
    }

    fn peek(&self) -> char {
        self.source_chars[self.current]
    }

    fn peek_next(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source_chars[self.current + 1]
        }
    }

    fn make_token(&self, ty: TokenType) -> Token {
        Token::new(ty, self.start, self.current - self.start, self.line)
    }

    fn make_string_token(&mut self) -> Result<Token, LoxError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(LoxError::new("Unterminated string.", self.line));
        }

        self.advance();

        Ok(self.make_token(TokenType::String))
    }

    fn make_number_token(&mut self) -> Result<Token, LoxError> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        Ok(self.make_token(TokenType::Number))
    }

    fn make_identifier_token(&mut self) -> Result<Token, LoxError> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        Ok(self.make_token(self.get_identifier_type()))
    }

    fn get_identifier_type(&self) -> TokenType {
        match self.source_chars[self.start] {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' if self.current - self.start > 1 => match self.source_chars[self.start + 1] {
                'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                'o' => self.check_keyword(2, 1, "r", TokenType::For),
                'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                _ => TokenType::Identifier,
            },
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "print", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' if self.current - self.start > 1 => match self.source_chars[self.start + 1] {
                'h' => self.check_keyword(2, 2, "is", TokenType::This),
                'r' => self.check_keyword(2, 2, "u", TokenType::True),
                _ => TokenType::Identifier,
            },
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn check_keyword(
        &self,
        start: usize,
        len: usize,
        rest: &'static str,
        ty: TokenType,
    ) -> TokenType {
        if self.current - self.start == start + len
            && &self.source[self.start..(self.start + start)] == rest
        {
            ty
        } else {
            TokenType::Identifier
        }
    }
}
