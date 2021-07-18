use crate::ast::{Expr, Literal};
use crate::token::{Token, TokenType};

pub(crate) struct ParserError<'e> {
    pub token: Token<'e>,
    pub message: String,
}

pub(crate) struct Parser<'p> {
    tokens: Vec<Token<'p>>,
    current: usize,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: Vec<Token<'p>>) -> Self {
        Self { tokens, current: 0 }
    }

    fn previous(&self) -> Token<'p> {
        self.tokens[self.current - 1]
    }

    fn peek(&self) -> Token<'p> {
        self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        self.peek().ty == TokenType::EOF
    }

    fn advance(&mut self) -> Token<'p> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn current_is(&self, ty: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().ty == ty
        }
    }

    fn current_is_any_of(&mut self, tys: &[TokenType]) -> bool {
        for &ty in tys {
            if self.current_is(ty) {
                self.advance();
                return true;
            }
        }

        false
    }

    pub fn parse(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        let mut expr = self.comparison()?;

        while self.current_is_any_of(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        let mut expr = self.term()?;

        while self.current_is_any_of(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = self.previous();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        let mut expr = self.factor()?;

        while self.current_is_any_of(&[TokenType::Plus, TokenType::Minus]) {
            let op = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        let mut expr = self.unary()?;

        while self.current_is_any_of(&[TokenType::Slash, TokenType::Star]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        if self.current_is_any_of(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous();
            let right = self.unary()?;
            Ok(Expr::Unary(op, Box::new(right)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        match self.peek().ty {
            TokenType::False => {
                self.advance();
                Ok(Expr::Literal(Literal::False))
            }
            TokenType::True => {
                self.advance();
                Ok(Expr::Literal(Literal::True))
            }
            TokenType::Nil => {
                self.advance();
                Ok(Expr::Literal(Literal::Nil))
            }
            TokenType::Number(x) => {
                self.advance();
                Ok(Expr::Literal(Literal::Number(x)))
            }
            TokenType::String(ref s) => {
                self.advance();
                Ok(Expr::Literal(Literal::String(s)))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(
                    TokenType::RightParen,
                    "Expect ')' after expression.".to_string(),
                )?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            _ => Err(ParserError {
                token: self.peek(),
                message: "Expect expression".to_string(),
            }),
        }
    }

    fn consume(&mut self, ty: TokenType, message: String) -> Result<Token<'p>, ParserError<'p>> {
        if self.current_is(ty) {
            Ok(self.advance())
        } else {
            Err(ParserError {
                token: self.peek(),
                message,
            })
        }
    }

    #[allow(dead_code)]
    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().ty == TokenType::Semicolon {
                return;
            }

            match self.peek().ty {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }
}
