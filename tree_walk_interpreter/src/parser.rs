use crate::ast::{Expr, Literal};
use crate::token::{Token, TokenType};

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

    fn expression(&mut self) -> Expr<'p> {
        self.equality()
    }

    fn equality(&mut self) -> Expr<'p> {
        let mut expr = self.comparison();

        while self.current_is_any_of(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous();
            let right = self.comparison();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        expr
    }

    fn comparison(&mut self) -> Expr<'p> {
        let mut expr = self.term();

        while self.current_is_any_of(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = self.previous();
            let right = self.term();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        expr
    }

    fn term(&mut self) -> Expr<'p> {
        let mut expr = self.factor();

        while self.current_is_any_of(&[TokenType::Plus, TokenType::Minus]) {
            let op = self.previous();
            let right = self.factor();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        expr
    }

    fn factor(&mut self) -> Expr<'p> {
        let mut expr = self.unary();
        
        while self.current_is_any_of(&[TokenType::Slash, TokenType::Star]) {
            let op = self.previous();
            let right = self.unary();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        expr
    }

    fn unary(&mut self) -> Expr<'p> {
        if self.current_is_any_of(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous();
            let right = self.unary();
            Expr::Unary(op, Box::new(right))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expr<'p> {
        match self.peek().ty {
            TokenType::False => {
                self.advance();
                Expr::Literal(Literal::False)
            },
            TokenType::True => {
                self.advance();
                Expr::Literal(Literal::True)
            },
            TokenType::Nil => {
                self.advance();
                Expr::Literal(Literal::Nil)
            },
            TokenType::Number(x) => {
                self.advance();
                Expr::Literal(Literal::Number(x))
            },
            TokenType::String(ref s) => {
                self.advance();
                Expr::Literal(Literal::String(s))
            },
            TokenType::LeftParen => {
                let mut expr = self.expression();
                self.consume(TokenType::RightParen, "Expect ')' after expression.");
                Expr::Grouping(Box::new(expr))
            }
            _ => Expr::Literal(Literal::Nil),
        }
    }

    fn consume(&mut self, ty: TokenType, message: &str) -> Result<Token<'p>, ()> {
        if self.current_is(ty) {
            self.advance()
        } else {
            Err(())
        }
    }
}

