use crate::ast::{Expr, Literal, Stmt};
use crate::token::{Token, TokenType};

pub(crate) struct ParserError<'e> {
    pub token: Token<'e>,
    pub message: String,
}

pub(crate) struct Parser<'p> {
    pub tokens: Vec<Token<'p>>,
    pub current: usize,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: Vec<Token<'p>>) -> Self {
        Self { tokens, current: 0 }
    }

    fn previous(&self) -> Token<'p> {
        self.tokens[self.current - 1].clone()
    }

    fn peek(&self) -> Token<'p> {
        self.tokens[self.current].clone()
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

    fn current_is(&self, ty: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.peek().ty == ty
        }
    }

    fn current_is_any_of(&mut self, tys: &[TokenType]) -> bool {
        for ty in tys {
            if self.current_is(ty) {
                self.advance();
                return true;
            }
        }

        false
    }

    pub(crate) fn parse(&mut self) -> Result<Vec<Stmt<'p>>, Vec<ParserError<'p>>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.is_at_end() {
            let decl = self.declaration();

            match decl {
                Ok(d) => statements.push(d),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn declaration(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
        let ret = if self.current_is_any_of(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };

        if ret.is_err() {
            self.synchronize();
        }

        ret
    }

    fn var_declaration(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
        let name = self.consume_until(TokenType::Identifier, "Expected variable name")?;

        let initializer = if self.current_is_any_of(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume_until(
            TokenType::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        Ok(Stmt::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
        if self.current_is_any_of(&[TokenType::Print]) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
        let expr = self.expression()?;
        self.consume_until(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print(expr))
    }

    fn expression_statement(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
        let expr = self.expression()?;
        self.consume_until(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Expr(expr))
    }

    fn expression(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        let expr = self.equality()?;

        if self.current_is_any_of(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;

            return if let Expr::Variable(name) = expr {
                Ok(Expr::Assign(name, Box::new(value)))
            } else {
                Err(ParserError {
                    token: equals,
                    message: "Invalid assignment target.".to_string(),
                })
            };
        }

        Ok(expr)
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
            TokenType::String(s) => {
                self.advance();
                Ok(Expr::Literal(Literal::String(s)))
            }
            TokenType::Identifier => {
                self.advance();
                Ok(Expr::Variable(self.previous()))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume_until(TokenType::RightParen, "Expected ')' after expression.")?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            _ => Err(ParserError {
                token: self.peek(),
                message: "Expected expression".to_string(),
            }),
        }
    }

    fn consume_until(
        &mut self,
        ty: TokenType,
        message: &str,
    ) -> Result<Token<'p>, ParserError<'p>> {
        if self.current_is(&ty) {
            Ok(self.advance())
        } else {
            Err(ParserError {
                token: self.peek(),
                message: message.to_string(),
            })
        }
    }

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
