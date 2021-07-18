use crate::ast::{Expr, Literal, Stmt};
use crate::token::{Token, TokenType};

pub struct ParserError<'e> {
    pub(crate) token: Token<'e>,
    pub(crate) message: String,
}

pub struct Parser<'p> {
    pub(crate) tokens: Vec<Token<'p>>,
    pub(crate) current: usize,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: Vec<Token<'p>>) -> Self {
        Self { tokens, current: 0 }
    }

    pub(crate) fn previous(&self) -> Token<'p> {
        self.tokens[self.current - 1].clone()
    }

    pub(crate) fn peek(&self) -> Token<'p> {
        self.tokens[self.current].clone()
    }

    pub(crate) fn is_at_end(&self) -> bool {
        self.peek().ty == TokenType::EOF
    }

    pub(crate) fn advance(&mut self) -> Token<'p> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    pub(crate) fn current_is(&self, ty: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.peek().ty == ty
        }
    }

    pub(crate) fn current_is_any_of(&mut self, tys: &[TokenType]) -> bool {
        for ty in tys {
            if self.current_is(ty) {
                self.advance();
                return true;
            }
        }

        false
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt<'p>>, Vec<ParserError<'p>>> {
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

    pub(crate) fn declaration(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
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

    pub(crate) fn var_declaration(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
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

    pub(crate) fn statement(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
        if self.current_is_any_of(&[TokenType::Print]) {
            self.print_statement()
        } else if self.current_is_any_of(&[TokenType::LeftBrace]) {
            self.block()
        } else {
            self.expression_statement()
        }
    }

    pub(crate) fn print_statement(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
        let expr = self.expression()?;
        self.consume_until(TokenType::Semicolon, "Expected ';' after value.")?;
        Ok(Stmt::Print(expr))
    }

    pub(crate) fn block(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
        let mut statements = Vec::new();

        while !self.current_is(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume_until(TokenType::RightBrace, "Expected '}' after block.")?;
        Ok(Stmt::Block(statements))
    }

    pub(crate) fn expression_statement(&mut self) -> Result<Stmt<'p>, ParserError<'p>> {
        let expr = self.expression()?;
        self.consume_until(TokenType::Semicolon, "Expected ';' after value.")?;
        Ok(Stmt::Expr(expr))
    }

    pub(crate) fn expression(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        self.assignment()
    }

    pub(crate) fn assignment(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
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

    pub(crate) fn equality(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        let mut expr = self.comparison()?;

        while self.current_is_any_of(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    pub(crate) fn comparison(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
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

    pub(crate) fn term(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        let mut expr = self.factor()?;

        while self.current_is_any_of(&[TokenType::Plus, TokenType::Minus]) {
            let op = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    pub(crate) fn factor(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        let mut expr = self.unary()?;

        while self.current_is_any_of(&[TokenType::Slash, TokenType::Star]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    pub(crate) fn unary(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
        if self.current_is_any_of(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous();
            let right = self.unary()?;
            Ok(Expr::Unary(op, Box::new(right)))
        } else {
            self.primary()
        }
    }

    pub(crate) fn primary(&mut self) -> Result<Expr<'p>, ParserError<'p>> {
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

    pub(crate) fn consume_until(
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

    pub(crate) fn synchronize(&mut self) {
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
