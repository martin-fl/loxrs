use crate::chunk::{Chunk, OpCode};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::{Object, Value};
use crate::LoxError;
use crate::DEBUG;

use crate::define_enum;

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

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
    prefix: Option<fn(&'_ mut Compiler<'r>, bool)>,
    infix: Option<fn(&'_ mut Compiler<'r>, bool)>,
    prec: Precedence,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Local {
    name: Token,
    depth: usize,
}

impl Default for Local {
    fn default() -> Local {
        Local { name: Token::new(TokenType::EOF, 0, 0, 0), depth: 0 }
    }
}

// TODO split the compiler into a Parser and a Generator, instead of doing everything in it
pub struct Compiler<'c> {
    // Compiler related fields
    locals: [Local; u8::MAX as usize + 1],
    local_count: usize,
    scope_depth: usize,

    // Generator related fields
    compiling_chunk: Rc<RefCell<Chunk>>,

    // Parsing related fields
    scanner: Scanner<'c>,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
}

// Compiling/Generating impls
impl<'c> Compiler<'c> {
    pub fn new(source: &'c str) -> Self {
        Self {
            locals: [Local::default(); u8::MAX as usize + 1],
            local_count: 0,
            scope_depth: 0,
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

        while !self.advance_if_current_is(TokenType::EOF) {
            self.declaration();
        }

        if self.had_error {
            return Err(LoxError::from_token(
                "Error during compilation.",
                self.current,
            ));
        }
        
        self.emit_return();
        if DEBUG && !self.had_error {
            self.current_chunk().deref().borrow().disassemble("CODE");
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

    fn make_identifier_constant(&mut self, token: Token) -> u8 {
        self.make_constant(Value::Obj(Box::new(Object::String(
            self.scanner.source[token.start..(token.start + token.len)].to_string(),
        ))))
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

    fn grouping(&mut self, _: bool) {
        self.expression();
        self.consume_until(TokenType::RightParen, "Expected ')' after expression.");
    }

    fn number(&mut self, _: bool) {
        let value = self.scanner.source
            [self.previous.start..(self.previous.start + self.previous.len)]
            .parse::<f64>()
            .expect("Couldn't parse as f64");
        self.emit_constant(Value::Number(value));
    }

    fn string(&mut self, _: bool) {
        let value = self.scanner.source
            [(self.previous.start + 1)..(self.previous.start + self.previous.len - 1)]
            .to_string();
        self.emit_constant(Value::Obj(Box::new(Object::String(value))))
    }

    fn unary(&mut self, _: bool) {
        let op_ty = self.previous.ty;

        self.parse_precedence(Precedence::Unary);

        match op_ty {
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            TokenType::Bang => self.emit_byte(OpCode::Not as u8),
            _ => return,
        }
    }

    fn binary(&mut self, _: bool) {
        let op_ty = self.previous.ty;
        let rule = Self::get_rule(op_ty);
        self.parse_precedence((rule.prec as u8 + 1).into());

        match op_ty {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Substract as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal as u8),
            TokenType::Greater => self.emit_byte(OpCode::Greater as u8),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit_byte(OpCode::Less as u8),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            _ => unreachable!(),
        }
    }

    fn literal(&mut self, _: bool) {
        match self.previous.ty {
            TokenType::False => self.emit_byte(OpCode::False as u8),
            TokenType::Nil => self.emit_byte(OpCode::Nil as u8),
            TokenType::True => self.emit_byte(OpCode::True as u8),
            _ => unreachable!(),
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous, can_assign);
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) {
        let mut arg = self.resolve_local(name);
        let (set_op, get_op) = if arg.is_some() {
            (OpCode::SetLocal, OpCode::GetLocal) 
        } else {
            arg = Some(self.make_identifier_constant(name));
            (OpCode::SetGlobal, OpCode::GetLocal)
        };
        
        if can_assign && self.advance_if_current_is(TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op as u8, arg.unwrap());
        } else {
            self.emit_bytes(get_op as u8, arg.unwrap());
        }
    }

    fn identifiers_are_equal(&self, a: &Token, b: &Token) -> bool {
        ( a.len == b.len ) 
        && (&self.scanner.source[a.start..(a.start+a.len)] == &self.scanner.source[b.start..(b.start+b.len)])
    }

    fn resolve_local(&self, name: Token) -> Option<u8> {
        for i in (0..self.local_count).rev() {
            if self.identifiers_are_equal(&self.locals[i].name, &name) { return Some(i as u8); }
        }

        None
    }

    fn parse_precedence(&mut self, prec: Precedence) {
        self.advance();
        let prefix_rule = Self::get_rule(self.previous.ty).prefix;
        match prefix_rule {
            Some(rule) => {
                let can_assign = prec as u8 <= Precedence::Assignment as u8;
                rule(self, can_assign);
                while prec as u8 <= Self::get_rule(self.current.ty).prec as u8 {
                    self.advance();
                    let infix_rule = Self::get_rule(self.previous.ty).infix;
                    (infix_rule.unwrap())(self, can_assign);

                    if can_assign && self.advance_if_current_is(TokenType::Equal) {
                        self.report_error(LoxError::from_token("Invalid assignment target.", self.current))
                    }
                }
            }
            None => {
                println!(
                    "{}, {:?}",
                    &self.scanner.source
                        [self.previous.start..(self.previous.start + self.previous.len)],
                    self.previous
                );
                self.report_error(LoxError::from_token("Expected expression.", self.current));
                return;
            }
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn declaration(&mut self) {
        if self.advance_if_current_is(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected a variable name.");

        if self.advance_if_current_is(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil as u8);
        }

        self.consume_until(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn parse_variable(&mut self, error_message: &'static str) -> u8 {
        self.consume_until(TokenType::Identifier, error_message);
        self.declare_variable();
        if self.scope_depth > 0 { return 0 }
        return self.make_identifier_constant(self.previous);
    }

    fn define_variable(&mut self, global: u8) {
        if self.scope_depth > 0 { return }
        self.emit_bytes(OpCode::DefineGlobal as u8, global);
    }

    fn declare_variable(&mut self) {
        if self.scope_depth == 0 { return }

        let name = self.previous;
        
        for i in (0..self.local_count).rev() {
            let local = &self.locals[i];
            if local.depth != usize::MAX && local.depth < self.scope_depth {
                break
            }

            if self.identifiers_are_equal(&local.name, &name) {
                self.report_error(LoxError::from_token("Already a variable with this name in this scope.", name));
            }
        }

        self.add_local(name);
    }

    fn add_local(&mut self, name: Token) {
        if self.local_count == u8::MAX as usize + 1 {
            self.report_error(LoxError::from_token("Too many local variables in function.", name));
            return;
        }

        let local = &mut self.locals[self.local_count];
        self.local_count += 1;
        local.name = name;
        local.depth = self.scope_depth;
    }

    fn statement(&mut self) {
        if self.advance_if_current_is(TokenType::Print) {
            self.print_statement();
        } else if self.advance_if_current_is(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn block(&mut self) {
        while !self.current_is(TokenType::RightBrace) && !self.current_is(TokenType::EOF) {
            self.declaration();
        }
        self.consume_until(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while self.local_count > 0 && self.locals[self.local_count - 1].depth > self.scope_depth {
            self.emit_byte(OpCode::Pop as u8);
            self.local_count -= 1;
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume_until(TokenType::Semicolon, "Expected ';' after expression.");
        self.emit_byte(OpCode::Pop as u8);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume_until(TokenType::Semicolon, "Expected ';' after expression.");
        self.emit_byte(OpCode::Print as u8);
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;
        while self.current.ty != TokenType::EOF {
            if self.previous.ty == TokenType::Semicolon {
                return;
            }

            match self.current.ty {
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

    fn consume_until(&mut self, ty: TokenType, message: &'static str) {
        if self.current.ty == ty {
            self.advance();
        } else {
            self.report_error(LoxError::from_token(message, self.current));
        }
    }

    fn current_is(&self, ty: TokenType) -> bool {
        self.current.ty == ty
    }

    fn advance_if_current_is(&mut self, ty: TokenType) -> bool {
        if !self.current_is(ty) {
            false
        } else {
            self.advance();
            true
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
                prefix: Some(Self::grouping),
                infix: None,
                prec: Precedence::None,
            },
            Bang => ParseRule {
                prefix: Some(Self::unary),
                infix: None,
                prec: Precedence::None,
            },
            BangEqual | EqualEqual => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                prec: Precedence::Equality,
            },
            Greater | GreaterEqual | Less | LessEqual => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                prec: Precedence::Comparison,
            },
            Identifier => ParseRule {
                prefix: Some(Self::variable),
                infix: None,
                prec: Precedence::None,
            },
            Minus => ParseRule {
                prefix: Some(Self::unary),
                infix: Some(Self::binary),
                prec: Precedence::Term,
            },
            Plus => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                prec: Precedence::Term,
            },
            Slash | Star => ParseRule {
                prefix: None,
                infix: Some(Self::binary),
                prec: Precedence::Factor,
            },
            Number => ParseRule {
                prefix: Some(Self::number),
                infix: None,
                prec: Precedence::Factor,
            },
            True | False | Nil => ParseRule {
                prefix: Some(Self::literal),
                infix: None,
                prec: Precedence::None,
            },
            String => ParseRule {
                prefix: Some(Self::string),
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


