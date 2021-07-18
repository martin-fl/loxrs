use std::fs::File;
use std::io::{self, Read, Write};

use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};

pub(crate) enum LoxError<'e> {
    FromScanner(usize, String),
    FromParser(Vec<(Token<'e>, String)>),
    FromInterpreter(Token<'e>, String),
}

impl<'a> LoxError<'a> {
    pub(crate) fn report(&self) {
        match self {
            LoxError::FromScanner(line, message) => eprintln!("[line {}] Error: {}", line, message),
            LoxError::FromParser(v) => {
                for (token, message) in v {
                    match token.ty {
                        TokenType::EOF => {
                            eprintln!("[line {}] Error at end: {}", token.line, message)
                        }
                        _ => eprintln!(
                            "[line {}] Error at '{}': {}",
                            token.line, token.lexeme, message
                        ),
                    }
                }
            }
            LoxError::FromInterpreter(token, message) => match token.ty {
                TokenType::EOF => eprintln!("[line {}] Error at end: {}", token.line, message),
                _ => eprintln!(
                    "[line {}] Error at '{}': {}",
                    token.line, token.lexeme, message
                ),
            },
        }
    }
}

pub struct Lox {
    interpreter: Interpreter,
}

impl Lox {
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
        }
    }

    fn run<'a>(&'a mut self, source: &'a str) -> Result<(), LoxError<'a>> {
        let scanner = Scanner::new(source);
        let tokens = scanner
            .scan_tokens()
            .map_err(|e| LoxError::FromScanner(e.line, e.message))?;

        let mut parser = Parser::new(tokens);
        let statements = parser.parse().map_err(|v| {
            LoxError::FromParser(
                v.iter()
                    .map(|e| (e.token.clone(), e.message.clone()))
                    .collect(),
            )
        })?;

        self.interpreter
            .interpret(statements)
            .map_err(|e| LoxError::FromInterpreter(e.token.clone(), e.message))?;

        Ok(())
    }

    pub fn run_file(&mut self, script_path: &str) -> io::Result<()> {
        let mut script_file = File::open(script_path)?;
        let mut script = String::new();
        script_file.read_to_string(&mut script)?;

        match self.run(&script) {
            Ok(()) => {}
            Err(e) => {
                e.report();
                panic!();
            }
        }

        Ok(())
    }

    pub fn run_prompt(&mut self) -> io::Result<()> {
        let mut line = String::new();
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        loop {
            print!("lox> ");
            let _ = stdout.flush();
            stdin.read_line(&mut line)?;
            if line.len() <= 1 {
                break;
            }
            match self.run(&line) {
                Ok(()) => {}
                Err(e) => e.report(),
            }
            line.clear();
        }

        Ok(())
    }
}
