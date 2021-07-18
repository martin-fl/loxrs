use std::fs::File;
use std::io::{self, Read, Write};

use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use crate::interpreter::Interpreter;

pub(crate) enum LoxError<'e> {
    FromScanner(usize, String),
    FromParser(Token<'e>, String),
    FromInterpreter(Token<'e>, String),
}

impl<'a> LoxError<'a> {
    pub(crate) fn report(&self) {
        let (line, where_, message) = match self {
            LoxError::FromScanner(line, message) => (*line, "".to_string(), message),
            LoxError::FromParser(token, message) 
                | LoxError::FromInterpreter(token, message) => match token.ty {
                TokenType::EOF => (token.line, " at end".to_string(), message),
                _ => (token.line, format!(" at '{}'", token.lexeme), message),
            },
        };
        eprintln!("[line {}] Error{}: {}", line, where_, message);
    }
}

#[derive(Clone, Copy)]
pub struct Lox;

impl Lox {
    fn run<'a>(source: &'a str) -> Result<(), LoxError<'a>> {
        let scanner = Scanner::new(source);
        let tokens = scanner
            .scan_tokens()
            .map_err(|e| LoxError::FromScanner(e.line, e.message))?;
        let mut parser = Parser::new(tokens);
        let tree = parser
            .parse()
            .map_err(|e| LoxError::FromParser(e.token.clone(), e.message))?;

        Interpreter::interpret(tree).map_err(|e| LoxError::FromInterpreter(e.token.clone(), e.message))?;
        
        Ok(())
    }

    pub fn run_file(script_path: &str) -> io::Result<()> {
        let mut script_file = File::open(script_path)?;
        let mut script = String::new();
        script_file.read_to_string(&mut script)?;

        match Lox::run(&script) {
            Ok(()) => {}
            Err(e) => {
                e.report();
                panic!();
            }
        }

        Ok(())
    }

    pub fn run_prompt() -> io::Result<()> {
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
            match Lox::run(&line) {
                Ok(()) => {}
                Err(e) => e.report(),
            }
            line.clear();
        }

        Ok(())
    }
}
