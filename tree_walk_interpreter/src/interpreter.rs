use std::fs::File;
use std::io::{self, Read, Write};

use crate::scanner::Scanner;
use crate::token::Token;

pub struct Interpreter {
    had_error: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { had_error: false }
    }

    fn run(&mut self, source: &str) {
        let mut scanner = Scanner::new(self, &source);
        let tokens = scanner.scan_tokens();

        for token in tokens {
            println!("{:?}", token);
        }
    }

    pub fn run_file(&mut self, script_path: &str) -> io::Result<()> {
        let mut script_file = File::open(script_path)?;
        let mut script = String::new();
        script_file.read_to_string(&mut script)?;

        self.run(&script);
        if self.had_error {
            panic!()
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
            self.run(&line);
            self.had_error = false;
            line.clear();
        }

        Ok(())
    }

    pub(crate) fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    pub fn report(&mut self, line: usize, where_: &str, message: &str) {
        eprintln!("[line {}] Error{}: {}", line, where_, message);
        // Safety: singlethreaded
        self.had_error = true;
    }
}
