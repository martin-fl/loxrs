use crate::chunk::{Chunk, OpCode};
use crate::compiler::Compiler;
use crate::value::Value;
use crate::LoxError;
use crate::DEBUG;

use std::cell::{Cell, RefCell};
use std::fs::File;
use std::io::{self, Read, Write};

pub enum InterpretError {
    CompileError(LoxError),
    RuntimeError(LoxError),
}

pub struct VM {
    chunk: Option<Box<Chunk>>,
    ip: usize,
    //stack: RefCell<[Value; VM::STACK_MAX]>,
    stack: RefCell<Vec<Value>>,
    stack_top: Cell<usize>,
}

impl VM {
    const STACK_MAX: usize = 256;

    pub fn new() -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: RefCell::new(Vec::new()),
            stack_top: Cell::new(0),
        }
    }

    pub fn run_prompt(&mut self) -> io::Result<()> {
        let mut line = String::from("  ");
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        while line.len() > 1 {
            line.clear();
            print!("lox> ");
            let _ = stdout.flush();
            stdin.read_line(&mut line)?;
            match self.interpret(&line) {
                Ok(()) => {}
                Err(_) => {}
            }
        }

        Ok(())
    }

    pub fn run_file(&mut self, script_path: &str) -> io::Result<()> {
        let mut script_file = File::open(script_path)?;
        let mut script = String::new();
        script_file.read_to_string(&mut script)?;

        match self.interpret(&script) {
            Ok(()) => {}
            Err(_) => {}
        }

        Ok(())
    }

    fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        let mut chunk = Chunk::new();

        Compiler::compile(source, &mut chunk).map_err(|e| InterpretError::CompileError(e))?;

        self.chunk = Some(Box::new(chunk));
        self.ip = 0;

        self.run()
    }

    // Note: assumes chunk is Some(_)
    fn run(&mut self) -> Result<(), InterpretError> {
        let instructions = &self.chunk.as_ref().unwrap().instructions;
        let constants = &self.chunk.as_ref().unwrap().constants;
        while self.ip < instructions.len() {
            if DEBUG {
                print!("          ");
                for i in 0..self.stack_top.get() {
                    print!("[{}]", &self.stack.borrow()[i]);
                }
                println!();

                self.chunk
                    .as_ref()
                    .unwrap()
                    .disassemble_instruction_at(self.ip);
            }

            match instructions[self.ip].into() {
                OpCode::Return => {
                    println!("{}", self.pop());
                    return Ok(());
                }
                OpCode::Constant => {
                    self.ip += 1;
                    let constant = constants[instructions[self.ip] as usize];
                    self.push(constant);
                }

                OpCode::Add => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(left + right);
                }
                OpCode::Substract => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(left - right);
                }
                OpCode::Multiply => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(left * right);
                }
                OpCode::Divide => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(left / right);
                }

                OpCode::Negate => self.stack.borrow_mut()[self.stack_top.get() - 1] *= -1.0, // self.push(-self.pop()),
                _ => {
                    return Err(InterpretError::CompileError(LoxError::new(
                        "Unknown OpCode",
                        self.chunk.as_ref().unwrap().lines[self.ip],
                    )))
                }
            }
            self.ip += 1;
        }
        Ok(())
    }

    fn push(&self, value: Value) {
        self.stack.borrow_mut().push(value);
        self.stack_top.set(self.stack_top.get() + 1);
    }

    fn pop(&self) -> Value {
        self.stack_top.set(self.stack_top.get() - 1);
        self.stack
            .borrow_mut()
            .pop()
            .expect("Can't pop an empty stack.")
    }
}
