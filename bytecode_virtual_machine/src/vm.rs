use crate::chunk::{Chunk, OpCode};
use crate::compiler::Compiler;
use crate::value::{Object, Value};
use crate::LoxError;
use crate::DEBUG;

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, Read, Write};
use std::fmt;

macro_rules! binary_op_number {
    ($self:ident,$op:tt) => {
        match ($self.peek(0),$self.peek(1)) {
            (Value::Number(_),Value::Number(_)) => {
                if let Value::Number(right) = $self.pop() {
                    if let Value::Number(left) = $self.pop() {
                        $self.push(Value::Number(left $op right));
                    }
                }
            },
            _ => return Err(InterpretError::RuntimeError(LoxError::new("Operands must be two numbers", $self.get_line())))
        }
    };
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError(LoxError),
    RuntimeError(LoxError),
}

impl fmt::Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpretError::CompileError(e) => write!(f, "{}", e),
            InterpretError::RuntimeError(e) => write!(f, "{}", e),
        }
    }
}

// Note: the VM leaks memory when handling objects
pub struct VM {
    chunk: Option<Box<Chunk>>,
    ip: usize,
    //stack: RefCell<[Value; VM::STACK_MAX]>,
    stack: RefCell<Vec<Value>>,
    stack_top: Cell<usize>,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: RefCell::new(Vec::new()),
            stack_top: Cell::new(0),
            globals: HashMap::new(),
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
                Err(e) => eprint!("{}", e),
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

        chunk = Compiler::new(source)
            .compile(chunk)
            .map_err(|e| InterpretError::CompileError(e))?;

        self.chunk = Some(Box::new(chunk));
        self.ip = 0;

        self.run()
    }

    // Note: assumes chunk is Some(_)
    fn run(&mut self) -> Result<(), InterpretError> {
        let instructions = &self.chunk.as_ref().unwrap().instructions;
        let constants = &self.chunk.as_ref().unwrap().constants;
        if DEBUG {
            print!("\n== VM BACKTRACE ==")
        }
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

            #[allow(unreachable_patterns)]
            match instructions[self.ip].into() {
                OpCode::Return => {
                    return Ok(());
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::Print => println!("{}", self.pop()),
                OpCode::DefineGlobal => {
                    self.ip += 1;
                    let name = constants[instructions[self.ip] as usize].clone();
                    if let Value::Obj(box Object::String(name)) = name {
                        self.globals.insert(name, self.peek(0));
                        self.pop();
                    }
                }
                OpCode::GetGlobal => {
                    self.ip += 1;
                    let name = constants[instructions[self.ip] as usize].clone();
                    if let Value::Obj(box Object::String(name)) = name {
                        if let Some(value) = self.globals.get(&name) {
                            self.push(value.clone());
                        } else {
                            return Err(InterpretError::RuntimeError(LoxError::new(
                                "Undefined variable.",
                                self.get_line(),
                            )));
                        }
                    }
                }
                OpCode::SetGlobal => {
                    self.ip += 1;
                    let name = constants[instructions[self.ip] as usize].clone();
                    if let Value::Obj(box Object::String(name)) = name {
                        if self.globals.contains_key(&name) { 
                            self.globals.insert(name, self.peek(0));
                        } else {
                            return Err(InterpretError::RuntimeError(LoxError::new(
                                "Undefined variable.",
                                self.get_line(),
                            )));
                        }
                    }
                }
                OpCode::GetLocal => {
                    self.ip +=1;
                    let slot = instructions[self.ip];
                    let local = self.stack.borrow()[slot as usize].clone();
                    self.push(local);
                }
                OpCode::SetLocal => {
                    self.ip +=1;
                    let slot = instructions[self.ip];
                    self.stack.borrow_mut()[slot as usize] = self.peek(0).clone();
                }
                OpCode::Constant => {
                    self.ip += 1;
                    let constant = constants[instructions[self.ip] as usize].clone();
                    self.push(constant);
                }
                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),

                OpCode::Equal => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(Value::Bool(left == right));
                }
                OpCode::Greater => match (self.peek(0), self.peek(1)) {
                    (Value::Number(_), Value::Number(_)) => {
                        if let Value::Number(right) = self.pop() {
                            if let Value::Number(left) = self.pop() {
                                self.push(Value::Bool(left > right));
                            }
                        }
                    }
                    _ => {
                        return Err(InterpretError::RuntimeError(LoxError::new(
                            "Operands must be two numbers",
                            self.get_line(),
                        )))
                    }
                },
                OpCode::Less => match (self.peek(0), self.peek(1)) {
                    (Value::Number(_), Value::Number(_)) => {
                        if let Value::Number(right) = self.pop() {
                            if let Value::Number(left) = self.pop() {
                                self.push(Value::Bool(left < right));
                            }
                        }
                    }
                    _ => {
                        return Err(InterpretError::RuntimeError(LoxError::new(
                            "Operands must be two numbers",
                            self.get_line(),
                        )))
                    }
                },

                OpCode::Add => {
                    if self.peek(0).is_string() && self.peek(1).is_string() {
                        match (self.pop(), self.pop()) {
                            (
                                Value::Obj(box Object::String(right)),
                                Value::Obj(box Object::String(left)),
                            ) => self.push(Value::Obj(Box::new(Object::String(format!(
                                "{}{}",
                                left, right
                            ))))),
                            _ => unreachable!(),
                        }
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        match (self.pop(), self.pop()) {
                            (Value::Number(right), Value::Number(left)) => {
                                self.push(Value::Number(left + right))
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        return Err(InterpretError::RuntimeError(LoxError::new(
                            "Operands must be two numbers",
                            self.get_line(),
                        )));
                    }
                }
                OpCode::Substract => binary_op_number!(self, -),
                OpCode::Multiply => binary_op_number!(self, *),
                OpCode::Divide => binary_op_number!(self, /),

                OpCode::Negate => {
                    if let Value::Number(x) = self.peek(0) {
                        self.stack.borrow_mut()[self.stack_top.get() - 1] = Value::Number(-x);
                    // self.push(-self.pop()),
                    } else {
                        return Err(InterpretError::RuntimeError(LoxError::new(
                            "Operand must be a number",
                            self.get_line(),
                        )));
                    }
                }
                OpCode::Not => self.push(Value::Bool(self.pop().is_falsey())),
                _ => {
                    return Err(InterpretError::CompileError(LoxError::new(
                        "Unknown OpCode",
                        self.get_line(),
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

    fn get_line(&self) -> usize {
        self.chunk.as_ref().unwrap().lines[self.ip]
    }

    fn peek(&self, distance: usize) -> Value {
        self.stack.borrow()[self.stack_top.get() - 1 - distance].clone()
    }
}
