use crate::chunk::OpCode;
use crate::compiler::{Compiler, FunctionType};
use crate::value::{FunctionObject, Object, Value};
use crate::LoxError;
use crate::DEBUG;

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::{self, Read, Write};
use std::ops::Deref;
use std::rc::Rc;

macro_rules! binary_op_number {
    ($self:ident,$op:tt,$get_line:ident) => {
        match ($self.peek(0),$self.peek(1)) {
            (Value::Number(_),Value::Number(_)) => {
                if let Value::Number(right) = $self.pop() {
                    if let Value::Number(left) = $self.pop() {
                        $self.push(Value::Number(left $op right));
                    }
                }
            },
            _ => return Err(InterpretError::RuntimeError(LoxError::new("Operands must be two numbers", $get_line!())))
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

#[derive(Debug)]
struct CallFrame {
    function: Rc<RefCell<FunctionObject>>,
    ip: usize,
    slots: usize,
}

// Note: the VM leaks memory when handling objects?
pub struct VM {
    frames: Vec<CallFrame>,
    stack: RefCell<Vec<Value>>,
    stack_top: Cell<usize>,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
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
        let function = Compiler::new(source, FunctionType::Script)
            .compile()
            .map_err(|e| InterpretError::CompileError(e))?;

        self.push(Value::Obj(Box::new(Object::Function(Rc::clone(&function)))));
        self.call(Rc::clone(&function), 0, 0)?;

        self.run()
    }

    // Note: assumes theres is a callframe at the top of self.frames
    fn run(&mut self) -> Result<(), InterpretError> {
        let mut curr_frame = self.frames.len() - 1;

        macro_rules! frame {
            () => {
                self.frames[curr_frame]
            };
        }
        macro_rules! frame_slots {
            ($slot:expr) => {
                self.stack.borrow()[self.frames[curr_frame].slots + $slot].clone()
            };
        }
        macro_rules! frame_slots_mut {
            ($slot:expr) => {
                self.stack.borrow_mut()[self.frames[curr_frame].slots + $slot]
            };
        }
        macro_rules! frame_chunk {
            () => {
                self.frames[curr_frame]
                    .function
                    .deref()
                    .borrow()
                    .chunk
                    .deref()
                    .borrow()
            };
        }
        macro_rules! get_line {
            () => {
                frame_chunk!().lines[frame!().ip]
            };
        }
        macro_rules! read_byte {
            () => {{
                let ip = frame!().ip;
                let ret = frame_chunk!().instructions[ip];
                frame!().ip += 1;
                ret
            }};
        }
        macro_rules! read_short {
            () => {{
                frame!().ip += 2;
                let ip = frame!().ip;
                let prev_instr = frame_chunk!().instructions[ip - 2];
                let curr_instr = frame_chunk!().instructions[ip - 1];
                ((prev_instr as u16) << 8) | curr_instr as u16
            }};
        }
        macro_rules! read_constant {
            () => {{
                let byte = read_byte!();
                let val = frame_chunk!().constants[byte as usize].clone();
                val
            }};
        }

        if DEBUG {
            println!("\n== VM BACKTRACE ==")
        }
        while frame!().ip < frame_chunk!().instructions.len() {
            if DEBUG {
                print!("          ");
                for i in 0..self.stack_top.get() {
                    print!("[{}]", &self.stack.borrow()[i]);
                }
                println!();

                frame_chunk!().disassemble_instruction_at(frame!().ip);
            }

            #[allow(unreachable_patterns)]
            match read_byte!().into() {
                OpCode::Call => {
                    let arg_count = read_byte!();
                    let callee = self.peek(arg_count as usize);
                    let line = get_line!();
                    self.call_value(callee, arg_count, line)?;
                    curr_frame = self.frames.len() - 1;
                }
                OpCode::Return => {
                    let result = self.pop();
                    if self.frames.len() == 1 {
                        self.pop();
                        return Ok(());
                    }
                    let top = frame!().slots;
                    self.stack_top.set(top);
                    self.push(result);
                    curr_frame -= 1;
                    self.frames.pop();
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::Jump => {
                    let offset = read_short!();
                    frame!().ip += offset as usize;

                }
                OpCode::JumpIfFalse => {
                    let offset = read_short!();
                    if self.peek(0).is_falsey() {
                        frame!().ip += offset as usize;
                    }
                }
                OpCode::Loop => {
                    let offset = read_short!();
                    frame!().ip -= offset as usize;
                }
                OpCode::Print => println!("{}", self.pop()),
                OpCode::DefineGlobal => {
                    let name = read_constant!().clone();
                    if let Value::Obj(box Object::String(name)) = name {
                        self.globals.insert(name, self.peek(0));
                        self.pop();
                    }
                }
                OpCode::GetGlobal => {
                    let name = read_constant!();
                    if let Value::Obj(box Object::String(name)) = name {
                        if let Some(value) = self.globals.get(&name) {
                            self.push(value.clone());
                        } else {
                            return Err(InterpretError::RuntimeError(LoxError::new(
                                "Undefined variable.",
                                get_line!(),
                            )));
                        }
                    }
                }
                OpCode::SetGlobal => {
                    let name = read_constant!();
                    if let Value::Obj(box Object::String(name)) = name {
                        if self.globals.contains_key(&name) {
                            self.globals.insert(name, self.peek(0));
                        } else {
                            return Err(InterpretError::RuntimeError(LoxError::new(
                                "Undefined variable.",
                                get_line!(),
                            )));
                        }
                    }
                }
                OpCode::GetLocal => {
                    let slot = read_byte!();
                    let local = frame_slots!(slot as usize);
                    self.push(local);
                }
                OpCode::SetLocal => {
                    let slot = read_byte!();
                    frame_slots_mut!(slot as usize) = self.peek(0).clone();
                }
                OpCode::Constant => {
                    let constant = read_constant!();
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
                            get_line!(),
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
                            get_line!(),
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
                            get_line!(),
                        )));
                    }
                }
                OpCode::Substract => binary_op_number!(self, -, get_line),
                OpCode::Multiply => binary_op_number!(self, *, get_line),
                OpCode::Divide => binary_op_number!(self, /, get_line),

                OpCode::Negate => {
                    if let Value::Number(x) = self.peek(0) {
                        self.stack.borrow_mut()[self.stack_top.get() - 1] = Value::Number(-x);
                    } else {
                        return Err(InterpretError::RuntimeError(LoxError::new(
                            "Operand must be a number",
                            get_line!(),
                        )));
                    }
                }
                OpCode::Not => self.push(Value::Bool(self.pop().is_falsey())),
                _ => {
                    return Err(InterpretError::CompileError(LoxError::new(
                        "Unknown OpCode",
                        get_line!(),
                    )))
                }
            }
        }
        Ok(())
    }

    fn push(&self, value: Value) {
        if self.stack_top.get() == self.stack.borrow().len() {
            self.stack.borrow_mut().push(value);
        } else {
            self.stack.borrow_mut()[self.stack_top.get()] = value;
        }
        self.stack_top.set(self.stack_top.get() + 1);
    }

    fn pop(&self) -> Value {
        if self.stack_top.get() == self.stack.borrow().len() {
            self.stack_top.set(self.stack_top.get() - 1); 
            self.stack
                .borrow_mut()
                .pop()
                .expect("Can't pop an empty stack.")
        } else {
            self.stack_top.set(self.stack_top.get() - 1); 
            self.stack.borrow()[self.stack_top.get()].clone()
        }
    }

    fn peek(&self, distance: usize) -> Value {
        self.stack.borrow()[self.stack_top.get() - 1 - distance].clone()
    }

    fn call_value(&mut self, callee: Value, arg_count: u8, line: usize) -> Result<(), InterpretError> {
        if let Value::Obj(box Object::Function(f)) = callee {
            self.call(f, arg_count, line)
        } else {
            Err(InterpretError::RuntimeError(LoxError::new("Can only call functions and classes.",line)))
        }
        
    }

    fn call(&mut self, function: Rc<RefCell<FunctionObject>>, arg_count: u8, line: usize) -> Result<(), InterpretError> {
        if function.deref().borrow().arity != arg_count as usize {
            return Err(InterpretError::RuntimeError(LoxError::new("Wrong number of arguments.",line)));
        }
        self.frames.push(CallFrame {
            function: Rc::clone(&function),
            ip: 0,
            slots: self.stack_top.get() - arg_count as usize - 1,
        });
        Ok(())
    }
}
