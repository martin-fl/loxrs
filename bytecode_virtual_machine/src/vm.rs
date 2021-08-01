use crate::chunk::OpCode;
use crate::compiler::{Compiler, FunctionType};
use crate::error::{EmitError, Error};
use crate::value::{Closure, NativeFn, Object, Value};
use crate::DEBUG;

use std::collections::HashMap;
use std::fs::File;
use std::io::{self, Read, Write};
use std::ops::Deref;
use std::rc::Rc;
use std::time;

#[derive(Debug)]
struct CallFrame {
    closure: Closure,
    ip: usize,
    slots: usize,
}

// Note: the VM leaks memory when handling objects? because of rust's drop rule with regards to
// boxes i'm not sure
pub struct VM {
    source: String,
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    stack_top: usize,
    globals: HashMap<String, Value>,
}

impl EmitError for VM {
    fn emit_error(&self, message: &str) -> Error {
        Error::new("", message, 0)
    }
}

impl VM {
    pub fn new() -> Self {
        let mut ret = Self {
            source: String::from("  "),
            frames: Vec::new(),
            stack: Vec::with_capacity(u8::MAX as usize), //RefCell::new(Vec::new()),
            stack_top: 0,                                //Cell::new(0),
            globals: HashMap::new(),
        };

        ret.define_native("clock".to_string(), Rc::new(clock_native));

        ret
    }

    pub fn run_prompt(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        while self.source.len() > 1 {
            self.source.clear();
            print!("lox> ");
            let _ = stdout.flush();
            stdin.read_line(&mut self.source)?;
            match self.interpret() {
                Ok(()) => {}
                Err(es) => es.iter().for_each(|e| eprint!("{}", e)),
            }
        }

        Ok(())
    }

    pub fn run_file(&mut self, script_path: &str) -> io::Result<()> {
        let mut script_file = File::open(script_path)?;
        script_file.read_to_string(&mut self.source)?;

        match self.interpret() {
            Ok(()) => {}
            Err(es) => es.iter().for_each(|e| eprint!("{}", e)),
        }

        Ok(())
    }

    fn interpret(&mut self) -> Result<(), Vec<Error>> {
        let function = Compiler::new(&self.source, FunctionType::Script).compile()?;

        function.deref().borrow_mut().name = Object::String("script".to_string());

        self.push(Value::Obj(Box::new(Object::Function(Rc::clone(&function)))));
        let closure = Closure(Rc::clone(&function));
        self.pop();
        self.push(Value::Obj(Box::new(Object::Closure(closure.clone()))));
        self.call(closure, 0, 0).map_err(|e| vec![e])?;

        self.run().map_err(|e| vec![e])
    }

    // Note: assumes theres is a callframe at the top of self.frames
    fn run(&mut self) -> Result<(), Error> {
        let mut curr_frame = self.frames.len() - 1;

        macro_rules! frame {
            () => {
                self.frames[curr_frame]
            };
        }
        macro_rules! frame_slots {
            ($slot:expr) => {
                self.stack[self.frames[curr_frame].slots + $slot]
            };
        }
        macro_rules! frame_chunk {
            () => {
                self.frames[curr_frame]
                    .closure
                    .0
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
        macro_rules! error {
            ($message:expr) => {{
                let line = get_line!();
                self.emit_error($message).at_line(line).with_content(
                    self.source
                        .lines()
                        .nth(line - 1)
                        .expect("Error at a line that doesn't exist."),
                )
            }};
        }

        macro_rules! binary_op_number {
            ($op:tt) => {
                match (self.peek(0),self.peek(1)) {
                    (Value::Number(_),Value::Number(_)) => {
                        if let Value::Number(right) = self.pop() {
                            if let Value::Number(left) = self.pop() {
                                self.push(Value::Number(left $op right));
                            }
                        }
                    },
                    _ => return Err(error!("Operands must be two numbers"))
                }
            };
        }

        if DEBUG {
            println!("\n== VM BACKTRACE ==")
        }
        while frame!().ip < frame_chunk!().instructions.len() {
            if DEBUG {
                print!("          ");
                for i in 0..self.stack_top {
                    print!("[{}]", &self.stack[i]);
                }
                println!();

                frame_chunk!().disassemble_instruction_at(frame!().ip);
            }

            #[allow(unreachable_patterns)]
            match read_byte!().into() {
                OpCode::Call => {
                    let arg_count = read_byte!() as usize;
                    let callee = self.peek(arg_count).clone();
                    let line = get_line!();
                    self.call_value(callee, arg_count, line)?;
                    curr_frame = self.frames.len() - 1;
                }
                OpCode::Closure => {
                    let fun = if let Value::Obj(box Object::Function(fun)) = read_constant!() {
                        fun
                    } else {
                        unreachable!()
                    };
                    let closure = Value::Obj(Box::new(Object::Closure(Closure(Rc::clone(&fun)))));
                    self.push(closure);
                }
                OpCode::Return => {
                    let result = self.pop();
                    if self.frames.len() == 1 {
                        self.pop();
                        return Ok(());
                    }
                    let top = frame!().slots;
                    self.stack_top = top;
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
                    let name = read_constant!();
                    if let Value::Obj(box Object::String(name)) = name {
                        self.globals.insert(name, self.peek(0).clone());
                        self.pop();
                    }
                }
                OpCode::GetGlobal => {
                    let name = read_constant!();
                    if let Value::Obj(box Object::String(name)) = name {
                        if let Some(value) = self.globals.get(&name) {
                            let value = value.clone();
                            self.push(value);
                        } else {
                            return Err(error!("Undefined variable."));
                        }
                    }
                }
                OpCode::SetGlobal => {
                    let name = read_constant!();
                    if let Value::Obj(box Object::String(name)) = name {
                        if self.globals.contains_key(&name) {
                            self.globals.insert(name, self.peek(0).clone());
                        } else {
                            return Err(error!("Undefined variable."));
                        }
                    }
                }
                OpCode::GetLocal => {
                    let slot = read_byte!();
                    let local = frame_slots!(slot as usize).clone();
                    self.push(local);
                }
                OpCode::SetLocal => {
                    let slot = read_byte!();
                    frame_slots!(slot as usize) = self.peek(0).clone();
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
                    _ => return Err(error!("Operands must be two numbers")),
                },
                OpCode::Less => match (self.peek(0), self.peek(1)) {
                    (Value::Number(_), Value::Number(_)) => {
                        if let Value::Number(right) = self.pop() {
                            if let Value::Number(left) = self.pop() {
                                self.push(Value::Bool(left < right));
                            }
                        }
                    }
                    _ => return Err(error!("Operands must be two numbers")),
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
                        return Err(error!("Operands must be two numbers"));
                    }
                }
                OpCode::Substract => binary_op_number!(-),
                OpCode::Multiply => binary_op_number!(*),
                OpCode::Divide => binary_op_number!(/),

                OpCode::Negate => {
                    if let &Value::Number(x) = self.peek(0) {
                        self.stack[self.stack_top - 1] = Value::Number(-x);
                    } else {
                        return Err(error!("Operand must be a number"));
                    }
                }
                OpCode::Not => {
                    let poped = self.pop().is_falsey();
                    self.push(Value::Bool(poped));
                }
                _ => return Err(error!("Unknown OpCode")),
            }
        }
        Ok(())
    }

    fn push(&mut self, value: Value) {
        if self.stack_top == self.stack.len() {
            self.stack.push(value);
        } else {
            self.stack[self.stack_top] = value;
        }
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        if self.stack_top == self.stack.len() {
            self.stack_top -= 1;
            self.stack.pop().expect("Can't pop an empty stack")
        } else {
            self.stack_top -= 1;
            self.stack.remove(self.stack_top)
        }
    }

    fn peek<'a>(&'a self, distance: usize) -> &'a Value {
        &self.stack[self.stack_top - 1 - distance]
    }

    fn call_value(&mut self, callee: Value, arg_count: usize, line: usize) -> Result<(), Error> {
        if callee.is_closure() || callee.is_native() {
            match callee {
                Value::Obj(box Object::Closure(c)) => self.call(c, arg_count, line),
                Value::Obj(box Object::Native(f)) => {
                    let result = f.0.deref()(
                        arg_count,
                        &self.stack[(self.stack_top - arg_count)..self.stack_top],
                    );
                    self.stack_top -= arg_count + 1;
                    self.push(result);
                    Ok(())
                }
                _ => unreachable!(),
            }
        } else {
            Err(self
                .emit_error("Can only call functions and classes.")
                .at_line(line)
                .with_content(self.source.lines().nth(line).unwrap()))
        }
    }

    fn call(&mut self, closure: Closure, arg_count: usize, line: usize) -> Result<(), Error> {
        if closure.0.deref().borrow().arity != arg_count {
            return Err(self
                .emit_error("Wrong number of arguments.")
                .at_line(line)
                .with_content(self.source.lines().nth(line).unwrap()));
        }
        self.frames.push(CallFrame {
            closure,
            ip: 0,
            slots: self.stack_top - arg_count - 1,
        });
        Ok(())
    }

    fn define_native(&mut self, name: String, function: Rc<dyn Fn(usize, &[Value]) -> Value>) {
        self.push(Value::Obj(Box::new(Object::String(name.clone()))));
        self.push(Value::Obj(Box::new(Object::Native(NativeFn(Rc::clone(
            &function,
        ))))));
        self.globals.insert(name, self.stack[1].clone());
        self.pop();
        self.pop();
    }
}

// NATIVES
fn clock_native(_: usize, _: &[Value]) -> Value {
    Value::Number(
        time::SystemTime::now()
            .duration_since(time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}
