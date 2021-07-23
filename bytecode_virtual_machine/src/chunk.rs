use crate::define_enum;
use crate::value::Value;

use std::convert::{From, Into};
use std::fmt;

define_enum! {
    OpCode,
    Return = 0,
    Constant = 1,
    Negate = 2,
    Add = 3,
    Substract = 4,
    Multiply = 5,
    Divide = 6,
    Nil = 7,
    True = 8,
    False = 9,
    Not = 10,
    Equal = 11,
    Greater = 12,
    Less = 13,
    Print = 14,
    Pop = 15,
    DefineGlobal = 16,
    GetGlobal = 17,
    SetGlobal = 18,
    GetLocal = 19,
    SetLocal = 20,
    JumpIfFalse = 21,
    Jump = 22,
    Loop = 23,
    Call = 24,
    Closure = 25,
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use OpCode::*;
        write!(
            f,
            "{}",
            match self {
                Return => "RET",
                Constant => "CONSTANT",
                Negate => "NEG",
                Add => "ADD",
                Substract => "SUB",
                Multiply => "MUL",
                Divide => "DIV",
                Nil => "NIL",
                True => "TRUE",
                False => "FALSE",
                Not => "NOT",
                Equal => "EQ",
                Greater => "GT",
                Less => "LT",
                Print => "PRINT",
                Pop => "POP",
                DefineGlobal => "DEFGV",
                GetGlobal => "GETGV",
                SetGlobal => "SETGV",
                GetLocal => "GETLV",
                SetLocal => "SETLV",
                JumpIfFalse => "JMPF",
                Jump => "JMP",
                Loop => "LOOP",
                Call => "CALL",
                Closure => "CLSR",
            }
        )
    }
}

#[derive(Debug, Clone, Default, PartialOrd, PartialEq)]
pub struct Chunk {
    pub(crate) lines: Vec<usize>,
    pub(crate) instructions: Vec<u8>,
    pub(crate) constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            lines: Vec::new(),
            instructions: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn push_byte(&mut self, byte: u8, line: usize) {
        self.lines.push(line);
        self.instructions.push(byte);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0usize;
        while offset < self.instructions.len() {
            offset = self.disassemble_instruction_at(offset);
        }
    }

    pub fn disassemble_instruction_at(&self, mut offset: usize) -> usize {
        print!("{:04} ", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let instruction: OpCode = self.instructions[offset].into();

        #[allow(unreachable_patterns)]
        match instruction {
            OpCode::Return
            | OpCode::Pop
            | OpCode::Print
            | OpCode::Nil
            | OpCode::False
            | OpCode::True
            | OpCode::Add
            | OpCode::Substract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Equal
            | OpCode::Greater
            | OpCode::Less
            | OpCode::Not
            | OpCode::Negate => {
                println!("{}", instruction);
                offset += 1;
            }
            OpCode::Constant | OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal => {
                let constant = self.instructions[offset + 1];
                print!("{:12} {:4} '", instruction.to_string(), constant);
                //TODO: replace when Value type evolve:
                print!("{}", self.constants[constant as usize]);
                println!("'");
                offset += 2;
            }
            OpCode::GetLocal | OpCode::SetLocal | OpCode::Call => {
                let slot = self.instructions[offset + 1];
                println!("{:12} {:4}", instruction.to_string(), slot);
                offset += 2;
            }
            OpCode::Jump | OpCode::JumpIfFalse => {
                let mut jump = (self.instructions[offset + 1] as u16) << 8;
                jump |= self.instructions[offset + 2] as u16;
                println!(
                    "{:12} {:4} -> {}",
                    instruction.to_string(),
                    offset,
                    offset + 3 + jump as usize
                );
                offset += 3;
            }
            OpCode::Loop => {
                let mut jump = (self.instructions[offset + 1] as u16) << 8;
                jump |= self.instructions[offset + 2] as u16;
                println!(
                    "{:12} {:4} -> {}",
                    instruction.to_string(),
                    offset,
                    offset + 3 - jump as usize
                );
                offset += 3;
            }
            OpCode::Closure => {
                offset += 1;
                let constant = self.instructions[offset];
                offset += 1;
                println!(
                    "{:12} {:4} {}",
                    instruction.to_string(),
                    constant,
                    self.constants[constant as usize]
                );
            }
            _ => {
                println!("Unknown:{:?}", instruction);
                offset += 1;
            }
        }

        offset
    }
}
