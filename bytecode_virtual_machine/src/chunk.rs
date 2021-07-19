use std::convert::{From, Into};

use crate::value::Value;

macro_rules! define_op_codes {
    ($($variant:ident = $byte:expr,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(u8)]
        pub enum OpCode {
            $($variant = $byte,)*
        }

        impl From<u8> for OpCode {
            fn from(byte: u8) -> Self {
                use OpCode::*;
                match byte {
                    $($byte => $variant,)*
                    _ => panic!("Unknown OpCode"),
                }
            }
        }
    };
}

define_op_codes! {
    Return = 0,
    Constant = 1,
    Negate = 2,
    Add = 3,
    Substract = 4,
    Multiply = 5,
    Divide = 6,
}

#[derive(Debug)]
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

    pub fn push_op(&mut self, op: OpCode, line: usize) {
        self.lines.push(line);
        self.instructions.push(op as u8);
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

        match instruction {
            OpCode::Return
            | OpCode::Add
            | OpCode::Substract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Negate => {
                println!("{:?}", instruction);
                offset += 1;
            }
            OpCode::Constant => {
                let constant = self.instructions[offset + 1];
                print!("{:->16?} {:4} '", instruction, constant);
                //TODO: replace when Value type evolve:
                print!("{}", self.constants[constant as usize]);
                println!("'");
                offset += 2;
            }
            _ => {
                println!("Unknown:{:?}", instruction);
                offset += 1;
            }
        }

        offset
    }
}
