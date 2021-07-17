#![feature(box_patterns)]

mod ast;
mod interpreter;
mod parser;
mod scanner;
mod token;

pub use interpreter::Interpreter;
