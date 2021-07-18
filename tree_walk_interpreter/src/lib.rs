#![feature(box_patterns, result_cloned)]

mod ast;
mod interpreter;
mod lox;
mod parser;
mod scanner;
mod token;

pub use lox::Lox;
