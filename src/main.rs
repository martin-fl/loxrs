use std::env;
use std::io;

use tree_walk_interpreter;
use bytecode_virtual_machine;

fn main() -> io::Result<()> {
    let mut args = env::args();

    let mut lox = tree_walk_interpreter::Lox::new();

    match args.len() {
        1 => lox.run_prompt()?,
        2 => lox.run_file(&args.nth(1).unwrap())?,
        _ => panic!("Usage: [script]"),
    }

    Ok(())
}
