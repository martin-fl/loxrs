use std::env;
use std::io;

#[cfg(not(feature = "treewalk"))]
use bytecode_virtual_machine::vm::VM;
#[cfg(feature = "treewalk")]
use tree_walk_interpreter::Lox;

fn main() -> io::Result<()> {
    #[cfg(feature = "treewalk")]
    let mut lox = Lox::new();
    #[cfg(not(feature = "treewalk"))]
    let mut lox = VM::new();

    let mut args = env::args();

    match args.len() {
        1 => lox.run_prompt()?,
        2 => lox.run_file(&args.nth(1).unwrap())?,
        _ => panic!("Usage: [script]"),
    }

    Ok(())
}
