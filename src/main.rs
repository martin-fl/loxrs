use std::env;
use std::io;

use tree_walk_interpreter::Interpreter;

fn main() -> io::Result<()> {
    let mut args = env::args();

    let mut interpreter = Interpreter::new();

    if args.len() > 2 {
        panic!("Usage [script]");
    } else if args.len() == 2 {
        interpreter.run_file(&args.nth(1).unwrap())?;
    } else {
        interpreter.run_prompt()?;
    }

    Ok(())
}
