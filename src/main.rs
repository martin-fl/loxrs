use std::env;
use std::io;

use tree_walk_interpreter::Lox;

fn main() -> io::Result<()> {
    let mut args = env::args();

    if args.len() > 2 {
        panic!("Usage: [script]");
    } else if args.len() == 2 {
        Lox::run_file(&args.nth(1).unwrap())?;
    } else {
        Lox::run_prompt()?;
    }

    Ok(())
}
