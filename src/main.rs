use std::{env::args, fs, io::stdout};

use eval::Engine;

use crate::parser::Parser;

mod error;
mod eval;
mod interp;
mod lexer;
mod parser;

fn main() {
    let mut args = args();
    let _ = args.next().expect("script name");
    let script = args.next().expect("one argument");

    let src = fs::read_to_string(script).unwrap();

    if let Err(err) = run(&src) {
        eprintln!("ERROR: {err}");
    }
}

fn run(src: &str) -> error::Result<()> {
    let tokens = lexer::tokenize(src)?;

    // println!("{:?}", &tokens);

    let ast = Parser::new(src, tokens).parse()?;

    // println!("{:?}", &ast);

    let engine = Engine::new(stdout());
    engine.eval(&ast);

    Ok(())
}
