use std::{env::args, fs, io::stdout};

use eval::Engine;

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

    let ast = parser::parse(tokens);

    // println!("{:?}", &ast);

    let engine = Engine::new(stdout());
    engine.eval(&ast);

    Ok(())
}
