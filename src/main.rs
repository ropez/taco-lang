use std::{
    env::args,
    fs,
    io::{pipe, Read},
};

use eval::Engine;

mod eval;
mod lexer;
mod parser;
mod interp;

fn main() {
    let mut args = args();
    let _ = args.next().expect("script name");
    let script = args.next().expect("one argument");

    let src = fs::read_to_string(script).unwrap();

    let tokens = lexer::tokenize(&src);

    // println!("{:?}", &tokens);

    let ast = parser::parse(tokens);

    // println!("{:?}", &ast);

    let (mut reader, writer) = pipe().unwrap();

    let engine = Engine::new(writer);
    engine.eval(&ast);

    drop(engine);

    let mut output = String::new();
    reader.read_to_string(&mut output).unwrap();
    println!("{output}");
}
