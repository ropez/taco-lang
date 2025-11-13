use std::io::{pipe, Read, Write};

use crate::{error::Result, eval::Engine, parser::Parser};

pub mod error;
pub mod eval;
mod interp;
pub mod lexer;
pub mod parser;
pub mod validate;

pub fn check_output(src: &str) -> Result<String> {
    let (mut reader, writer) = pipe().expect("create pipe");

    check_call(src, writer)?;

    let mut out = String::new();
    reader.read_to_string(&mut out).unwrap();
    Ok(out)
}

pub fn check_call<O>(src: &str, out: O) -> Result<()>
where
    O: Write,
{
    let tokens = lexer::tokenize(src)?;
    let ast = Parser::new(src, tokens).parse()?;

    validate::validate(src, &ast)?;

    let engine = Engine::new(out);
    engine.eval(&ast);

    Ok(())
}
