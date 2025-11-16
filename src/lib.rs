use std::{
    io::{pipe, Read, Write},
    sync::{Arc, Mutex},
};

use crate::{error::Result, eval::Engine, parser::Parser, validate::Validator};

pub mod error;
pub mod eval;
mod interp;
pub mod lexer;
pub mod parser;
pub mod validate;
mod extensions;

pub fn check_output(src: &str) -> Result<String> {
    let (mut reader, writer) = pipe().expect("create pipe");

    check_call(src, writer)?;

    let mut out = String::new();
    reader.read_to_string(&mut out).unwrap();
    Ok(out)
}

pub fn check_call<O>(src: &str, out: O) -> Result<()>
where
    O: Write + 'static,
{
    let tokens = lexer::tokenize(src)?;
    let ast = Parser::new(src, tokens).parse()?;

    let out = Arc::new(Mutex::new(out));

    let mut validator = Validator::new(src);
    let mut engine = Engine::default();

    for (name, f) in extensions::fs::create() {
        let name = name.into();
        validator = validator.with_global(Arc::clone(&name), f.script_type);
        engine = engine.with_global(Arc::clone(&name), f.func);
    }

    for (name, f) in extensions::print::create(out) {
        let name = name.into();
        validator = validator.with_global(Arc::clone(&name), f.script_type);
        engine = engine.with_global(Arc::clone(&name), f.func);
    }

    validator.validate(&ast)?;
    engine.eval(&ast);

    Ok(())
}
