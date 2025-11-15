use std::{
    io::{pipe, Read, Write},
    sync::{Arc, Mutex},
};

use crate::{
    error::Result,
    eval::{Engine, ScriptValue},
    parser::Parser,
    validate::{ScriptType, Validator},
};

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
    O: Write + 'static,
{
    let tokens = lexer::tokenize(src)?;
    let ast = Parser::new(src, tokens).parse()?;

    let print_type = ScriptType::Function {
        params: vec![("_".into(), ScriptType::Str)],
        ret: Box::new(ScriptType::Void),
    };

    let validator = Validator::new(src)
        .with_global("print", print_type.clone())
        .with_global("println", print_type.clone());

    validator.validate(&ast)?;

    let stdout1 = Arc::new(Mutex::new(out));
    let stdout2 = stdout1.clone();

    let print_fn = move |args: &[Arc<ScriptValue>]| {
        if let Some(arg) = args.first() {
            let mut out = stdout1.lock().unwrap();
            write!(out, "{arg}").unwrap();
        }
        ScriptValue::Void
    };

    let println_fn = move |args: &[Arc<ScriptValue>]| {
        if let Some(arg) = args.first() {
            let mut out = stdout2.lock().unwrap();
            writeln!(out, "{arg}").unwrap();
        }
        ScriptValue::Void
    };

    let engine = Engine::default()
        .with_global("print", print_fn)
        .with_global("println", println_fn);

    engine.eval(&ast);

    Ok(())
}
