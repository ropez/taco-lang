use std::{
    collections::HashMap,
    io::{Read, Write, pipe},
    sync::{Arc, Mutex},
};

use crate::{
    error::Result,
    ident::Ident,
    interpreter::Interpreter,
    parser::Parser,
    stdlib::{NativeFunction, NativeMethod},
    validate::Validator,
};

pub mod error;
mod fmt;
pub mod ident;
mod interpopation;
pub mod interpreter;
pub mod lexer;
pub mod parser;
mod stdlib;
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

    let out = Arc::new(Mutex::new(out));

    let mut builder = Builder::default();

    stdlib::state::build(&mut builder);
    stdlib::print::build(&mut builder, out);
    stdlib::fs::build(&mut builder);

    let mut validator = Validator::new(src);
    let mut interpreter = Interpreter::default();

    for (name, f) in builder.functions {
        let name = name.as_str();
        validator = validator.with_native(name, Arc::clone(&f));
        interpreter = interpreter.with_native(name, Arc::clone(&f));
    }

    validator.validate(&ast)?;
    interpreter.execute(&ast);

    Ok(())
}

#[derive(Default)]
struct Builder {
    functions: HashMap<Ident, Arc<dyn NativeFunction>>,
    methods: HashMap<Ident, Arc<dyn NativeMethod>>,
}

impl Builder {
    pub fn add_function<T>(&mut self, name: Ident, func: T)
    where
        T: NativeFunction + 'static,
    {
        self.functions.insert(name, Arc::new(func));
    }

    pub fn add_method<T>(&mut self, name: Ident, method: T)
    where
        T: NativeMethod + 'static,
    {
        self.methods.insert(name, Arc::new(method));
    }
}
