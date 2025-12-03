use std::{
    collections::HashMap,
    io::{Read, Write, pipe},
    sync::{Arc, Mutex},
};

use crate::{
    error::Result,
    ident::{Ident, global},
    interpreter::Interpreter,
    parser::Parser,
    stdlib::{
        NativeFunction, NativeFunctionRef, NativeMethod, NativeMethodRef,
        list::{ListFind, ListMap, ListMapTo, ListPush, ListSort, ListSum, ListUnzip},
        string::StringLines,
    },
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
    stdlib::type_of::build(&mut builder);

    builder.add_method(global::LIST, "push", ListPush);
    builder.add_method(global::LIST, "find", ListFind);
    builder.add_method(global::LIST, "unzip", ListUnzip);
    builder.add_method(global::LIST, "sum", ListSum);
    builder.add_method(global::LIST, "sort", ListSort);
    builder.add_method(global::LIST, "map", ListMap);
    builder.add_method(global::LIST, "map_to", ListMapTo);
    builder.add_method(global::STRING, "lines", StringLines);

    let validator = builder.build_validator();
    validator.validate(&ast).map_err(|err| {
        let loc = err.loc;
        err.into_inner().into_source_error(src, loc)
    })?;

    let interpreter = builder.build_interpreter();
    interpreter.execute(&ast);

    Ok(())
}

#[derive(Default)]
struct Builder {
    functions: HashMap<Ident, NativeFunctionRef>,
    methods: HashMap<(Ident, Ident), NativeMethodRef>,
}

impl Builder {
    pub fn add_function<T>(&mut self, name: impl Into<Ident>, func: T)
    where
        T: NativeFunction + 'static,
    {
        self.functions
            .insert(name.into(), NativeFunctionRef::from(func));
    }

    pub fn add_method<T>(&mut self, ns: impl Into<Ident>, name: impl Into<Ident>, method: T)
    where
        T: NativeMethod + 'static,
    {
        self.methods
            .insert((ns.into(), name.into()), NativeMethodRef::from(method));
    }

    fn build_validator(&self) -> Validator {
        Validator::default()
            .with_functions(self.functions.clone())
            .with_methods(self.methods.clone())
    }

    fn build_interpreter(&self) -> Interpreter {
        Interpreter::default()
            .with_functions(self.functions.clone())
            .with_methods(self.methods.clone())
    }
}
