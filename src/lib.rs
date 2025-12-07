use std::{
    collections::HashMap,
    io::{Read, Write, pipe},
    sync::{Arc, Mutex},
};

use crate::{
    error::Result,
    ident::{Ident, global},
    interpreter::{Interpreter, ScriptValue, Tuple},
    parser::Parser,
    stdlib::{
        NativeFunction, NativeFunctionRef, NativeMethod, NativeMethodRef, list::ListZip,
        record::RecordWithMethod,
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
    let (validator, interpreter) = setup(out);

    let tokens = lexer::tokenize(src)?;
    let ast = Parser::new(src, tokens).parse()?;

    validator.validate(&ast).map_err(|err| {
        let loc = err.loc;
        err.into_inner().into_source_error(src, loc)
    })?;

    interpreter.execute(&ast);

    Ok(())
}

pub fn run_tests<O>(src: &str, out: O) -> Result<()>
where
    O: Write + 'static,
{
    let (validator, interpreter) = setup(out);

    let tokens = lexer::tokenize(src)?;
    let ast = Parser::new(src, tokens).parse()?;

    validator.validate(&ast).map_err(|err| {
        let loc = err.loc;
        err.into_inner().into_source_error(src, loc)
    })?;

    let exported = interpreter
        .execute(&ast)
        .map_err(|err| err.into_source_error(src))?;

    for (ident, value) in &exported {
        if let ScriptValue::ScriptFunction { function, .. } = value {
            if function.params.is_empty() {
                eprint!("- {ident}");
                match interpreter.eval_callable(value, &Tuple::identity()) {
                    Ok(_) => eprintln!("\x1b[32m ok\x1b[0m"),
                    Err(err) => {
                        eprintln!("\x1b[32m failed\x1b[0m");
                        eprintln!("{}", err.into_source_error(src));
                    }
                }
            } else {
                eprintln!("Unexpected arguments");
            }
        }
    }

    Ok(())
}

fn setup<O>(out: O) -> (Validator, Interpreter)
where
    O: Write + 'static,
{
    let out = Arc::new(Mutex::new(out));

    let mut builder = Builder::default();

    stdlib::string::build(&mut builder);
    stdlib::list::build(&mut builder);
    stdlib::state::build(&mut builder);
    stdlib::print::build(&mut builder, out);
    stdlib::parse::build(&mut builder);

    #[cfg(feature = "json")]
    stdlib::json::build(&mut builder);

    stdlib::fs::build(&mut builder);
    stdlib::type_of::build(&mut builder);

    // XXX Namespace for functions (List::zip)
    builder.add_function("zip", ListZip);

    builder.add_method(global::REC, "with", RecordWithMethod);

    let validator = builder.build_validator();
    let interpreter = builder.build_interpreter();
    (validator, interpreter)
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
