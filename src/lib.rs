use std::{
    collections::HashMap,
    io::{Read, Write, pipe},
    sync::{Arc, Mutex},
};

use crate::{
    error::Error,
    ext::{NativeFunction, NativeFunctionRef, NativeMethod, NativeMethodRef},
    ident::Ident,
    interpreter::{Interpreter, ScriptValue, Tuple},
    parser::Parser,
    validate::Validator,
};

pub mod error;
pub mod ext;
pub mod ident;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod validate;

mod fmt;
mod interpopation;
mod stdlib;

#[cfg(test)]
mod tests;

pub fn check_output(src: &str) -> Result<String, Error> {
    let (mut reader, writer) = pipe().expect("create pipe");

    check_call(src, writer)?;

    let mut out = String::new();
    reader.read_to_string(&mut out).unwrap();
    Ok(out)
}

pub fn check_call<O>(src: &str, out: O) -> Result<(), Error>
where
    O: Write + Send + Sync + 'static,
{
    let (validator, interpreter) = setup(out);

    let tokens = lexer::tokenize(src).map_err(|err| err.into_source_error(src))?;
    let ast = Parser::new(src, tokens)
        .parse()
        .map_err(|err| err.into_source_error(src))?;

    validator
        .validate(&ast)
        .map_err(|err| err.into_source_error(src))?;

    interpreter
        .execute(&ast)
        .map_err(|err| err.into_source_error(src))?;

    Ok(())
}

#[derive(Clone, Debug, Default)]
pub struct TestStats {
    pub failed: i32,
    pub succeeded: i32,
    pub errors: i32,
}

impl TestStats {
    pub fn update(&mut self, other: &Self) {
        self.failed += other.failed;
        self.succeeded += other.succeeded;
        self.errors += other.errors;
    }

    pub fn error() -> Self {
        Self {
            failed: 0,
            succeeded: 0,
            errors: 1,
        }
    }
}

pub fn run_tests(src: &str) -> Result<TestStats, Error> {
    let (mut reader, writer) = pipe().expect("create pipe");

    let (validator, interpreter) = setup(writer);

    let tokens = lexer::tokenize(src).map_err(|err| err.into_source_error(src))?;
    let ast = Parser::new(src, tokens)
        .parse()
        .map_err(|err| err.into_source_error(src))?;

    validator
        .validate(&ast)
        .map_err(|err| err.into_source_error(src))?;

    let exported = interpreter
        .execute(&ast)
        .map_err(|err| err.into_source_error(src))?;

    let mut stats = TestStats::default();
    for (ident, value) in exported {
        if let ScriptValue::ScriptFunction { function, .. } = &value {
            if function.params.is_empty() {
                eprint!("  {ident}...");

                let eval_result = interpreter.eval_callable(value, &Tuple::identity());
                // XXX Read and display output for each test
                match eval_result {
                    Ok(_) => {
                        eprintln!("\x1b[32m ok \x1b[0m");
                        stats.succeeded += 1;
                    }
                    Err(err) => {
                        eprintln!("\x1b[31m failed\x1b[0m");
                        eprintln!("{}", err.into_source_error(src));
                        stats.failed += 1;
                    }
                }
            } else {
                eprintln!("Unexpected arguments");
            }
        }
    }

    drop(interpreter);
    drop(validator);

    let mut buf = String::new();
    reader.read_to_string(&mut buf).unwrap();
    if !buf.is_empty() {
        eprintln!();
        eprintln!("=== Captured output (all tests) ===");
        eprintln!("{buf}");
    }

    Ok(stats)
}

fn setup<O>(out: O) -> (Validator, Interpreter)
where
    O: Write + Send + Sync + 'static,
{
    let out = Arc::new(Mutex::new(out));

    let mut builder = Builder::default();

    stdlib::build(&mut builder, out);

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
        T: NativeFunction + Send + Sync + 'static,
    {
        self.functions
            .insert(name.into(), NativeFunctionRef::from(func));
    }

    pub fn add_method<T>(&mut self, ns: impl Into<Ident>, name: impl Into<Ident>, method: T)
    where
        T: NativeMethod + Send + Sync + 'static,
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
