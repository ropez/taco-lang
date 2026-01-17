use std::{collections::HashMap, io::Write, path::PathBuf, sync::Arc};

use crate::{
    error::Error,
    ext::{NativeFunction, NativeFunctionRef, NativeMethod, NativeMethodRef},
    ident::Ident,
    interpreter::Interpreter,
    output_adapter::OutputAdapter,
    parser::Parser,
    script_type::{UnionType, ScriptType},
    script_value::{ScriptValue, Tuple},
    type_scope::{TypeDefinition},
    validate::Validator,
};

use async_lock::Mutex;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

pub mod error;
pub mod ext;
pub mod ident;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod script_type;
pub mod script_value;
pub mod type_scope;
pub mod validate;

mod fmt;
mod interpolation;
mod output_adapter;
mod stdlib;

#[cfg(test)]
mod tests;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn wasm_main(src: &str) -> String {
    let stdout = OutputAdapter::new();

    match check_call(src, stdout.clone()) {
        Ok(_) => stdout.output(),
        Err(err) => {
            format!("{err}")
        }
    }
}

pub fn check_output(src: &str) -> Result<String, Error> {
    let stdout = OutputAdapter::new();

    check_call(src, stdout.clone())?;

    Ok(stdout.output())
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

    #[cfg(feature = "pipe")]
    {
        let errors = interpreter.tracker.wait_all();
        if let Some(err) = errors.first() {
            Err(err.clone().into_source_error(src))
        } else {
            Ok(())
        }
    }

    #[cfg(not(feature = "pipe"))]
    Ok(())
}

#[derive(Clone, Debug, Default)]
pub struct TestStats {
    pub succeeded: i32,

    pub failures: Vec<TestFailure>,
    pub errors: Vec<TestError>,
}

#[derive(Clone, Debug)]
pub struct TestFailure {
    pub test_name: Ident,
    pub error: String,
    pub output: String,
}

#[derive(Clone, Debug)]
pub struct TestError {
    pub file_name: PathBuf,
    pub error: String,
}

impl TestStats {
    pub fn update(&mut self, other: Self) {
        self.succeeded += other.succeeded;
        self.failures.extend(other.failures);
        self.errors.extend(other.errors);
    }

    pub fn error(err: impl Into<TestError>) -> Self {
        Self {
            succeeded: 0,
            failures: Vec::new(),
            errors: vec![err.into()],
        }
    }
}

pub fn run_tests(src: &str) -> Result<TestStats, Error> {
    let stdout = OutputAdapter::new();

    let (validator, interpreter) = setup(stdout.clone());

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
        if let ScriptValue::ScriptFunction(f) = &value {
            if f.function.params.items().is_empty() {
                eprint!("  {ident}...");

                let eval_result = interpreter.eval_callable(value, &Tuple::identity());

                match eval_result {
                    Ok(_) => {
                        eprintln!("\x1b[32m ok \x1b[0m");
                        stats.succeeded += 1;
                    }
                    Err(err) => {
                        eprintln!("\x1b[31m failed\x1b[0m");
                        stats.failures.push(TestFailure {
                            test_name: ident,
                            error: err.into_source_error(src).to_string(),
                            output: stdout.output(),
                        });
                    }
                }

                stdout.clear();
            } else {
                eprintln!("Unexpected arguments");
            }
        }
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
    // Types of global values (by name of value)
    global_types: HashMap<Ident, ScriptType>,
    global_values: HashMap<Ident, ScriptValue>,
    methods: HashMap<(Ident, Ident), NativeMethodRef>,

    // Global types (by name of type)
    types: HashMap<Ident, TypeDefinition>,
}

impl Builder {
    pub fn add_global(
        &mut self,
        name: impl Into<Ident>,
        script_type: ScriptType,
        value: ScriptValue,
    ) {
        let key = name.into();
        self.global_types.insert(key.clone(), script_type);
        self.global_values.insert(key.clone(), value);
    }

    pub fn add_function<T>(&mut self, name: impl Into<Ident>, func: T)
    where
        T: NativeFunction + Send + Sync + 'static,
    {
        let f = NativeFunctionRef::from(func);
        self.add_global(
            name,
            ScriptType::NativeFunction(f.clone()),
            ScriptValue::NativeFunction(f.clone()),
        );
    }

    pub fn add_method<T>(&mut self, ns: impl Into<Ident>, name: impl Into<Ident>, method: T)
    where
        T: NativeMethod + Send + Sync + 'static,
    {
        self.methods
            .insert((ns.into(), name.into()), NativeMethodRef::from(method));
    }

    pub fn add_union(&mut self, name: impl Into<Ident>, def: Arc<UnionType>) {
        self.types
            .insert(name.into(), TypeDefinition::UnionDefinition(def));
    }

    fn build_validator(&self) -> Validator {
        Validator::default()
            .with_types(self.types.clone())
            .with_globals(self.global_types.clone())
            .with_methods(self.methods.clone())
    }

    fn build_interpreter(&self) -> Interpreter {
        Interpreter::default()
            .with_types(self.types.clone())
            .with_globals(self.global_values.clone())
            .with_methods(self.methods.clone())
    }
}
