use std::{fmt, sync::Arc};

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::NativeFunction,
    interpreter::Interpreter,
    script_type::{RecType, ScriptType, TupleType},
    script_value::{ContentType, ScriptValue, Tuple, TupleItem},
    stdlib,
};

pub fn build(builder: &mut Builder) {
    builder.add_function("int::parse", ParseIntFunc);
    builder.add_function("Range::parse", ParseRangeFunc);
}

pub(crate) struct ParseFunc {
    def: Arc<RecType>,
}

impl ParseFunc {
    pub(crate) fn new(def: Arc<RecType>) -> Self {
        Self { def }
    }
}

impl NativeFunction for ParseFunc {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn return_type(&self, _: &TupleType) -> Result<ScriptType, TypeError> {
        // XXX How to programmatically create a custom error type, or include Taco snippets in stdlib?
        let error_typ = ScriptType::Str;
        let value_typ = ScriptType::RecInstance(Arc::clone(&self.def));

        Ok(ScriptType::fallible_of(value_typ, error_typ))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let (input, content_type) = arguments.single()?.as_string_and_type()?;

        let parse_result = match content_type {
            ContentType::Undefined => parse_default(&self.def, &input),

            #[cfg(feature = "json")]
            ContentType::Json => stdlib::json::parse_json(&self.def, &input),

            #[allow(unreachable_patterns)]
            _ => Err(ScriptError::panic("Parser not found"))?,
        };

        Ok(match parse_result {
            Ok(val) => ScriptValue::ok(ScriptValue::Rec {
                def: Arc::clone(&self.def),
                value: Arc::new(val),
            }),
            Err(err) => ScriptValue::err(ScriptValue::string(err.msg)),
        })
    }
}

pub struct ParseError {
    msg: String,
}

impl ParseError {
    pub fn new(msg: impl fmt::Display) -> Self {
        Self {
            msg: msg.to_string(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

fn parse_default(rec: &RecType, input: &str) -> Result<Tuple, ParseError> {
    let mut values = Vec::new();
    let mut tokens = input.split_ascii_whitespace();
    for d in rec.params.items() {
        values.push(TupleItem::new(
            d.name.clone(),
            match &d.value {
                ScriptType::Int => ScriptValue::Int(
                    tokens
                        .next()
                        .ok_or_else(|| ParseError::new("Expected token"))?
                        .parse::<i64>()
                        .map_err(ParseError::new)?,
                ),
                ScriptType::Str => ScriptValue::string(
                    tokens
                        .next()
                        .ok_or_else(|| ParseError::new("Expected token"))?,
                ),
                o => Err(ParseError::new(format!("Don't know how to parse {o:?}")))?,
            },
        ));
    }

    Ok(Tuple::new(values))
}

struct ParseIntFunc;
impl NativeFunction for ParseIntFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let input = arguments.single()?.as_string()?;
        let val = input
            .parse()
            .map_err(|err| ScriptError::panic(format!("{err}, input: '{input}'")))?;
        Ok(ScriptValue::Int(val))
    }

    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn return_type(&self, _: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Int)
    }
}

struct ParseRangeFunc;
impl NativeFunction for ParseRangeFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let input = arguments.single()?.as_string()?;

        if let Some(n) = input.find('-') {
            let (l, r) = input.split_at(n);
            let l = l.parse().map_err(ScriptError::panic)?;
            let r = r[1..].parse().map_err(ScriptError::panic)?;

            Ok(ScriptValue::Range(l, r))
        } else {
            Err(ScriptError::panic("Parse error"))
        }
    }

    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn return_type(&self, _: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Range)
    }
}
