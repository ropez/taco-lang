use std::sync::Arc;

use crate::{
    Builder,
    error::ScriptError,
    ext::NativeFunction,
    interpreter::Interpreter,
    parser::{Record, TypeExpression},
    script_value::{ContentType, ScriptValue, Tuple, TupleItem},
    stdlib,
    validate::{ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("int::parse", ParseIntFunc);
    builder.add_function("Range::parse", ParseRangeFunc);
}

pub(crate) struct ParseFunc {
    def: Arc<Record>,
}

impl ParseFunc {
    pub(crate) fn new(def: Arc<Record>) -> Self {
        Self { def }
    }
}

impl NativeFunction for ParseFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let (input, content_type) = arguments.single()?.as_string_and_type()?;

        let value = match content_type {
            ContentType::Undefined => parse_default(&self.def, &input)?,

            #[cfg(feature = "json")]
            ContentType::Json => stdlib::json::parse_json(&self.def, &input)?,

            #[allow(unreachable_patterns)]
            _ => Err(ScriptError::panic("Parser not found"))?,
        };

        Ok(ScriptValue::Rec {
            def: Arc::clone(&self.def),
            value: Arc::new(value),
        })
    }
}

fn parse_default(rec: &Record, input: &str) -> Result<Tuple, ScriptError> {
    let mut values = Vec::new();
    let mut tokens = input.split_ascii_whitespace();
    for d in rec.params.as_ref() {
        values.push(TupleItem::new(
            d.name.clone(),
            match d.type_expr.as_ref() {
                TypeExpression::Int => ScriptValue::Int(
                    tokens
                        .next()
                        .ok_or_else(|| ScriptError::panic("Expected token"))?
                        .parse::<i64>()
                        .map_err(ScriptError::panic)?,
                ),
                TypeExpression::Str => ScriptValue::string(
                    tokens
                        .next()
                        .ok_or_else(|| ScriptError::panic("Expected token"))?,
                ),
                o => Err(ScriptError::panic(format!("Don't know how to parse {o:?}")))?,
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

    fn return_type(&self, _: &TupleType) -> ScriptType {
        ScriptType::Int
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

    fn return_type(&self, _: &TupleType) -> ScriptType {
        ScriptType::Range
    }
}
