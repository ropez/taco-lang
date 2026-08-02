use std::{fmt, num::ParseIntError, sync::Arc};

use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeResult},
    ext::{NativeFunction, NativeTypeMethod},
    interpreter::Interpreter,
    script_type::{RecType, ScriptType, TupleItemType, TupleType},
    script_value::{ContentType, ScriptValue, Tuple, TupleItem},
    stdlib,
    type_scope::TypeDefinition,
};

pub fn build(builder: &mut Builder) {
    let parse_error = RecType::new(
        "ParseError",
        TupleType::new(vec![TupleItemType::named("message", ScriptType::Str)]),
    );
    builder.add_record("ParseError", Arc::clone(&parse_error));

    builder.add_function("int::parse", ParseIntFunc::new(parse_error.clone()));
    builder.add_function("Range::parse", ParseRangeFunc::new(parse_error.clone()));

    builder.add_type_method("parse", ParseFunc::new(parse_error.clone()));
}

pub(crate) struct ParseFunc {
    parse_error: Arc<RecType>,
}

impl ParseFunc {
    pub(crate) fn new(parse_error: Arc<RecType>) -> Self {
        Self { parse_error }
    }
}

impl NativeTypeMethod for ParseFunc {
    fn arguments_type(&self, _: &TypeDefinition) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, typedef: &TypeDefinition, _: &TupleType) -> TypeResult<ScriptType> {
        let error_typ = ScriptType::RecInstance(Arc::clone(&self.parse_error));

        let value_typ = match typedef {
            TypeDefinition::RecDefinition(def) => ScriptType::RecInstance(Arc::clone(def)),
            TypeDefinition::UnionDefinition(def) => ScriptType::UnionInstance(Arc::clone(def)),
        };

        Ok(ScriptType::fallible_of(value_typ, error_typ))
    }

    fn call(
        &self,
        _: &Interpreter,
        typedef: &TypeDefinition,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let (input, content_type) = arguments.single()?.as_string_and_type()?;

        let parse_result = match content_type {
            ContentType::Undefined => parse_default(typedef, &input),

            #[cfg(feature = "json")]
            ContentType::Json => stdlib::json::parse_json(typedef, &input),

            #[allow(unreachable_patterns)]
            _ => Err(ScriptError::panic("Parser not found"))?,
        }
        .map_err(|err| err.as_script_error(&self.parse_error))?;

        Ok(ScriptValue::ok(parse_result))
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

    fn as_script_error(&self, def: &Arc<RecType>) -> ScriptError {
        let err = ScriptValue::Rec {
            def: Arc::clone(def),
            value: Arc::new(Tuple::new(vec![TupleItem::named(
                "message".into(),
                ScriptValue::string(self.msg.clone()),
            )])),
        };

        ScriptError::error(err)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

fn parse_default(typedef: &TypeDefinition, input: &str) -> Result<ScriptValue, ParseError> {
    match typedef {
        TypeDefinition::RecDefinition(def) => {
            let mut values = Vec::new();
            let mut tokens = input.split_ascii_whitespace();
            for d in def.params.items() {
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

            Ok(ScriptValue::Rec {
                def: Arc::clone(def),
                value: Arc::new(Tuple::new(values)),
            })
        }
        TypeDefinition::UnionDefinition(_) => Err(ParseError::new(
            "Default parser doesn't support union parsing",
        )),
    }
}

struct ParseIntFunc {
    parse_error: Arc<RecType>,
}

impl ParseIntFunc {
    fn new(parse_error: Arc<RecType>) -> Self {
        Self { parse_error }
    }
}

impl NativeFunction for ParseIntFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let mut args = arguments.iter_args();
        let input = args
            .next_positional()
            .ok_or_else(|| ScriptError::panic("Expected argument"))?
            .as_string()?;
        let base = args
            .get("base")
            .map(|a| a.as_int())
            .transpose()?
            .unwrap_or(10);
        let base: u32 = base.try_into().map_err(ScriptError::panic)?;

        let res = i64::from_str_radix(input.as_ref(), base)
            .map(ScriptValue::Int)
            .map_err(|err| ParseError::new(format!("{err}, input: '{input}'")))
            .map_err(|err| err.as_script_error(&self.parse_error))?;

        Ok(ScriptValue::ok(res))
    }

    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        // XXX Create a custom error type
        let error_typ = ScriptType::RecInstance(Arc::clone(&self.parse_error));
        let value_typ = ScriptType::Int;

        Ok(ScriptType::fallible_of(value_typ, error_typ))
    }
}

struct ParseRangeFunc {
    parse_error: Arc<RecType>,
}

impl ParseRangeFunc {
    fn new(parse_error: Arc<RecType>) -> Self {
        Self { parse_error }
    }
}

impl NativeFunction for ParseRangeFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let input = arguments.single()?.as_string()?;

        fn inner(input: &str) -> Result<ScriptValue, ParseError> {
            if let Some(n) = input.find('-') {
                let (l, r) = input.split_at(n);
                let l = l
                    .parse()
                    .map_err(|err: ParseIntError| ParseError::new(err.to_string()))?;
                let r = r[1..]
                    .parse()
                    .map_err(|err: ParseIntError| ParseError::new(err.to_string()))?;

                Ok(ScriptValue::Range(l, r))
            } else {
                Err(ParseError::new("Parse error"))
            }
        }

        let res = inner(input.as_ref()).map_err(|err| err.as_script_error(&self.parse_error))?;
        Ok(ScriptValue::ok(res))
    }

    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        let error_typ = ScriptType::RecInstance(Arc::clone(&self.parse_error));
        let value_typ = ScriptType::Range;

        Ok(ScriptType::fallible_of(value_typ, error_typ))
    }
}
