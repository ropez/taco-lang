use std::{fmt, num::ParseIntError, sync::Arc};

use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeResult},
    ext::NativeFunction,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ContentType, ScriptValue, Tuple, TupleItem},
    stdlib,
    type_scope::TypeDefinition,
};

pub fn build(builder: &mut Builder) {
    builder.add_function("int::parse", ParseIntFunc);
    builder.add_function("Range::parse", ParseRangeFunc);
}

pub(crate) struct ParseFunc {
    typedef: TypeDefinition,
}

impl ParseFunc {
    pub(crate) fn new(typedef: TypeDefinition) -> Self {
        Self { typedef }
    }
}

impl NativeFunction for ParseFunc {
    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        // XXX How to programmatically create a custom error type, or include Taco snippets in stdlib?
        let error_typ = ScriptType::Str;

        let value_typ = match &self.typedef {
            TypeDefinition::RecDefinition(def) => ScriptType::RecInstance(Arc::clone(def)),
            TypeDefinition::UnionDefinition(def) => ScriptType::UnionInstance(Arc::clone(def)),
        };

        Ok(ScriptType::fallible_of(value_typ, error_typ))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let (input, content_type) = arguments.single()?.as_string_and_type()?;

        let parse_result = match content_type {
            ContentType::Undefined => parse_default(&self.typedef, &input),

            #[cfg(feature = "json")]
            ContentType::Json => stdlib::json::parse_json(&self.typedef, &input),

            #[allow(unreachable_patterns)]
            _ => Err(ScriptError::panic("Parser not found"))?,
        }
        .map_err(|err| ScriptValue::string(err.msg));

        Ok(parse_result.into())
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

struct ParseIntFunc;
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
            .map_err(|err| ScriptValue::string(format!("{err}, input: '{input}'")));

        Ok(res.into())
    }

    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        // XXX Create a custom error type
        let error_typ = ScriptType::Str;
        let value_typ = ScriptType::Int;

        Ok(ScriptType::fallible_of(value_typ, error_typ))
    }
}

struct ParseRangeFunc;
impl NativeFunction for ParseRangeFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let input = arguments.single()?.as_string()?;

        fn inner(input: &str) -> Result<ScriptValue, ScriptValue> {
            if let Some(n) = input.find('-') {
                let (l, r) = input.split_at(n);
                let l = l
                    .parse()
                    .map_err(|err: ParseIntError| ScriptValue::string(err.to_string()))?;
                let r = r[1..]
                    .parse()
                    .map_err(|err: ParseIntError| ScriptValue::string(err.to_string()))?;

                Ok(ScriptValue::Range(l, r))
            } else {
                Err(ScriptValue::string("Parse error"))
            }
        }

        Ok(inner(input.as_ref()).into())
    }

    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        // XXX Create a custom error type
        let error_typ = ScriptType::Str;
        let value_typ = ScriptType::Range;

        Ok(ScriptType::fallible_of(value_typ, error_typ))
    }
}
