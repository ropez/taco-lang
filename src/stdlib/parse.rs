use std::sync::Arc;

use crate::{
    Builder,
    error::ScriptError,
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    parser::{Record, TypeExpression},
    stdlib::NativeFunction,
    validate::{ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
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
        if let Some(ScriptValue::String(s)) = arguments.first() {
            let mut values = Vec::new();
            let mut tokens = s.split_ascii_whitespace();
            for d in self.def.params.as_ref() {
                values.push(TupleItem::new(
                    d.name.clone(),
                    match d.type_expr.as_ref() {
                        TypeExpression::Int => {
                            ScriptValue::Int(tokens.next().unwrap().parse::<i64>().unwrap())
                        }
                        TypeExpression::Str => ScriptValue::String(tokens.next().unwrap().into()),
                        o => todo!("Don't know how to parse {o:?}"),
                    },
                ));
            }

            Ok(ScriptValue::Rec {
                def: Arc::clone(&self.def),
                value: Arc::new(Tuple::new(values)),
            })
        } else {
            Err(ScriptError::panic("Expected string, got {arguments:?}"))
        }
    }
}

struct ParseRangeFunc;
impl NativeFunction for ParseRangeFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let Some(ScriptValue::String(s)) = arguments.first() else {
            return Err(ScriptError::panic("Not a string"));
        };
        if let Some(n) = s.find('-') {
            let (l, r) = s.split_at(n);
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

    fn return_type(&self) -> ScriptType {
        ScriptType::Range
    }
}
