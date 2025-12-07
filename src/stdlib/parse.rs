use std::sync::Arc;

use crate::{
    Builder,
    error::{ScriptError, ScriptErrorKind},
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    parser::{Record, TypeExpression},
    stdlib::NativeFunction,
    validate::{ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
    // XXX Should be range::parse
    builder.add_function("range__parse", ParseRangeFunc);
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
            for d in &self.def.params {
                values.push(TupleItem::new(
                    d.name.clone(),
                    match d.type_expr.as_ref() {
                        TypeExpression::Scalar(ident) => match ident.as_str() {
                            "str" => ScriptValue::String(tokens.next().unwrap().into()),
                            "int" => {
                                ScriptValue::Int(tokens.next().unwrap().parse::<i64>().unwrap())
                            }
                            o => todo!("Don't know how to parse {o:?}"),
                        },
                        o => todo!("Don't know how to parse {o:?}"),
                    },
                ));
            }

            Ok(ScriptValue::Rec {
                def: Arc::clone(&self.def),
                value: Arc::new(Tuple::new(values)),
            })
        } else {
            panic!("Expected string, got {arguments:?}");
        }
    }
}

struct ParseRangeFunc;
impl NativeFunction for ParseRangeFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let Some(ScriptValue::String(s)) = arguments.first() else {
            panic!("Expected string, got {arguments:?}");
        };
        if let Some(n) = s.find('-') {
            let (l, r) = s.split_at(n);
            let l = l
                .parse()
                .map_err(|_| ScriptError::new(ScriptErrorKind::unknown("Parse error")))?;
            let r = r[1..]
                .parse()
                .map_err(|_| ScriptError::new(ScriptErrorKind::unknown("Parse error")))?;

            Ok(ScriptValue::Range(l, r))
        } else {
            panic!("Parse error");
        }
    }

    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn return_type(&self) -> ScriptType {
        ScriptType::Range
    }
}
