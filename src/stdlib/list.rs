use std::sync::Arc;

use crate::{
    error::TypeError,
    interpreter::{ScriptValue, Tuple, TupleItem},
    stdlib::NativeMethod,
    validate::{ScriptType, TupleItemType, TupleType},
};

#[derive(Debug, Clone)]
pub struct List(Vec<ScriptValue>);

impl List {
    pub fn new(items: Vec<ScriptValue>) -> Self {
        Self(items)
    }

    pub fn items(&self) -> &[ScriptValue] {
        &self.0
    }
}

impl From<List> for ScriptValue {
    fn from(value: List) -> Self {
        ScriptValue::List(Arc::new(value))
    }
}

pub(crate) struct ListPush;
impl ListMethod for ListPush {
    fn list_call(&self, subject: &List, arguments: &Tuple) -> ScriptValue {
        // This is the Copy on Write feature of the language in play.
        // Can we avoid copy, if we see that the original will not be used again?
        let mut res = Vec::clone(&subject.0);
        for arg in arguments.items() {
            res.push(arg.value.clone());
        }
        List::new(res).into()
    }

    // XXX To handle the EmptyList case, this needs to override NativeMethod directly

    fn list_arguments_type(&self, inner: &ScriptType) -> TupleType {
        // XXX Variadic args not supported (but allowed in call)
        TupleType::from_single(inner.clone())
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::List(Box::new(inner.clone())))
    }
}

trait ListMethod: NativeMethod {
    fn list_arguments_type(&self, inner: &ScriptType) -> TupleType {
        let _ = inner;
        TupleType::identity()
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        let _ = inner;
        Ok(ScriptType::identity())
    }

    fn list_call(&self, subject: &List, arguments: &Tuple) -> ScriptValue;
}

pub(crate) struct ListFind;
impl ListMethod for ListFind {
    fn list_arguments_type(&self, inner: &ScriptType) -> TupleType {
        TupleType::from_single(inner.clone())
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Opt(Box::new(inner.clone())))
    }

    fn list_call(&self, list: &List, arguments: &Tuple) -> ScriptValue {
        let val = arguments.single();
        let opt_val = list.items().iter().find(|v| ScriptValue::eq(v, val));
        opt_val.cloned().unwrap_or(ScriptValue::None)
    }
}

pub(crate) struct ListUnzip;
impl ListMethod for ListUnzip {
    fn list_call(&self, subject: &List, _arguments: &Tuple) -> ScriptValue {
        // TODO These methods should create "stream" or "iterator" instead of just copying the whole list up-front

        let tuples: Vec<_> = subject.0.iter().map(|arg| arg.as_tuple()).collect();

        let mut lists = Vec::new();

        let tuple = if let Some(first) = tuples.first() {
            for it in first.items() {
                lists.push(vec![it.value.clone()]);
            }

            for tuple in &tuples[1..] {
                for (i, it) in tuple.items().iter().enumerate() {
                    lists[i].push(it.value.clone());
                }
            }

            let items = lists
                .into_iter()
                .map(|l| TupleItem::unnamed(List::new(l)))
                .collect();
            Tuple::new(items)
        } else {
            Tuple::identity()
        };

        ScriptValue::Tuple(Arc::new(tuple))
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        if let Some(tuple) = inner.as_tuple() {
            let types: Vec<_> = tuple
                .items()
                .iter()
                .map(|t| TupleItemType::unnamed(ScriptType::List(Box::new(t.value.clone()))))
                .collect();

            Ok(ScriptType::Tuple(TupleType::from(types)))
        } else {
            panic!("Expected list of tuples, found {inner}");
            // XXX Error handling
            // Err(self.fail(
            //     format!("Expected list of tuples, found {typ}"),
            //     expr.loc,
            // ))
        }
    }
}

pub(crate) fn list_zip(arguments: &Tuple) -> ScriptValue {
    let lists: Vec<_> = arguments
        .items()
        .iter()
        .map(|arg| arg.value.as_iterable())
        .collect();

    let mut tuples = Vec::new();
    let len = lists.iter().map(|l| l.len()).min().unwrap_or_default();
    for i in 0..len {
        let mut items = Vec::new();

        for l in &lists {
            items.push(TupleItem::unnamed(l[i].clone()));
        }

        tuples.push(Tuple::new(items));
    }

    let values = tuples
        .into_iter()
        .map(Arc::new)
        .map(ScriptValue::Tuple)
        .collect();

    ScriptValue::List(Arc::new(List::new(values)))
}

pub(crate) struct ListSum;
impl ListMethod for ListSum {
    fn list_call(&self, subject: &List, _arguments: &Tuple) -> ScriptValue {
        if subject.items().iter().any(|v| v.is_nan()) {
            ScriptValue::NaN
        } else {
            ScriptValue::Number(
                subject
                    .items()
                    .iter()
                    .map(|val| val.as_number())
                    .reduce(|n, a| n + a)
                    .unwrap_or_default(),
            )
        }
    }
}

pub(crate) struct ListSort;
impl ListMethod for ListSort {
    fn list_call(&self, subject: &List, _arguments: &Tuple) -> ScriptValue {
        let mut values: Vec<_> = subject.items().iter().map(|val| val.as_number()).collect();

        values.sort();

        let values = values.into_iter().map(ScriptValue::Number).collect();

        List::new(values).into()
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::List(Box::new(inner.clone())))
    }
}

impl<T> NativeMethod for T
where
    T: ListMethod,
{
    fn arguments_type(&self, subject: &ScriptType) -> TupleType {
        if let ScriptType::List(inner) = subject {
            self.list_arguments_type(inner)
        } else {
            panic!("Not a list")
        }
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        if let ScriptType::List(inner) = subject {
            self.list_return_type(inner)
        } else {
            panic!("Not a list")
        }
    }

    fn call(&self, subject: &ScriptValue, arguments: &Tuple) -> ScriptValue {
        if let ScriptValue::List(list) = subject {
            self.list_call(list, arguments)
        } else {
            panic!("Not a list")
        }
    }
}
