use std::sync::Arc;

use crate::{
    error::TypeError,
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    stdlib::{NativeFunction, NativeMethod, NativeMethodType},
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
}

impl ListMethodType for ListPush {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        // XXX Variadic args not supported (but allowed in call)
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(inner.clone()))
    }
}

trait ListMethodType {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        let _ = inner;
        Ok(TupleType::identity())
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        let _ = inner;
        Ok(ScriptType::identity())
    }

    fn empty_list_arguments_type(&self) -> Result<TupleType, TypeError> {
        self.list_arguments_type(&ScriptType::Generic)
    }

    fn empty_list_return_type(&self) -> Result<ScriptType, TypeError> {
        self.list_return_type(&ScriptType::Generic)
    }
}

trait ListMethod {
    fn list_call(&self, subject: &List, arguments: &Tuple) -> ScriptValue;
}

pub(crate) struct ListFind;
impl ListMethodType for ListFind {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Opt(Box::new(inner.clone())))
    }
}

impl ListMethod for ListFind {
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
}

impl ListMethodType for ListUnzip {
    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        let Some(tuple_typ) = inner.as_tuple() else {
            return Err(TypeError::InvalidMapTo(inner.clone()));
        };

        let types: Vec<_> = tuple_typ
            .items()
            .iter()
            .map(|t| TupleItemType::unnamed(ScriptType::list_of(t.value.clone())))
            .collect();

        Ok(ScriptType::Tuple(TupleType::from(types)))
    }
}

pub(crate) struct ListZip;
impl NativeFunction for ListZip {
    fn call(&self, arguments: &Tuple) -> ScriptValue {
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

impl ListMethodType for ListSum {
    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        match inner {
            ScriptType::Int => Ok(ScriptType::Int),
            _ => Err(TypeError::InvalidExpression), // XXX Better type
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
}

impl ListMethodType for ListSort {
    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::List(Box::new(inner.clone())))
    }
}

pub(crate) struct ListMapType;
impl ListMethodType for ListMapType {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        Ok(TupleType::from_single(ScriptType::Function {
            params: TupleType::from_single(inner.clone()),
            ret: ScriptType::Generic.into(),
        }))
    }

    fn list_return_type(&self, _inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(ScriptType::Generic))
    }
}

// XXX Requires splitting into separate traits for validation ond evaluation (or more Arc and Mutex)
pub(crate) struct ListMap<'a>(pub(crate) &'a Interpreter);
impl ListMethod for ListMap<'_> {
    fn list_call(&self, subject: &List, arguments: &Tuple) -> ScriptValue {
        let callable = arguments.single();
        let mut mapped = Vec::new();
        for item in subject.items() {
            let value = self.0.eval_callable(callable, &item.to_single_argument());
            mapped.push(value);
        }
        ScriptValue::List(Arc::new(List::new(mapped)))
    }
}

pub(crate) struct ListMapToType;
impl ListMethodType for ListMapToType {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        let Some(tuple_typ) = inner.as_tuple() else {
            return Err(TypeError::InvalidMapTo(inner.clone()));
        };
        Ok(TupleType::from_single(ScriptType::Function {
            params: tuple_typ.clone(),
            ret: ScriptType::Generic.into(),
        }))
    }

    fn list_return_type(&self, _inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(ScriptType::Generic))
    }
}

pub(crate) struct ListMapTo<'a>(pub(crate) &'a Interpreter);
impl ListMethod for ListMapTo<'_> {
    fn list_call(&self, subject: &List, arguments: &Tuple) -> ScriptValue {
        let callable = arguments.single();
        let mut mapped = Vec::new();
        for item in subject.items() {
            let value = self.0.eval_callable(callable, &item.as_tuple());
            mapped.push(value);
        }
        ScriptValue::List(Arc::new(List::new(mapped)))
    }
}

impl<T> NativeMethod for T
where
    T: ListMethod,
{
    fn call(&self, subject: &ScriptValue, arguments: &Tuple) -> ScriptValue {
        if let ScriptValue::List(list) = subject {
            self.list_call(list, arguments)
        } else {
            panic!("Not a list")
        }
    }
}

impl<T> NativeMethodType for T
where
    T: ListMethodType,
{
    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        match subject {
            ScriptType::EmptyList => self.empty_list_arguments_type(),
            ScriptType::List(inner) => self.list_arguments_type(inner),
            _ => panic!("Not a list"),
        }
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        match subject {
            ScriptType::EmptyList => self.empty_list_return_type(),
            ScriptType::List(inner) => self.list_return_type(inner),
            _ => panic!("Not a list"),
        }
    }
}
