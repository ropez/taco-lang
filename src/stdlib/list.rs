use std::sync::Arc;

use crate::{
    Builder,
    error::{ScriptError, ScriptErrorKind, TypeError, TypeErrorKind},
    ident::global,
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    stdlib::{NativeFunction, NativeMethod},
    validate::{ScriptType, TupleItemType, TupleType},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_method(global::LIST, "push", ListPush);
    builder.add_method(global::LIST, "find", ListFind);
    builder.add_method(global::LIST, "count", ListCount);
    builder.add_method(global::LIST, "unzip", ListUnzip);
    builder.add_method(global::LIST, "sum", ListSum);
    builder.add_method(global::LIST, "sort", ListSort);
    builder.add_method(global::LIST, "map", ListMap);
    builder.add_method(global::LIST, "map_to", ListMapTo);
    builder.add_method(global::LIST, "filter", ListFilter);
    builder.add_method(global::LIST, "flatten", ListFlatten);

    builder.add_method(global::RANGE, "map", ListMap);
    builder.add_method(global::RANGE, "filter", ListFilter);
}

#[derive(Debug, Clone)]
pub struct List(Vec<ScriptValue>);

impl List {
    pub fn new(items: Vec<ScriptValue>) -> Self {
        Self(items)
    }

    pub fn items(&self) -> &[ScriptValue] {
        &self.0
    }

    pub fn push(&mut self, item: ScriptValue) {
        self.0.push(item);
    }
}

impl From<List> for ScriptValue {
    fn from(value: List) -> Self {
        ScriptValue::List(Arc::new(value))
    }
}

pub(crate) struct ListPush;
impl ListMethod for ListPush {
    fn list_call(
        &self,
        _: &Interpreter,
        mut subject: Arc<List>,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single()?;
        Arc::make_mut(&mut subject).push(arg.clone());
        Ok(ScriptValue::List(subject))
    }

    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        // XXX Variadic args not supported (but allowed in call)
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(inner.clone()))
    }
}

trait ListMethod {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        let _ = inner;
        Ok(TupleType::identity())
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        let _ = inner;
        Ok(ScriptType::identity())
    }

    fn empty_list_arguments_type(&self) -> Result<TupleType, TypeError> {
        self.list_arguments_type(&ScriptType::Infer(1))
    }

    fn empty_list_return_type(&self) -> Result<ScriptType, TypeError> {
        self.list_return_type(&ScriptType::Infer(1))
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError>;
}

pub(crate) struct ListFind;
impl ListMethod for ListFind {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Opt(Box::new(inner.clone())))
    }

    fn list_call(
        &self,
        _: &Interpreter,
        list: Arc<List>,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let val = arguments.single()?;
        let opt_val = list.items().iter().find(|v| ScriptValue::eq(v, val));
        Ok(opt_val.cloned().unwrap_or(ScriptValue::None))
    }
}

pub(crate) struct ListCount;
impl ListMethod for ListCount {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Int)
    }

    fn list_call(
        &self,
        _: &Interpreter,
        list: Arc<List>,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let val = arguments.single()?;
        let count = list
            .items()
            .iter()
            .filter(|v| ScriptValue::eq(v, val))
            .count();
        Ok(ScriptValue::Int(count.try_into().unwrap()))
    }
}

pub(crate) struct ListUnzip;
impl ListMethod for ListUnzip {
    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        // TODO These methods should create "stream" or "iterator" instead of just copying the whole list up-front

        let tuples: Vec<_> = subject
            .items()
            .iter()
            .map(|arg| arg.as_tuple().expect("list containing tuples"))
            .collect();

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

        Ok(ScriptValue::Tuple(Arc::new(tuple)))
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        let Some(tuple_typ) = inner.as_tuple() else {
            return Err(TypeError::new(TypeErrorKind::InvalidMapTo(inner.clone())));
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
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
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

        Ok(ScriptValue::List(Arc::new(List::new(values))))
    }

    // XXX Supporting exactly two arguments only

    fn arguments_type(&self) -> TupleType {
        let args = vec![
            TupleItemType::unnamed(ScriptType::list_of(ScriptType::Infer(1))),
            TupleItemType::unnamed(ScriptType::list_of(ScriptType::Infer(2))),
        ];
        TupleType::from(args)
    }

    fn return_type(&self) -> ScriptType {
        let args = vec![
            TupleItemType::unnamed(ScriptType::Infer(1)),
            TupleItemType::unnamed(ScriptType::Infer(2)),
        ];
        ScriptType::list_of(ScriptType::Tuple(TupleType::from(args)))
    }
}

pub(crate) struct ListSum;
impl ListMethod for ListSum {
    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        if subject.items().iter().any(|v| v.is_nan()) {
            Ok(ScriptValue::NaN)
        } else {
            let numbers = subject
                .items()
                .iter()
                .map(|val| val.as_int())
                .collect::<Result<Vec<_>, ScriptError>>()?;
            Ok(ScriptValue::Int(
                numbers.into_iter().reduce(|n, a| n + a).unwrap_or_default(),
            ))
        }
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        match inner {
            ScriptType::Int => Ok(ScriptType::Int),
            _ => Err(TypeError::new(TypeErrorKind::InvalidExpression)), // XXX Better type
        }
    }
}

pub(crate) struct ListSort;
impl ListMethod for ListSort {
    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let mut values: Vec<_> = subject
            .items()
            .iter()
            .map(|val| val.as_int())
            .collect::<Result<Vec<_>, ScriptError>>()?;

        values.sort();

        let values = values.into_iter().map(ScriptValue::Int).collect();

        Ok(List::new(values).into())
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::List(Box::new(inner.clone())))
    }
}

pub(crate) struct ListMap;
impl ListMethod for ListMap {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        Ok(TupleType::from_single(ScriptType::Function {
            params: TupleType::from_single(inner.clone()),
            ret: ScriptType::Infer(1).into(),
        }))
    }

    fn list_return_type(&self, _inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(ScriptType::Infer(1)))
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let callable = arguments.single()?; // XXX Maybe we can have owned args here
        let mut mapped = Vec::new();
        for item in subject.items() {
            let value = interpreter.eval_callable(callable.clone(), &item.to_single_argument())?;
            mapped.push(value);
        }
        Ok(ScriptValue::List(Arc::new(List::new(mapped))))
    }
}

pub(crate) struct ListFilter;
impl ListMethod for ListFilter {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        Ok(TupleType::from_single(ScriptType::Function {
            params: TupleType::from_single(inner.clone()),
            ret: ScriptType::Bool.into(),
        }))
    }

    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(inner.clone()))
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let callable = arguments.single()?;
        let mut mapped = Vec::new();
        for item in subject.items() {
            let value = interpreter.eval_callable(callable.clone(), &item.to_single_argument())?;
            if let ScriptValue::Boolean(v) = value
                && v
            {
                mapped.push(item.clone());
            }
        }
        Ok(ScriptValue::List(Arc::new(List::new(mapped))))
    }
}

pub(crate) struct ListFlatten;
impl ListMethod for ListFlatten {
    fn list_return_type(&self, inner: &ScriptType) -> Result<ScriptType, TypeError> {
        // XXX Need something like inner.as_iterable()
        let res = match inner {
            ScriptType::List(i) => Ok(ScriptType::clone(i)),
            ScriptType::Range => Ok(ScriptType::Int),
            _ => Err(TypeError::new(TypeErrorKind::InvalidArgument {
                expected: "list of lists".into(),
                actual: inner.clone(),
            })),
        };
        res.map(ScriptType::list_of)
    }

    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let mut items = Vec::new();

        for inner in subject.items() {
            items.extend(inner.as_iterable().iter().map(ScriptValue::clone));
        }

        let list = List::new(items);
        Ok(ScriptValue::List(list.into()))
    }
}

pub(crate) struct ListMapTo;
impl ListMethod for ListMapTo {
    fn list_arguments_type(&self, inner: &ScriptType) -> Result<TupleType, TypeError> {
        let Some(tuple_typ) = inner.as_tuple() else {
            return Err(TypeError::new(TypeErrorKind::InvalidMapTo(inner.clone())));
        };
        Ok(TupleType::from_single(ScriptType::Function {
            params: tuple_typ.clone(),
            ret: ScriptType::Infer(1).into(),
        }))
    }

    fn list_return_type(&self, _inner: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(ScriptType::Infer(1)))
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let callable = arguments.single()?;
        let mut mapped = Vec::new();
        for item in subject.items() {
            let tuple = item.as_tuple().expect("list of tuples");
            let value = interpreter.eval_callable(callable.clone(), &tuple)?;
            mapped.push(value);
        }
        Ok(ScriptValue::List(Arc::new(List::new(mapped))))
    }
}

impl<T> NativeMethod for T
where
    T: ListMethod,
{
    fn call(
        &self,
        interpreter: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        if let ScriptValue::List(list) = subject {
            self.list_call(interpreter, list, arguments)
        } else if let ScriptValue::Range(l, r) = subject {
            // XXX list_call should have some kind of iterator
            let list = Arc::new(List::new((l..=r).map(ScriptValue::Int).collect()));
            self.list_call(interpreter, list, arguments)
        } else {
            Err(ScriptError::panic("Not a list"))
        }
    }

    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        match subject {
            ScriptType::EmptyList => self.empty_list_arguments_type(),
            ScriptType::List(inner) => self.list_arguments_type(inner),
            ScriptType::Range => self.list_arguments_type(&ScriptType::Int),
            _ => Err(TypeError::new(TypeErrorKind::InvalidIterable(
                subject.clone(),
            ))),
        }
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        match subject {
            ScriptType::EmptyList => self.empty_list_return_type(),
            ScriptType::List(inner) => self.list_return_type(inner),
            ScriptType::Range => self.list_return_type(&ScriptType::Int),
            _ => Err(TypeError::new(TypeErrorKind::InvalidIterable(
                subject.clone(),
            ))),
        }
    }
}
