use std::sync::Arc;

use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeError, TypeErrorKind, TypeResult},
    ext::{NativeFunction, NativeMethod},
    ident::global,
    interpreter::Interpreter,
    script_type::{FunctionType, ScriptType, TupleItemType, TupleType},
    script_value::{ScriptValue, Tuple, TupleItem},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_method(global::LIST, "len", ListLen);
    builder.add_method(global::LIST, "push", ListPush);
    builder.add_method(global::LIST, "at", ListAt);
    builder.add_method(global::LIST, "skip", ListSkip);
    builder.add_method(global::LIST, "take", ListTake);
    builder.add_method(global::LIST, "find", ListFind);
    builder.add_method(global::LIST, "find_index", ListFindIndex);
    builder.add_method(global::LIST, "contains", ListContains);
    builder.add_method(global::LIST, "count", ListCount);
    builder.add_method(global::LIST, "unzip", ListUnzip);
    builder.add_method(global::LIST, "sum", ListSum);
    builder.add_method(global::LIST, "max", ListMax);
    builder.add_method(global::LIST, "sort", ListSort);
    builder.add_method(global::LIST, "map", ListMap);
    builder.add_method(global::LIST, "map_to", ListMapTo);
    builder.add_method(global::LIST, "any", ListAny);
    builder.add_method(global::LIST, "scan", ListScan);
    builder.add_method(global::LIST, "filter", ListFilter);
    builder.add_method(global::LIST, "flatten", ListFlatten);
    builder.add_method(global::LIST, "enumerate", ListEnumerate);
    builder.add_method(global::LIST, "flip", ListFlip);

    builder.add_method(global::RANGE, "contains", ListContains); // XXX Ineffective for Range
    builder.add_method(global::RANGE, "map", ListMap);
    builder.add_method(global::RANGE, "filter", ListFilter);
    builder.add_method(global::RANGE, "to_list", ToList);

    builder.add_function("List::zip", ListZip);
    builder.add_function("List::cartesian", ListCartesian);
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

    pub fn len(&self) -> usize {
        self.0.len()
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

pub(crate) struct ListLen;
impl ListMethod for ListLen {
    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        Ok(ScriptValue::Int(subject.len() as i64))
    }

    fn list_return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }
}

pub(crate) struct ListPush;
impl ListMethod for ListPush {
    fn list_call(
        &self,
        _: &Interpreter,
        mut subject: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let arg = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;
        Arc::make_mut(&mut subject).push(arg.clone());
        Ok(ScriptValue::List(subject))
    }

    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        // XXX Variadic args not supported (but allowed in call)
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::list_of(inner.clone()))
    }

    fn empty_list_return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        let arg = arguments.single()?;
        Ok(ScriptType::list_of(arg.clone()))
    }
}

trait ListMethod {
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        let _ = inner;
        Ok(TupleType::identity())
    }

    fn list_return_type(
        &self,
        inner: &ScriptType,
        arguments: &TupleType,
    ) -> TypeResult<ScriptType> {
        let _ = inner;
        let _ = arguments;
        Ok(ScriptType::identity())
    }

    fn empty_list_arguments_type(&self) -> TypeResult<TupleType> {
        self.list_arguments_type(&ScriptType::Infer(1))
    }

    fn empty_list_return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        self.list_return_type(&ScriptType::Infer(1), arguments)
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue>;
}

pub(crate) struct ListAt;
impl ListMethod for ListAt {
    fn list_arguments_type(&self, _inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Int))
    }

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::opt_of(inner.clone()))
    }

    fn list_call(
        &self,
        _: &Interpreter,
        list: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let val = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?
            .as_int()?;
        let opt_val = list.items().get(val as usize);
        Ok(ScriptValue::opt(opt_val.cloned()))
    }
}

pub(crate) struct ListSkip;
impl ListMethod for ListSkip {
    fn list_arguments_type(&self, _inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Int))
    }

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::list_of(inner.clone()))
    }

    fn list_call(
        &self,
        _: &Interpreter,
        list: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let n = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?
            .as_int()?;
        let items = list.items().iter().skip(n as usize).cloned().collect();
        Ok(ScriptValue::from(List::new(items)))
    }
}

pub(crate) struct ListTake;
impl ListMethod for ListTake {
    fn list_arguments_type(&self, _inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Int))
    }

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::list_of(inner.clone()))
    }

    fn list_call(
        &self,
        _: &Interpreter,
        list: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let n = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?
            .as_int()?;
        let items = list.items().iter().take(n as usize).cloned().collect();
        Ok(ScriptValue::from(List::new(items)))
    }
}

pub(crate) struct ListFind;
impl ListMethod for ListFind {
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Opt(Box::new(inner.clone())))
    }

    fn list_call(
        &self,
        _: &Interpreter,
        list: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let val = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;
        let opt_val = list.items().iter().find(|v| ScriptValue::eq(v, &val));
        Ok(ScriptValue::opt(opt_val.cloned()))
    }
}

pub(crate) struct ListContains;
impl ListMethod for ListContains {
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Bool)
    }

    fn list_call(
        &self,
        _: &Interpreter,
        list: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let val = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;
        let val = list.items().iter().any(|v| ScriptValue::eq(v, &val));
        Ok(ScriptValue::Boolean(val))
    }
}

pub(crate) struct ListFindIndex;
impl ListMethod for ListFindIndex {
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, _inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Opt(ScriptType::Int.into()))
    }

    fn list_call(
        &self,
        _: &Interpreter,
        list: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let val = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;
        let opt_idx = list
            .items()
            .iter()
            .position(|v| ScriptValue::eq(v, &val))
            .map(|i| ScriptValue::Int(i as i64));
        Ok(ScriptValue::opt(opt_idx))
    }
}

pub(crate) struct ListCount;
impl ListMethod for ListCount {
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(inner.clone()))
    }

    fn list_return_type(&self, _inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }

    fn list_call(
        &self,
        _: &Interpreter,
        list: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let val = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;
        let count = list
            .items()
            .iter()
            .filter(|v| ScriptValue::eq(v, &val))
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
    ) -> ScriptResult<ScriptValue> {
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

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        let Some(tuple_typ) = inner.as_tuple() else {
            return Err(TypeError::new(TypeErrorKind::InvalidMapTo(inner.clone())));
        };

        let types: Vec<_> = tuple_typ
            .items()
            .iter()
            .map(|t| TupleItemType::unnamed(ScriptType::list_of(t.value.clone())))
            .collect();

        Ok(ScriptType::Tuple(TupleType::new(types)))
    }
}

pub(crate) struct ListZip;
impl NativeFunction for ListZip {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
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

    // XXX Validation is kind-of duplicated in arguments_type and return_type

    fn arguments_type(&self, arguments: &TupleType) -> TypeResult<TupleType> {
        for args in arguments.positional() {
            inner_type(args)?; // Validate list type
        }
        Ok(arguments.clone())
    }

    fn return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        let args = arguments
            .positional()
            .map(|arg| Ok(TupleItemType::unnamed(inner_type(arg)?)))
            .collect::<TypeResult<Vec<_>>>()?;
        Ok(ScriptType::list_of(ScriptType::Tuple(TupleType::new(args))))
    }
}

pub(crate) struct ListCartesian;
impl NativeFunction for ListCartesian {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let mut tuples = vec![Tuple::identity()];

        for src in arguments.positional() {
            let mut product = Vec::new();
            for t in &tuples {
                for val in src.as_iterable() {
                    // XXX Simpler way to get as Vec?
                    let mut items: Vec<_> = t.items().iter().map(TupleItem::clone).collect();
                    items.push(TupleItem::unnamed(val.clone()));
                    product.push(Tuple::new(items));
                }
            }

            tuples = product;
        }

        let values = tuples
            .into_iter()
            .map(Arc::new)
            .map(ScriptValue::Tuple)
            .collect();

        Ok(ScriptValue::List(Arc::new(List::new(values))))
    }

    fn arguments_type(&self, arguments: &TupleType) -> TypeResult<TupleType> {
        for args in arguments.positional() {
            inner_type(args)?; // Validate list type
        }
        Ok(arguments.clone())
    }

    fn return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        let args = arguments
            .positional()
            .map(|arg| Ok(TupleItemType::unnamed(inner_type(arg)?)))
            .collect::<TypeResult<Vec<_>>>()?;
        Ok(ScriptType::list_of(ScriptType::Tuple(TupleType::new(args))))
    }
}

pub(crate) struct ListSum;
impl ListMethod for ListSum {
    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        if subject.items().iter().any(|v| v.is_nan()) {
            Ok(ScriptValue::NaN)
        } else {
            let numbers = subject
                .items()
                .iter()
                .map(|val| val.as_int())
                .collect::<ScriptResult<Vec<_>>>()?;

            let val = numbers
                .into_iter()
                .reduce(|n, a| n + a)
                .map(ScriptValue::Int);
            Ok(ScriptValue::opt(val))
        }
    }

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        match inner {
            ScriptType::Int => Ok(ScriptType::opt_of(ScriptType::Int)),
            _ => Err(TypeError::expected_type(
                ScriptType::list_of(ScriptType::Int),
                ScriptType::list_of(inner.clone()),
            )),
        }
    }

    fn empty_list_return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::opt_of(ScriptType::Int))
    }
}

pub(crate) struct ListMax;
impl ListMethod for ListMax {
    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        if subject.items().iter().any(|v| v.is_nan()) {
            Ok(ScriptValue::NaN)
        } else {
            let numbers = subject
                .items()
                .iter()
                .map(|val| val.as_int())
                .collect::<ScriptResult<Vec<_>>>()?;

            let val = numbers.into_iter().max().map(ScriptValue::Int);
            Ok(ScriptValue::opt(val))
        }
    }

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        match inner {
            ScriptType::Int => Ok(ScriptType::opt_of(ScriptType::Int)),
            _ => Err(TypeError::expected_type(
                ScriptType::list_of(ScriptType::Int),
                ScriptType::list_of(inner.clone()),
            )),
        }
    }

    fn empty_list_return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::opt_of(ScriptType::Int))
    }
}

pub(crate) struct ListSort;
impl ListMethod for ListSort {
    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let mut values: Vec<_> = subject
            .items()
            .iter()
            .map(|val| val.as_int())
            .collect::<ScriptResult<Vec<_>>>()?;

        values.sort();

        let values = values.into_iter().map(ScriptValue::Int).collect();

        Ok(List::new(values).into())
    }

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::List(Box::new(inner.clone())))
    }
}

pub(crate) struct ToList;
impl ListMethod for ToList {
    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::list_of(inner.clone()))
    }

    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        Ok(ScriptValue::List(subject))
    }
}

pub(crate) struct ListMap;
impl ListMethod for ListMap {
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Function(
            FunctionType::new(TupleType::from_single(inner.clone()), ScriptType::Infer(1)),
        )))
    }

    fn list_return_type(
        &self,
        _inner: &ScriptType,
        arguments: &TupleType,
    ) -> TypeResult<ScriptType> {
        let arg = arguments.single()?;
        let ret = arg.as_callable_ret(arguments)?;
        Ok(ScriptType::list_of(ret))
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let callable = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;
        let mut mapped = Vec::new();
        for item in subject.items() {
            let value = interpreter.eval_callable(callable.clone(), &item.to_single_argument())?;
            mapped.push(value);
        }
        Ok(ScriptValue::List(Arc::new(List::new(mapped))))
    }
}

pub(crate) struct ListAny;
impl ListMethod for ListAny {
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Function(
            FunctionType::new(TupleType::from_single(inner.clone()), ScriptType::Bool),
        )))
    }

    fn list_return_type(
        &self,
        _inner: &ScriptType,
        _arguments: &TupleType,
    ) -> TypeResult<ScriptType> {
        Ok(ScriptType::Bool)
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let callable = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;
        for item in subject.items() {
            let value = interpreter.eval_callable(callable.clone(), &item.to_single_argument())?;
            if value.as_boolean()? {
                return Ok(ScriptValue::Boolean(true));
            }
        }
        Ok(ScriptValue::Boolean(false))
    }
}

// 'scan' is similar to 'map', but the callback receives the previous mapped value together
// with the current item, and we must provide an initial value for the first iteration.
pub(crate) struct ListScan;
impl ListMethod for ListScan {
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::new(vec![
            TupleItemType::named("initial", ScriptType::Infer(1)),
            TupleItemType::named(
                "mapper",
                ScriptType::Function(FunctionType::new(
                    TupleType::new(vec![
                        TupleItemType::unnamed(ScriptType::Infer(1)),
                        TupleItemType::unnamed(inner.clone()),
                    ]),
                    ScriptType::Infer(1),
                )),
            ),
        ]))
    }

    fn list_return_type(
        &self,
        _inner: &ScriptType,
        arguments: &TupleType,
    ) -> TypeResult<ScriptType> {
        let arg = arguments.get_named("mapper").cloned().unwrap();
        let ret = arg.as_callable_ret(arguments)?;
        Ok(ScriptType::list_of(ret))
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let mut args = arguments.iter_args();
        let mapper = args
            .get("mapper")
            .ok_or_else(|| ScriptError::panic("mapper"))?;
        let mut value = args
            .get("initial")
            .ok_or_else(|| ScriptError::panic("initial"))?
            .clone();
        let mut mapped = Vec::new();
        for item in subject.items() {
            let args = Tuple::new(vec![
                TupleItem::unnamed(value.clone()),
                TupleItem::unnamed(item.clone()),
            ]);
            value = interpreter.eval_callable(mapper.clone(), &args)?;
            mapped.push(value.clone());
        }
        Ok(ScriptValue::List(Arc::new(List::new(mapped))))
    }
}

pub(crate) struct ListFilter;
impl ListMethod for ListFilter {
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Function(
            FunctionType::new(TupleType::from_single(inner.clone()), ScriptType::Bool),
        )))
    }

    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::list_of(inner.clone()))
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let callable = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;
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
    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
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
    ) -> ScriptResult<ScriptValue> {
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
    fn list_arguments_type(&self, inner: &ScriptType) -> TypeResult<TupleType> {
        let Some(tuple_typ) = inner.as_tuple() else {
            return Err(TypeError::new(TypeErrorKind::InvalidMapTo(inner.clone())));
        };
        Ok(TupleType::from_single(ScriptType::Function(
            FunctionType::new(tuple_typ.clone(), ScriptType::Infer(1)),
        )))
    }

    fn list_return_type(
        &self,
        inner: &ScriptType,
        arguments: &TupleType,
    ) -> TypeResult<ScriptType> {
        let arg = arguments.single()?;
        let ret = arg.as_callable_ret(&TupleType::from_single(inner.clone()))?;
        Ok(ScriptType::list_of(ret))
    }

    fn list_call(
        &self,
        interpreter: &Interpreter,
        subject: Arc<List>,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let callable = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;
        let mut mapped = Vec::new();
        for item in subject.items() {
            let tuple = item.as_tuple().expect("list of tuples");
            let value = interpreter.eval_callable(callable.clone(), &tuple)?;
            mapped.push(value);
        }
        Ok(ScriptValue::List(Arc::new(List::new(mapped))))
    }
}

struct ListEnumerate;
impl ListMethod for ListEnumerate {
    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        let tuple = TupleType::new(vec![
            TupleItemType::unnamed(ScriptType::Int),
            TupleItemType::unnamed(inner.clone()),
        ]);
        Ok(ScriptType::list_of(ScriptType::Tuple(tuple)))
    }

    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let items: Vec<_> = subject
            .items()
            .iter()
            .enumerate()
            .map(|(i, v)| {
                let tuple = Tuple::new(vec![
                    TupleItem::unnamed(ScriptValue::Int(i as i64)),
                    TupleItem::unnamed(v.clone()),
                ]);
                ScriptValue::Tuple(Arc::new(tuple))
            })
            .collect();

        Ok(ScriptValue::from(List::new(items)))
    }
}

struct ListFlip;
impl ListMethod for ListFlip {
    fn list_return_type(&self, inner: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        match inner {
            ScriptType::List(inner) => Ok(ScriptType::list_of(ScriptType::list_of(
                inner.as_ref().clone(),
            ))),
            _ => Err(TypeError::invalid_argument("List of lists", inner.clone())),
        }
    }

    fn list_call(
        &self,
        _: &Interpreter,
        subject: Arc<List>,
        _: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let mut flipped: Vec<Vec<ScriptValue>> = Vec::new();

        for inner in subject.items() {
            for (i, item) in inner.as_iterable().iter().enumerate() {
                if let Some(r) = flipped.get_mut(i) {
                    r.push(item.clone());
                } else {
                    flipped.push(vec![item.clone()]);
                }
            }
        }

        Ok(List::new(
            flipped
                .into_iter()
                .map(List::new)
                .map(ScriptValue::from)
                .collect(),
        )
        .into())
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
    ) -> ScriptResult<ScriptValue> {
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

    fn arguments_type(&self, subject: &ScriptType) -> TypeResult<TupleType> {
        match subject {
            ScriptType::EmptyList => self.empty_list_arguments_type(),
            ScriptType::List(inner) => self.list_arguments_type(inner),
            ScriptType::Range => self.list_arguments_type(&ScriptType::Int),
            _ => Err(TypeError::new(TypeErrorKind::InvalidIterable(
                subject.clone(),
            ))),
        }
    }

    fn return_type(&self, subject: &ScriptType, arguments: &TupleType) -> TypeResult<ScriptType> {
        // XXX Pass args
        match subject {
            ScriptType::EmptyList => self.empty_list_return_type(arguments),
            ScriptType::List(inner) => self.list_return_type(inner, arguments),
            ScriptType::Range => self.list_return_type(&ScriptType::Int, arguments),
            _ => Err(TypeError::new(TypeErrorKind::InvalidIterable(
                subject.clone(),
            ))),
        }
    }
}

fn inner_type(t: &ScriptType) -> TypeResult<ScriptType> {
    match t {
        ScriptType::EmptyList => todo!(),
        ScriptType::List(inner) => Ok(inner.as_ref().clone()),
        ScriptType::Range => Ok(ScriptType::Int),
        _ => Err(TypeError::invalid_argument("iterable", t.clone())),
    }
}
