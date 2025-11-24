use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter, Write as _},
    sync::{Arc, Mutex, RwLock},
};

use crate::{
    fmt::{fmt_inner_list, fmt_tuple},
    ident::Ident,
    lexer::Src,
    parser::{
        ArgumentsKind, Assignee, AstNode, Enumeration, Expression, Function, Parameter, Record,
        TypeExpression,
    },
};

#[derive(Debug, Clone)]
pub enum ScriptValue {
    Boolean(bool),
    String(Arc<str>),
    Number(i64),
    Range(i64, i64),
    List(Arc<Vec<ScriptValue>>),
    Tuple(Arc<Tuple>),

    NaN,

    // The *instance* of a record. Not the record itself (which might be treated as a function)
    Rec {
        def: Arc<Record>,
        value: Arc<Tuple>,
    },

    Enum {
        def: Arc<Enumeration>,
        index: usize,
        value: Arc<Tuple>,
    },

    Fun {
        function: Arc<Function>,
        captured_scope: Arc<Scope>, // XXX Fix warning with dedicated struct type
    },

    State(Arc<RwLock<ScriptValue>>),
}

impl ScriptValue {
    pub fn identity() -> Self {
        Self::Tuple(Arc::new(Tuple::identity()))
    }

    pub fn is_nan(&self) -> bool {
        matches!(self, Self::NaN)
    }

    pub fn as_tuple(&self) -> Option<&Arc<Tuple>> {
        match self {
            Self::Tuple(tuple) => Some(tuple),
            Self::Rec { value, .. } => Some(value),
            _ => None,
        }
    }
}

impl PartialEq for ScriptValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Boolean(l), Self::Boolean(r)) => l == r,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (
                Self::Enum {
                    def: dl,
                    index: il,
                    value: lv,
                },
                Self::Enum {
                    def: dr,
                    index: ir,
                    value: rv,
                },
            ) => Arc::ptr_eq(dl, dr) && *il == *ir && *lv == *rv,
            _ => todo!("Equality for {self:?}"),
        }
    }
}

impl Display for ScriptValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ScriptValue::String(s) => write!(f, "{s}"),
            ScriptValue::Number(n) => write!(f, "{n}"),
            ScriptValue::Tuple(t) => write!(f, "{t}"),
            ScriptValue::Boolean(b) => match b {
                true => write!(f, "true"),
                false => write!(f, "false"),
            },
            ScriptValue::List(items) => {
                write!(f, "[")?;
                fmt_inner_list(f, items)?;
                write!(f, "]")?;
                Ok(())
            }
            ScriptValue::Enum {
                def,
                index,
                value: values,
            } => {
                let var = &def.variants[*index];
                write!(f, "{}", var.name)?;
                if !values.is_empty() {
                    write!(f, "{values}")?
                }
                Ok(())
            }
            ScriptValue::Rec {
                def: rec,
                value: values,
            } => {
                write!(f, "{}", rec.name)?;
                write!(f, "{values}")?;
                Ok(())
            }
            ScriptValue::NaN => write!(f, "NaN"),
            _ => todo!("Display impl for {self:?}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleItem {
    name: Option<Ident>,
    value: ScriptValue,
}

impl TupleItem {
    pub fn new(name: Option<Ident>, value: ScriptValue) -> Self {
        Self { name, value }
    }

    pub fn named(name: Ident, value: ScriptValue) -> Self {
        Self::new(Some(name), value)
    }

    pub fn unnamed(value: ScriptValue) -> Self {
        Self::new(None, value)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Tuple(Vec<TupleItem>);

impl Tuple {
    const fn identity() -> Self {
        Self(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn first(&self) -> Option<&ScriptValue> {
        self.0.first().map(|item| &item.value)
    }

    pub fn get(&self, key: &Ident) -> Option<&ScriptValue> {
        self.0
            .iter()
            .find(|i| i.name.as_ref() == Some(key))
            .map(|item| &item.value)
    }

    pub fn at(&self, index: usize) -> Option<&ScriptValue> {
        self.0.get(index).map(|item| &item.value)
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_tuple(f, self.0.iter().map(|a| (a.name.clone(), &a.value)))
    }
}

#[derive(Debug)]
pub enum Completion {
    EndOfBlock,
    ExplicitReturn(ScriptValue),
    ImpliedReturn(ScriptValue),
}

#[derive(Default, Clone)]
pub(crate) struct Scope {
    // XXX Try to remove Arc, so that the scope _owns_ the value
    // Don't clone the scope, but create something like a stack of scopes?
    locals: HashMap<Ident, ScriptValue>,

    // XXX Use some "types" like in validation?
    records: HashMap<Ident, Arc<Record>>,
    enums: HashMap<Ident, Arc<Enumeration>>,

    arguments: Arc<Tuple>,
}

pub trait NativeFn: FnMut(&Tuple) -> ScriptValue {}
impl<T: FnMut(&Tuple) -> ScriptValue> NativeFn for T {}

impl Scope {
    fn set_local(&mut self, name: Ident, value: ScriptValue) {
        // Make sure we never assign a value to '_'
        if name.as_str() != "_" {
            self.locals.insert(name, value);
        }
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope").finish()
    }
}

#[derive(Default)]
pub struct Engine {
    globals: HashMap<Ident, Mutex<Box<dyn NativeFn>>>,
}

impl Engine {
    pub fn with_global<F>(self, name: impl Into<Ident>, val: F) -> Self
    where
        F: NativeFn + 'static,
    {
        let mut globals = self.globals;
        globals.insert(name.into(), Mutex::new(Box::new(val)));
        Self { globals }
    }

    pub fn eval(&self, ast: &[AstNode]) {
        self.eval_block(ast, Scope::default());
    }

    fn eval_block(&self, ast: &[AstNode], mut scope: Scope) -> Completion {
        for node in ast {
            match node {
                AstNode::Assignment { assignee, value } => {
                    let rhs = self.eval_expr(value, &scope);
                    eval_assignment(assignee, &rhs, &mut scope);
                }
                AstNode::Function { name, fun, .. } => {
                    let val = ScriptValue::Fun {
                        function: Arc::clone(fun),
                        captured_scope: Arc::new(scope.clone()),
                    };
                    scope.set_local(name.clone(), val);
                }
                AstNode::Rec(rec) => {
                    scope.records.insert(rec.name.clone(), Arc::clone(rec));
                }
                AstNode::Enum(rec) => {
                    scope.enums.insert(rec.name.clone(), Arc::clone(rec));
                }
                AstNode::Iteration {
                    ident,
                    iterable,
                    body,
                } => {
                    let iterable = self.eval_expr(iterable, &scope);
                    match iterable {
                        ScriptValue::List(ref items) => {
                            for item in items.as_ref() {
                                let mut scope = scope.clone();
                                scope.set_local(ident.clone(), item.clone());
                                if let Completion::ExplicitReturn(val) =
                                    self.eval_block(body, scope)
                                {
                                    return Completion::ExplicitReturn(val);
                                }
                            }
                        }
                        ScriptValue::Range(lhs, rhs) => {
                            for v in lhs..=rhs {
                                let mut scope = scope.clone();
                                scope.set_local(ident.clone(), ScriptValue::Number(v));
                                if let Completion::ExplicitReturn(val) =
                                    self.eval_block(body, scope)
                                {
                                    return Completion::ExplicitReturn(val);
                                }
                            }
                        }
                        _ => panic!("Expected iterable, found: {iterable}"),
                    }
                }
                AstNode::Condition {
                    cond,
                    body,
                    else_body,
                } => {
                    let val = self.eval_expr(cond, &scope);
                    let ScriptValue::Boolean(val) = val else {
                        panic!("Not a boolean");
                    };

                    let branch = if val { Some(body) } else { else_body.as_ref() };
                    if let Some(block) = branch {
                        let mut scope = scope.clone();
                        match self.eval_block(block, scope) {
                            Completion::ExplicitReturn(val) => {
                                return Completion::ExplicitReturn(val);
                            }
                            Completion::ImpliedReturn(val) if ast.len() == 1 => {
                                return Completion::ImpliedReturn(val);
                            }
                            _ => (),
                        }
                    }
                }
                AstNode::Expression(expr) => {
                    let val = self.eval_expr(expr, &scope);

                    // Implied return if and only if the block consist of exactly one expression
                    if ast.len() == 1 {
                        return Completion::ImpliedReturn(val);
                    }
                }
                AstNode::Return(expr) => {
                    let val = self.eval_expr(expr, &scope);
                    return Completion::ExplicitReturn(val);
                }
            }
        }

        Completion::EndOfBlock
    }

    fn eval_expr(&self, expr: &Src<Expression>, scope: &Scope) -> ScriptValue {
        match expr.as_ref() {
            Expression::String(s) => ScriptValue::String(Arc::clone(s)),
            Expression::StringInterpolate(parts) => {
                // TODO Lazy evaluation (StringInterpolate ScriptValue variant with scope)
                let mut builder = String::new();
                for (expr, _) in parts {
                    let val = self.eval_expr(expr, scope);
                    write!(builder, "{val}").unwrap();
                }
                ScriptValue::String(builder.into())
            }
            Expression::Number(n) => ScriptValue::Number(*n),
            Expression::True => ScriptValue::Boolean(true),
            Expression::False => ScriptValue::Boolean(false),
            Expression::Arguments => ScriptValue::Tuple(Arc::clone(&scope.arguments)),
            Expression::List(s) => {
                let values = s.iter().map(|i| self.eval_expr(i, scope)).collect();
                ScriptValue::List(Arc::new(values))
            }
            Expression::Tuple(s) => {
                let items = s
                    .args
                    .iter()
                    .map(|arg| {
                        let value = self.eval_expr(&arg.expr, scope);
                        TupleItem::new(arg.name.clone(), value)
                    })
                    .collect();

                ScriptValue::Tuple(Arc::new(Tuple(items)))
            }
            Expression::Ref(ident) => match scope.locals.get(ident) {
                None => panic!("Undefined reference: {ident}"),
                Some(value) => value.clone(),
            },
            Expression::PrefixedName(prefix, name) => {
                if let Some(v) = scope.records.get(prefix) {
                    todo!()
                } else if let Some(v) = scope.enums.get(prefix) {
                    if let Some((index, _variant)) =
                        v.variants.iter().enumerate().find(|(_, v)| v.name == *name)
                    {
                        ScriptValue::Enum {
                            def: Arc::clone(v),
                            index,
                            value: Arc::new(Tuple::identity()),
                        }
                    } else {
                        panic!("Enum variant not found: {name} in {prefix}");
                    }
                } else {
                    panic!("Enum not found: {prefix}")
                }
            }
            Expression::Access { subject, key } => {
                let subject = self.eval_expr(subject, scope);
                match subject {
                    ScriptValue::Tuple(tuple) => {
                        match tuple.0.iter().find(|p| p.name.as_ref() == Some(key)) {
                            Some(value) => value.value.clone(),
                            None => panic!("{} doesn't have a property named: {}", tuple, key),
                        }
                    }
                    ScriptValue::Rec {
                        def: rec,
                        value: values,
                    } => {
                        let index = rec.params.iter().position(|p| p.name.as_ref() == Some(key));
                        match index {
                            None => panic!("{} doesn't have a property named: {}", rec.name, key),
                            Some(index) => match values.at(index) {
                                Some(value) => value.clone(),
                                None => panic!("Index out of range"),
                            },
                        }
                    }
                    _ => panic!("Unexpected property access on {subject:?}"),
                }
            }
            Expression::Not(expr) => {
                let val = self.eval_expr(expr, scope);
                if let ScriptValue::Boolean(b) = val {
                    ScriptValue::Boolean(!b)
                } else {
                    panic!("Not a boolean")
                }
            }
            Expression::Equal(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                ScriptValue::Boolean(lhs.eq(&rhs))
            }
            Expression::NotEqual(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                ScriptValue::Boolean(!lhs.eq(&rhs))
            }
            Expression::Range(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs, rhs) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        ScriptValue::Range(lhs, rhs)
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            Expression::Negate(expr) => {
                let val = self.eval_expr(expr, scope);
                match val {
                    ScriptValue::Number(n) => ScriptValue::Number(-n),
                    ScriptValue::NaN => val,
                    _ => panic!("Expected number"),
                }
            }
            Expression::Addition(lhs, rhs) => {
                self.eval_arithmetic(i64::checked_add, lhs, rhs, scope)
            }
            Expression::Subtraction(lhs, rhs) => {
                self.eval_arithmetic(i64::checked_sub, lhs, rhs, scope)
            }
            Expression::Multiplication(lhs, rhs) => {
                self.eval_arithmetic(i64::checked_mul, lhs, rhs, scope)
            }
            Expression::Division(lhs, rhs) => {
                self.eval_arithmetic(i64::checked_div, lhs, rhs, scope)
            }
            Expression::LessThan(lhs, rhs) => self.eval_comparison(|a, b| a < b, lhs, rhs, scope),
            Expression::GreaterThan(lhs, rhs) => {
                self.eval_comparison(|a, b| a > b, lhs, rhs, scope)
            }
            Expression::LessOrEqual(lhs, rhs) => {
                self.eval_comparison(|a, b| a <= b, lhs, rhs, scope)
            }
            Expression::GreaterOrEqual(lhs, rhs) => {
                self.eval_comparison(|a, b| a >= b, lhs, rhs, scope)
            }
            Expression::Call { subject, arguments } => self.eval_call(subject, arguments, scope),
        }
    }

    fn eval_arithmetic<F>(
        &self,
        op: F,
        lhs: &Src<Expression>,
        rhs: &Src<Expression>,
        scope: &Scope,
    ) -> ScriptValue
    where
        F: FnOnce(i64, i64) -> Option<i64>,
    {
        let lhs = self.eval_expr(lhs, scope);
        let rhs = self.eval_expr(rhs, scope);
        match (lhs, rhs) {
            (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => op(lhs, rhs)
                .map(ScriptValue::Number)
                .unwrap_or(ScriptValue::NaN),
            (ScriptValue::NaN, ScriptValue::Number(_)) => ScriptValue::NaN,
            (ScriptValue::Number(_), ScriptValue::NaN) => ScriptValue::NaN,
            _ => panic!("Expected numbers"),
        }
    }

    fn eval_comparison<F>(
        &self,
        op: F,
        lhs: &Src<Expression>,
        rhs: &Src<Expression>,
        scope: &Scope,
    ) -> ScriptValue
    where
        F: FnOnce(i64, i64) -> bool,
    {
        let lhs = self.eval_expr(lhs, scope);
        let rhs = self.eval_expr(rhs, scope);
        match (lhs, rhs) {
            (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                ScriptValue::Boolean(op(lhs, rhs))
            }
            (ScriptValue::NaN, ScriptValue::Number(_)) => ScriptValue::NaN,
            (ScriptValue::Number(_), ScriptValue::NaN) => ScriptValue::NaN,
            _ => panic!("Expected numbers"),
        }
    }

    fn eval_call(
        &self,
        subject: &Src<Expression>,
        arguments: &ArgumentsKind,
        scope: &Scope,
    ) -> ScriptValue {
        let arguments = self.eval_args(arguments, scope);
        match subject.as_ref() {
            Expression::Ref(name) => match name.as_str() {
                "state" => {
                    let arg = &arguments.0.first().expect("state arg").value;
                    ScriptValue::State(Arc::new(RwLock::new(arg.clone())))
                }
                _ => {
                    if let Some(val) = scope.locals.get(name) {
                        if let ScriptValue::Fun {
                            function,
                            captured_scope,
                        } = val
                        {
                            let mut inner_scope = Scope::clone(captured_scope);

                            let values = transform_args(&function.params, &arguments);
                            for item in &values.0 {
                                if let Some(name) = &item.name {
                                    inner_scope.set_local(name.clone(), item.value.clone());
                                }
                            }
                            inner_scope.arguments = Arc::new(values);

                            match self.eval_block(&function.body, inner_scope) {
                                Completion::EndOfBlock => ScriptValue::identity(),
                                Completion::ExplicitReturn(v) => v,
                                Completion::ImpliedReturn(v) => v,
                            }
                        } else {
                            panic!("Expected function, found {val:?}")
                        }
                    } else if let Some(rec) = scope.records.get(name) {
                        let values = transform_args(&rec.params, &arguments);
                        ScriptValue::Rec {
                            def: Arc::clone(rec),
                            value: Arc::new(values),
                        }
                    } else if let Some(func) = self.globals.get(name) {
                        let mut func = func.lock().unwrap();
                        func(&arguments)
                    } else {
                        panic!("Undefined function: {subject:?}")
                    }
                }
            },
            Expression::PrefixedName(prefix, name) => {
                if let Some(def) = scope.records.get(prefix) {
                    match name.as_str() {
                        "parse" => {
                            if let Some(ScriptValue::String(s)) = arguments.at(0) {
                                let mut nums =
                                    s.split_whitespace().map(|t| t.parse::<i64>().unwrap());
                                let mut values = Vec::new();
                                for d in &def.params {
                                    values.push(TupleItem::new(
                                        d.name.clone(),
                                        ScriptValue::Number(nums.next().unwrap()),
                                    ));
                                }

                                ScriptValue::Rec {
                                    def: Arc::clone(def),
                                    value: Arc::new(Tuple(values)),
                                }
                            } else {
                                panic!("Expected string");
                            }
                        }
                        _ => panic!("Method not found: {name} in {prefix}"),
                    }
                } else if let Some(v) = scope.enums.get(prefix) {
                    if let Some((index, variant)) =
                        v.variants.iter().enumerate().find(|(_, v)| v.name == *name)
                    {
                        let values = transform_args(&variant.params, &arguments);
                        ScriptValue::Enum {
                            def: Arc::clone(v),
                            index,
                            value: Arc::new(values),
                        }
                    } else {
                        panic!("Enum variant not found: {name} in {prefix}");
                    }
                } else {
                    panic!("Enum not found: {prefix}")
                }
            }
            Expression::Access { subject, key } => {
                let subject = self.eval_expr(subject, scope);
                match (&subject, key.as_str()) {
                    (ScriptValue::State(state), "get") => {
                        let v = state.read().unwrap();
                        v.clone()
                    }
                    (ScriptValue::State(state), "set") => {
                        if let Some(val) = arguments.at(0) {
                            let mut v = state.write().unwrap();
                            *v = val.clone();
                        }
                        ScriptValue::identity()
                    }
                    (ScriptValue::List(values), "push") => {
                        // This is the Copy on Write feature of the language in play.
                        // Can we avoid copy, if we see that the original will not be used again?
                        let mut res = Vec::clone(values);
                        for arg in &arguments.0 {
                            res.push(arg.value.clone());
                        }
                        ScriptValue::List(Arc::new(res))
                    }
                    (ScriptValue::List(list), "unzip") => {
                        // TODO These methods should create "stream" or "iterator" instead of just copying the whole list up-front

                        let tuples: Vec<_> = list
                            .iter()
                            .map(|arg| {
                                if let Some(l) = arg.as_tuple() {
                                    Arc::clone(l)
                                } else {
                                    panic!("Not a tuple")
                                }
                            })
                            .collect();

                        let mut lists = Vec::new();

                        let tuple = if let Some(first) = tuples.first() {
                            for it in &first.0 {
                                lists.push(vec![it.value.clone()]);
                            }

                            for tuple in &tuples[1..] {
                                for (i, it) in tuple.0.iter().enumerate() {
                                    lists[i].push(it.value.clone());
                                }
                            }

                            let items = lists
                                .into_iter()
                                .map(|l| TupleItem::unnamed(ScriptValue::List(Arc::new(l))))
                                .collect();
                            Tuple(items)
                        } else {
                            Tuple::identity()
                        };

                        ScriptValue::Tuple(Arc::new(tuple))
                    }
                    (ScriptValue::List(_), "zip") => {
                        // Input should be empty

                        let lists: Vec<_> = arguments
                            .0
                            .iter()
                            .map(|arg| {
                                if let ScriptValue::List(l) = &arg.value {
                                    Arc::clone(l)
                                } else {
                                    panic!("Not a list")
                                }
                            })
                            .collect();

                        let mut tuples = Vec::new();
                        'outer: for i in 0.. {
                            let mut items = Vec::new();

                            for l in &lists {
                                if let Some(it) = l.get(i) {
                                    items.push(TupleItem::unnamed(it.clone()));
                                } else {
                                    break 'outer;
                                }
                            }

                            tuples.push(Tuple(items));
                        }

                        let values = tuples
                            .into_iter()
                            .map(Arc::new)
                            .map(ScriptValue::Tuple)
                            .collect();

                        ScriptValue::List(Arc::new(values))
                    }
                    (ScriptValue::List(list), "sum") => {
                        if list.iter().any(|v| v.is_nan()) {
                            ScriptValue::NaN
                        } else {
                            ScriptValue::Number(
                                list.iter()
                                    .map(|val| {
                                        if let ScriptValue::Number(n) = val {
                                            *n
                                        } else {
                                            panic!("Expected numbers")
                                        }
                                    })
                                    .reduce(|n, a| n + a)
                                    .unwrap_or_default(),
                            )
                        }
                    }
                    (ScriptValue::List(list), "sort") => {
                        let mut values: Vec<_> = list
                            .iter()
                            .map(|val| {
                                if let ScriptValue::Number(n) = val {
                                    *n
                                } else {
                                    panic!("Expected numbers")
                                }
                            })
                            .collect();

                        values.sort();

                        let values = values.into_iter().map(ScriptValue::Number).collect();

                        ScriptValue::List(Arc::new(values))
                    }
                    (ScriptValue::List(list), "map") => {
                        if let Some(val) = arguments.at(0) {
                            if let ScriptValue::Fun {
                                function,
                                captured_scope,
                            } = val
                            {
                                let mut mapped = Vec::new();
                                for item in list.as_ref() {
                                    let mut inner_scope = Scope::clone(captured_scope);

                                    let arguments = Tuple(vec![TupleItem::unnamed(item.clone())]);
                                    let values = transform_args(&function.params, &arguments);
                                    for item in &values.0 {
                                        if let Some(name) = &item.name {
                                            inner_scope.set_local(name.clone(), item.value.clone());
                                        }
                                    }
                                    inner_scope.arguments = Arc::new(values);

                                    let ret = match self.eval_block(&function.body, inner_scope) {
                                        Completion::EndOfBlock => ScriptValue::identity(),
                                        Completion::ExplicitReturn(v) => v,
                                        Completion::ImpliedReturn(v) => v,
                                    };

                                    mapped.push(ret);
                                }

                                ScriptValue::List(Arc::new(mapped))
                            } else {
                                panic!("Argument to 'map' is not a function");
                            }
                        } else {
                            panic!("Missing positional argument");
                        }
                    }
                    (ScriptValue::List(list), "map_to") => {
                        if let Some(val) = arguments.at(0) {
                            if let ScriptValue::Fun {
                                function,
                                captured_scope,
                            } = val
                            {
                                let mut mapped = Vec::new();
                                for item in list.as_ref() {
                                    let mut inner_scope = Scope::clone(captured_scope);

                                    let Some(arguments) = item.as_tuple() else {
                                        panic!("Epected tuple");
                                    };

                                    let values = transform_args(&function.params, arguments);
                                    for item in &values.0 {
                                        if let Some(name) = &item.name {
                                            inner_scope.set_local(name.clone(), item.value.clone());
                                        }
                                    }
                                    inner_scope.arguments = Arc::new(values);

                                    let ret = match self.eval_block(&function.body, inner_scope) {
                                        Completion::EndOfBlock => ScriptValue::identity(),
                                        Completion::ExplicitReturn(v) => v,
                                        Completion::ImpliedReturn(v) => v,
                                    };

                                    mapped.push(ret);
                                }

                                ScriptValue::List(Arc::new(mapped))
                            } else {
                                panic!("Argument to 'map' is not a function");
                            }
                        } else {
                            panic!("Missing positional argument");
                        }
                    }
                    (
                        ScriptValue::Rec {
                            def: rec,
                            value: values,
                        },
                        "with",
                    ) => {
                        let mut values = values.0.clone();

                        // XXX Should be possible to use "apply" here as well

                        for (param, v) in rec.params.iter().zip(values.iter_mut()) {
                            if let Some(name) = &param.name
                                && let Some(val) = arguments.get(name)
                            {
                                v.value = val.clone();
                            }
                        }

                        ScriptValue::Rec {
                            def: Arc::clone(rec),
                            value: Arc::new(Tuple(values)),
                        }
                    }
                    (ScriptValue::String(s), "lines") => {
                        let lines = s
                            .lines()
                            .filter(|l| !l.is_empty())
                            .map(|l| ScriptValue::String(Arc::from(l)))
                            .collect();
                        ScriptValue::List(Arc::new(lines))
                    }
                    _ => panic!("Unknown method: {key} on {subject:?}"),
                }
            }
            _ => panic!("Call on something that's not a ref"),
        }
    }

    fn eval_args(&self, arguments: &ArgumentsKind, scope: &Scope) -> Arc<Tuple> {
        match arguments {
            ArgumentsKind::Inline(arguments) => {
                let items: Vec<_> = arguments
                    .args
                    .iter()
                    .map(|a| TupleItem::new(a.name.clone(), self.eval_expr(&a.expr, scope)))
                    .collect();

                Arc::new(Tuple(items))
            }
            ArgumentsKind::Destructure(expr) => {
                let arg = self.eval_expr(expr, scope);
                match &arg {
                    ScriptValue::Tuple(tuple) => Arc::clone(tuple),
                    ScriptValue::Rec { value: values, .. } => Arc::clone(values),
                    _ => panic!("Expected a tuple, found {arg}"),
                }
            }
            ArgumentsKind::DestructureImplicit(_) => Arc::clone(&scope.arguments),
        }
    }
}

// "Transform" the arguments tuple passed to a function call-side, into the tuple
// seen in scope from inside the function.
//
// Example:
// fun foo(a: int, b: str) {}
//
// foo(10, 20)
//
// When calling the function, we pass a tuple with unnamed items (int, int).
// Inside foo(), we will receive a tuple with named items (a: int, b: int).
// This also allows named arguments at call-site to be order differently than
// inside the function, or even before positional arguments.
fn transform_args(params: &[Parameter], args: &Tuple) -> Tuple {
    let mut items = Vec::new();

    let mut positional = args.0.iter().filter(|arg| arg.name.is_none());
    for par in params.iter() {
        if let Some(name) = par.name.clone() {
            if let Some(arg) = args.0.iter().find(|a| a.name.as_ref() == Some(&name)) {
                let val = transform_value(par, arg);
                items.push(TupleItem::named(name, val));
            } else if let Some(arg) = positional.next() {
                let val = transform_value(par, arg);
                items.push(TupleItem::named(name, val));
            } else {
                panic!("Missing argument {name}");
            }
        } else if let Some(arg) = positional.next() {
            let val = transform_value(par, arg);
            items.push(TupleItem::unnamed(val));
        } else {
            panic!("Missing positional argument");
        }
    }

    fn transform_value(par: &Parameter, arg: &TupleItem) -> ScriptValue {
        match (par.type_expr.as_ref(), &arg.value) {
            (TypeExpression::Tuple(t), ScriptValue::Tuple(tup)) => {
                let applied = transform_args(t, tup);
                ScriptValue::Tuple(Arc::new(applied))
            }
            (TypeExpression::Tuple(t), ScriptValue::Rec { value: values, .. }) => {
                let applied = transform_args(t, values);
                ScriptValue::Tuple(Arc::new(applied))
            }
            _ => arg.value.clone(),
        }
    }

    Tuple(items)
}

fn eval_assignment(lhs: &Src<Assignee>, rhs: &ScriptValue, scope: &mut Scope) {
    let lhs = lhs.as_ref();
    match (&lhs.name, &lhs.pattern) {
        (None, None) => {}
        (Some(name), None) => scope.set_local(name.clone(), rhs.clone()),
        (_, Some(pattern)) => match rhs {
            ScriptValue::Tuple(values) => eval_destructure(pattern, values, scope),
            ScriptValue::Rec { value: values, .. } => eval_destructure(pattern, values, scope),
            _ => panic!("Expected tuple, found: {rhs}"),
        },
    }
}

fn eval_destructure(lhs: &[Src<Assignee>], rhs: &Tuple, scope: &mut Scope) {
    let mut positional = rhs.0.iter().filter(|arg| arg.name.is_none());
    for par in lhs.iter() {
        if let Some(name) = &par.as_ref().name {
            if let Some(arg) = rhs.0.iter().find(|a| a.name.as_ref() == Some(name)) {
                eval_assignment(par, &arg.value, scope);
            } else if let Some(arg) = positional.next() {
                eval_assignment(par, &arg.value, scope);
            } else {
                panic!("Missing argument {name}");
            }
        } else if let Some(arg) = positional.next() {
            eval_assignment(par, &arg.value, scope);
        } else {
            panic!("Missing positional argument");
        }
    }
}
