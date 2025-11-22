use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter, Write as _},
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use crate::{
    fmt::{fmt_inner_list, fmt_tuple},
    ident::Ident,
    lexer::Loc,
    parser::{
        ArgumentsKind, Assignee, AstNode, Enumeration, Expression, Function, Parameter, Record,
        TypeExpression,
    },
};

#[derive(Debug)]
pub enum ScriptValue {
    Boolean(bool),
    String(Arc<str>),
    Number(i64),
    Range(i64, i64),
    List(Vec<Arc<ScriptValue>>),
    Tuple(Tuple),

    Rec {
        rec: Arc<Record>,
        values: Tuple,
    },

    Enum {
        def: Arc<Enumeration>,
        index: usize,
        values: Tuple,
    },

    State(RwLock<Arc<ScriptValue>>),
}

impl ScriptValue {
    pub const fn identity() -> Self {
        Self::Tuple(Tuple::identity())
    }
}

impl PartialEq for ScriptValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (
                Self::Enum {
                    def: dl,
                    index: il,
                    values: lv,
                },
                Self::Enum {
                    def: dr,
                    index: ir,
                    values: rv,
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
            ScriptValue::Enum { def, index, values } => {
                let var = &def.variants[*index];
                write!(f, "{}", var.name)?;
                if !values.is_empty() {
                    write!(f, "{values}")?
                }
                Ok(())
            }
            ScriptValue::Rec { rec, values } => {
                write!(f, "{}", rec.name)?;
                write!(f, "{values}")?;
                Ok(())
            }
            _ => todo!("Display impl for {self:?}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleItem {
    name: Option<Ident>,
    value: Arc<ScriptValue>,
}

impl TupleItem {
    pub fn new(name: Option<Ident>, value: Arc<ScriptValue>) -> Self {
        Self { name, value }
    }

    pub fn named(name: Ident, value: Arc<ScriptValue>) -> Self {
        Self::new(Some(name), value)
    }

    pub fn unnamed(value: Arc<ScriptValue>) -> Self {
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

    pub fn first(&self) -> Option<Arc<ScriptValue>> {
        self.0.first().map(|item| Arc::clone(&item.value))
    }

    pub fn get(&self, key: &Ident) -> Option<Arc<ScriptValue>> {
        self.0
            .iter()
            .find(|i| i.name.as_ref() == Some(key))
            .map(|item| Arc::clone(&item.value))
    }

    pub fn at(&self, index: usize) -> Option<Arc<ScriptValue>> {
        self.0.get(index).map(|item| Arc::clone(&item.value))
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
    ExplicitReturn(Arc<ScriptValue>),
    ImpliedReturn(Arc<ScriptValue>),
}

#[derive(Default, Clone)]
struct Scope {
    // XXX Try to remove Arc, so that the scope _owns_ the value
    // Don't clone the scope, but create something like a stack of scopes?
    locals: HashMap<Ident, Arc<ScriptValue>>,

    // XXX functions, records could all be script values in locals
    functions: HashMap<Ident, (Arc<Function>, Scope)>,
    records: HashMap<Ident, Arc<Record>>,
    enums: HashMap<Ident, Arc<Enumeration>>,

    arguments: Tuple,
}

pub trait NativeFn: FnMut(Tuple) -> ScriptValue {}
impl<T: FnMut(Tuple) -> ScriptValue> NativeFn for T {}

impl Scope {
    fn set_local(&mut self, name: Ident, value: Arc<ScriptValue>) {
        // Make sure we never assign a value to '_'
        if name.as_str() != "_" {
            self.locals.insert(name, value);
        }
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
                    eval_assignment(assignee, rhs, &mut scope);
                }
                AstNode::Function { name, fun, .. } => {
                    scope
                        .functions
                        .insert(name.clone(), (Arc::clone(fun), scope.clone()));
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
                    match *iterable {
                        ScriptValue::List(ref items) => {
                            for item in items {
                                let mut scope = scope.clone();
                                scope.set_local(ident.clone(), Arc::clone(item));
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
                                scope.set_local(ident.clone(), Arc::new(ScriptValue::Number(v)));
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
                    let ScriptValue::Boolean(val) = *val else {
                        panic!("Not a boolean");
                    };

                    let branch = if val { Some(body) } else { else_body.as_ref() };
                    if let Some(block) = branch {
                        let scope = scope.clone();
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

    fn eval_expr(&self, expr: &Loc<Expression>, scope: &Scope) -> Arc<ScriptValue> {
        match expr.as_ref() {
            Expression::String(s) => Arc::new(ScriptValue::String(Arc::clone(s))),
            Expression::StringInterpolate(parts) => {
                // TODO Lazy evaluation (StringInterpolate ScriptValue variant with scope)
                let mut builder = String::new();
                for (expr, _) in parts {
                    let val = self.eval_expr(expr, scope);
                    write!(builder, "{val}").unwrap();
                }
                Arc::new(ScriptValue::String(builder.into()))
            }
            Expression::Number(n) => Arc::new(ScriptValue::Number(*n)),
            Expression::True => Arc::new(ScriptValue::Boolean(true)),
            Expression::False => Arc::new(ScriptValue::Boolean(false)),
            Expression::Arguments => Arc::new(ScriptValue::Tuple(scope.arguments.clone())),
            Expression::List(s) => Arc::new(ScriptValue::List(
                s.iter().map(|i| self.eval_expr(i, scope)).collect(),
            )),
            Expression::Tuple(s) => {
                let items = s
                    .args
                    .iter()
                    .map(|arg| {
                        let value = self.eval_expr(&arg.expr, scope);
                        TupleItem::new(arg.name.clone(), value)
                    })
                    .collect();

                Arc::new(ScriptValue::Tuple(Tuple(items)))
            }
            Expression::Ref(ident) => match scope.locals.get(ident) {
                None => panic!("Undefined reference: {ident}"),
                Some(value) => Arc::clone(value),
            },
            Expression::PrefixedName(prefix, name) => match scope.enums.get(prefix) {
                Some(v) => {
                    if let Some((index, _variant)) =
                        v.variants.iter().enumerate().find(|(_, v)| v.name == *name)
                    {
                        Arc::new(ScriptValue::Enum {
                            def: Arc::clone(v),
                            index,
                            values: Tuple::identity(),
                        })
                    } else {
                        panic!("Enum variant not found: {name} in {prefix}");
                    }
                }
                None => panic!("Enum not found: {prefix}"),
            },
            Expression::Access { subject, key } => {
                let subject = self.eval_expr(subject, scope);
                match subject.as_ref() {
                    ScriptValue::Tuple(tuple) => {
                        match tuple.0.iter().find(|p| p.name.as_ref() == Some(key)) {
                            Some(value) => Arc::clone(&value.value),
                            None => panic!("{} doesn't have a property named: {}", tuple, key),
                        }
                    }
                    ScriptValue::Rec { rec, values } => {
                        let index = rec.params.iter().position(|p| p.name.as_ref() == Some(key));
                        match index {
                            None => panic!("{} doesn't have a property named: {}", rec.name, key),
                            Some(index) => match values.at(index) {
                                Some(value) => value,
                                None => panic!("Index out of range"),
                            },
                        }
                    }
                    _ => panic!("Unexpected property access on {subject:?}"),
                }
            }
            Expression::Not(expr) => {
                let val = self.eval_expr(expr, scope);
                if let ScriptValue::Boolean(b) = *val {
                    Arc::new(ScriptValue::Boolean(!b))
                } else {
                    panic!("Not a boolean")
                }
            }
            Expression::Equal(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                Arc::new(ScriptValue::Boolean(lhs.eq(&rhs)))
            }
            Expression::NotEqual(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                Arc::new(ScriptValue::Boolean(!lhs.eq(&rhs)))
            }
            Expression::Range(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Range(*lhs, *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            Expression::Addition(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs + *rhs))
                    }
                    _ => panic!("Expected numbers"),
                }
            }
            Expression::Subtraction(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs - *rhs))
                    }
                    _ => panic!("Expected numbers"),
                }
            }
            Expression::Multiplication(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs * *rhs))
                    }
                    _ => panic!("Expected numbers"),
                }
            }
            Expression::Division(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs / *rhs))
                    }
                    _ => panic!("Expected numbers"),
                }
            }
            Expression::Call { subject, arguments } => self.eval_call(subject, arguments, scope),
        }
    }

    fn eval_call(
        &self,
        subject: &Loc<Expression>,
        arguments: &ArgumentsKind,
        scope: &Scope,
    ) -> Arc<ScriptValue> {
        let arguments = self.eval_args(arguments, scope);
        match subject.as_ref() {
            Expression::Ref(name) => match name.as_str() {
                "state" => {
                    let arg = Arc::clone(&arguments.0.first().expect("state arg").value);
                    Arc::new(ScriptValue::State(RwLock::new(arg)))
                }
                _ => {
                    if let Some((fun, captured_scope)) = scope.functions.get(name) {
                        let mut inner_scope = captured_scope.clone();

                        let values = transform_args(&fun.params, arguments);
                        for item in &values.0 {
                            if let Some(name) = &item.name {
                                inner_scope.set_local(name.clone(), Arc::clone(&item.value));
                            }
                        }
                        inner_scope.arguments = values;

                        match self.eval_block(&fun.body, inner_scope) {
                            Completion::EndOfBlock => Arc::new(ScriptValue::identity()),
                            Completion::ExplicitReturn(v) => v,
                            Completion::ImpliedReturn(v) => v,
                        }
                    } else if let Some(rec) = scope.records.get(name) {
                        let values = transform_args(&rec.params, arguments);
                        let instance = ScriptValue::Rec {
                            rec: Arc::clone(rec),
                            values,
                        };

                        Arc::new(instance)
                    } else if let Some(func) = self.globals.get(name) {
                        let mut func = func.lock().unwrap();
                        let ret = func(arguments);
                        Arc::new(ret)
                    } else {
                        panic!("Undefined function: {subject:?}")
                    }
                }
            },
            Expression::PrefixedName(prefix, name) => {
                if let Some(v) = scope.enums.get(prefix) {
                    if let Some((index, variant)) =
                        v.variants.iter().enumerate().find(|(_, v)| v.name == *name)
                    {
                        let values = transform_args(&variant.params, arguments);
                        Arc::new(ScriptValue::Enum {
                            def: Arc::clone(v),
                            index,
                            values,
                        })
                    } else {
                        panic!("Enum variant not found: {name} in {prefix}");
                    }
                } else {
                    panic!("Enum not found: {prefix}")
                }
            }
            Expression::Access { subject, key } => {
                let subject = self.eval_expr(subject, scope);
                match (subject.as_ref(), key.as_str()) {
                    (ScriptValue::State(state), "get") => {
                        let v = state.read().unwrap();
                        Arc::clone(v.deref())
                    }
                    (ScriptValue::State(state), "set") => {
                        if let Some(val) = arguments.at(0) {
                            let mut v = state.write().unwrap();
                            *v = val;
                        }
                        Arc::new(ScriptValue::identity())
                    }
                    (ScriptValue::List(list), "push") => {
                        // This is the Copy on Write feature of the language in play.
                        // Can we avoid copy, if we see that the original will not be used again?
                        let mut res = list.clone();
                        for arg in &arguments.0 {
                            res.push(Arc::clone(&arg.value));
                        }
                        Arc::new(ScriptValue::List(res))
                    }
                    (ScriptValue::Rec { rec, values }, "with") => {
                        let mut items = values.0.clone();

                        // XXX Should be possible to use "apply" here as well

                        for (param, v) in rec.params.iter().zip(items.iter_mut()) {
                            if let Some(name) = &param.name
                                && let Some(val) = arguments.get(name)
                            {
                                v.value = val;
                            }
                        }

                        Arc::new(ScriptValue::Rec {
                            rec: Arc::clone(rec),
                            values: Tuple(items),
                        })
                    }
                    _ => panic!("Unknown method: {key} on {subject:?}"),
                }
            }
            _ => panic!("Call on something that's not a ref"),
        }
    }

    fn eval_args(&self, arguments: &ArgumentsKind, scope: &Scope) -> Tuple {
        match arguments {
            ArgumentsKind::Inline(arguments) => {
                let items: Vec<_> = arguments
                    .args
                    .iter()
                    .map(|a| TupleItem::new(a.name.clone(), self.eval_expr(&a.expr, scope)))
                    .collect();

                Tuple(items)
            }
            ArgumentsKind::Destructure(expr) => {
                let arg = self.eval_expr(expr, scope);
                match arg.as_ref() {
                    ScriptValue::Tuple(tuple) => tuple.clone(),
                    ScriptValue::Rec { values, .. } => values.clone(),
                    _ => panic!("Expected a tuple, found {arg}"),
                }
            }
            ArgumentsKind::DestructureImplicit(_) => scope.arguments.clone(),
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
fn transform_args(params: &[Parameter], args: Tuple) -> Tuple {
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

    fn transform_value(par: &Parameter, arg: &TupleItem) -> Arc<ScriptValue> {
        match (par.type_expr.as_ref(), arg.value.as_ref()) {
            (TypeExpression::Tuple(t), ScriptValue::Tuple(tup)) => {
                let applied = transform_args(t, tup.clone());
                Arc::new(ScriptValue::Tuple(applied))
            }
            (TypeExpression::Tuple(t), ScriptValue::Rec { values, .. }) => {
                let applied = transform_args(t, values.clone());
                Arc::new(ScriptValue::Tuple(applied))
            }
            _ => arg.value.clone(),
        }
    }

    Tuple(items)
}

fn eval_assignment(lhs: &Loc<Assignee>, rhs: Arc<ScriptValue>, scope: &mut Scope) {
    let lhs = lhs.as_ref();
    match (&lhs.name, &lhs.pattern) {
        (None, None) => {}
        (Some(name), None) => scope.set_local(name.clone(), rhs),
        (_, Some(pattern)) => match rhs.as_ref() {
            ScriptValue::Tuple(values) => eval_destructure(pattern, values, scope),
            ScriptValue::Rec { values, .. } => eval_destructure(pattern, values, scope),
            _ => panic!("Expected tuple, found: {rhs}"),
        },
    }
}

fn eval_destructure(lhs: &[Loc<Assignee>], rhs: &Tuple, scope: &mut Scope) {
    let mut positional = rhs.0.iter().filter(|arg| arg.name.is_none());
    for par in lhs.iter() {
        if let Some(name) = &par.as_ref().name {
            if let Some(arg) = rhs.0.iter().find(|a| a.name.as_ref() == Some(name)) {
                eval_assignment(par, Arc::clone(&arg.value), scope);
            } else if let Some(arg) = positional.next() {
                eval_assignment(par, Arc::clone(&arg.value), scope);
            } else {
                panic!("Missing argument {name}");
            }
        } else if let Some(arg) = positional.next() {
            eval_assignment(par, Arc::clone(&arg.value), scope);
        } else {
            panic!("Missing positional argument");
        }
    }
}
