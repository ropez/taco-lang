use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter, Write as _},
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use crate::{
    fmt::{fmt_inner_list, fmt_tuple},
    parser::{
        Arguments, Assignmee, AstNode, Enumeration, Expression, ExpressionKind, Function,
        Parameter, Record,
    },
};

#[derive(Debug)]
pub enum ScriptValue {
    Boolean(bool),
    String(Arc<str>),
    Number(i64),
    Range(i64, i64),
    List(Vec<Arc<ScriptValue>>),
    Tuple(Vec<Arc<ScriptValue>>),

    Rec {
        rec: Arc<Record>,
        values: Vec<Arc<ScriptValue>>,
    },

    Enum {
        def: Arc<Enumeration>,
        index: usize,
        values: Vec<Arc<ScriptValue>>, // Maybe use a Tuple struct?
    },

    State(RwLock<Arc<ScriptValue>>),
}

impl ScriptValue {
    pub const fn identity() -> Self {
        Self::Tuple(Vec::new())
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
            ScriptValue::Tuple(items) => fmt_tuple(f, items),
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
                    fmt_tuple(f, values)?;
                }
                Ok(())
            }
            _ => todo!("Display impl for {self:?}"),
        }
    }
}

#[derive(Debug)]
pub struct EvaluatedArguments {
    pub(crate) args: Vec<Arc<ScriptValue>>,
    pub(crate) kwargs: Vec<(Arc<str>, Arc<ScriptValue>)>,
}

#[derive(Debug)]
pub enum Completion {
    EndOfBlock,
    ExplicitReturn(Arc<ScriptValue>),
    ImpliedReturn(Arc<ScriptValue>),
}

#[derive(Default, Clone)]
struct Scope {
    locals: HashMap<Arc<str>, Arc<ScriptValue>>,

    // XXX functions, records could all be script values in locals
    functions: HashMap<Arc<str>, (Arc<Function>, Scope)>,
    records: HashMap<Arc<str>, Arc<Record>>,
    enums: HashMap<Arc<str>, Arc<Enumeration>>,
}

pub trait NativeFn: FnMut(&[Arc<ScriptValue>]) -> ScriptValue {}
impl<T: FnMut(&[Arc<ScriptValue>]) -> ScriptValue> NativeFn for T {}

impl Scope {
    fn set_local(&mut self, name: Arc<str>, value: Arc<ScriptValue>) {
        // Make sure we never assign a value to '_'
        if name.as_ref() != "_" {
            self.locals.insert(name, value);
        }
    }
}

#[derive(Default)]
pub struct Engine {
    globals: HashMap<Arc<str>, Mutex<Box<dyn NativeFn>>>,
}

impl Engine {
    pub fn with_global<F>(self, name: impl Into<Arc<str>>, val: F) -> Self
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
                AstNode::Assignment { assignee, value } => match assignee {
                    Assignmee::Scalar(name) => {
                        scope.set_local(Arc::clone(name), self.eval_expr(value, &scope));
                    }
                    Assignmee::Destructure(names) => {
                        let rhs = self.eval_expr(value, &scope);
                        if let ScriptValue::Tuple(values) = rhs.as_ref() {
                            assert_eq!(names.len(), values.len());
                            for (n, v) in names.iter().zip(values) {
                                scope.set_local(Arc::clone(n), Arc::clone(v));
                            }
                        } else {
                            panic!("Expected tuple, found: {rhs}");
                        }
                    }
                },
                AstNode::Function { name, fun, .. } => {
                    scope
                        .functions
                        .insert(Arc::clone(name), (Arc::clone(fun), scope.clone()));
                }
                AstNode::Rec(rec) => {
                    scope.records.insert(Arc::clone(&rec.name), Arc::clone(rec));
                }
                AstNode::Enum(rec) => {
                    scope.enums.insert(Arc::clone(&rec.name), Arc::clone(rec));
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
                                scope.set_local(Arc::clone(ident), Arc::clone(item));
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
                                scope
                                    .set_local(Arc::clone(ident), Arc::new(ScriptValue::Number(v)));
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

    fn eval_expr(&self, expr: &Expression, scope: &Scope) -> Arc<ScriptValue> {
        match &expr.kind {
            ExpressionKind::String(s) => Arc::new(ScriptValue::String(Arc::clone(s))),
            ExpressionKind::StringInterpolate(parts) => {
                // TODO Lazy evaluation (StringInterpolate ScriptValue variant with scope)
                let mut builder = String::new();
                for expr in parts {
                    let val = self.eval_expr(expr, scope);
                    write!(builder, "{val}").unwrap();
                }
                Arc::new(ScriptValue::String(builder.into()))
            }
            ExpressionKind::Number(n) => Arc::new(ScriptValue::Number(*n)),
            ExpressionKind::True => Arc::new(ScriptValue::Boolean(true)),
            ExpressionKind::False => Arc::new(ScriptValue::Boolean(false)),
            ExpressionKind::List(s) => Arc::new(ScriptValue::List(
                s.iter().map(|i| self.eval_expr(i, scope)).collect(),
            )),
            ExpressionKind::Tuple(s) => Arc::new(ScriptValue::Tuple(
                s.iter().map(|i| self.eval_expr(i, scope)).collect(),
            )),
            ExpressionKind::Ref(ident) => match scope.locals.get(ident) {
                None => panic!("Undefined reference: {ident}"),
                Some(value) => Arc::clone(value),
            },
            ExpressionKind::PrefixedName(prefix, name) => match scope.enums.get(prefix) {
                Some(v) => {
                    if let Some((index, _variant)) =
                        v.variants.iter().enumerate().find(|(_, v)| v.name == *name)
                    {
                        Arc::new(ScriptValue::Enum {
                            def: Arc::clone(v),
                            index,
                            values: Default::default(),
                        })
                    } else {
                        panic!("Enum variant not found: {name} in {prefix}");
                    }
                }
                None => panic!("Enum not found: {prefix}"),
            },
            ExpressionKind::Access { subject, key } => {
                let subject = self.eval_expr(subject, scope);
                match subject.as_ref() {
                    ScriptValue::Rec { rec, values } => {
                        let index = rec.params.iter().position(|p| p.name == *key);
                        match index {
                            None => panic!("Property not found: {key:?}"),
                            Some(index) => Arc::clone(&values[index]),
                        }
                    }
                    _ => panic!("Unexpected property access on {subject:?}"),
                }
            }
            ExpressionKind::Not(expr) => {
                let val = self.eval_expr(expr, scope);
                if let ScriptValue::Boolean(b) = *val {
                    Arc::new(ScriptValue::Boolean(!b))
                } else {
                    panic!("Not a boolean")
                }
            }
            ExpressionKind::Equal(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                Arc::new(ScriptValue::Boolean(lhs.eq(&rhs)))
            }
            ExpressionKind::NotEqual(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                Arc::new(ScriptValue::Boolean(!lhs.eq(&rhs)))
            }
            ExpressionKind::Range(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Range(*lhs, *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Addition(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs + *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Subtraction(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs - *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Multiplication(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs * *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Division(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs / *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Call { subject, arguments } => {
                self.eval_call(subject, arguments, scope)
            }
        }
    }

    fn eval_call(
        &self,
        subject: &Expression,
        arguments: &Arguments,
        scope: &Scope,
    ) -> Arc<ScriptValue> {
        let arguments = self.eval_args(arguments, scope);
        match &subject.kind {
            ExpressionKind::Ref(name) => match name.as_ref() {
                "state" => {
                    let arg = Arc::clone(arguments.args.first().expect("state arg"));
                    Arc::new(ScriptValue::State(RwLock::new(arg)))
                }
                _ => {
                    if let Some((fun, captured_scope)) = scope.functions.get(name) {
                        let mut inner_scope = captured_scope.clone();

                        let values = self.apply_args_to_params(&fun.params, arguments);
                        for (ident, val) in fun.params.iter().zip(values) {
                            inner_scope.set_local(ident.name.clone(), val);
                        }

                        match self.eval_block(&fun.body, inner_scope) {
                            Completion::EndOfBlock => Arc::new(ScriptValue::identity()),
                            Completion::ExplicitReturn(v) => v,
                            Completion::ImpliedReturn(v) => v,
                        }
                    } else if let Some(rec) = scope.records.get(name) {
                        let values = self.apply_args_to_params(&rec.params, arguments);

                        let instance = ScriptValue::Rec {
                            rec: Arc::clone(rec),
                            values,
                        };

                        Arc::new(instance)
                    } else if let Some(func) = self.globals.get(name) {
                        let values = arguments.args;
                        let mut func = func.lock().unwrap();
                        let ret = func(&values);
                        Arc::new(ret)
                    } else {
                        panic!("Undefined function: {subject:?}")
                    }
                }
            },
            ExpressionKind::PrefixedName(prefix, name) => {
                if let Some(v) = scope.enums.get(prefix) {
                    if let Some((index, variant)) =
                        v.variants.iter().enumerate().find(|(_, v)| v.name == *name)
                    {
                        // XXX Clunky and unnatural to just ignore kwargs here
                        let values = arguments.args;

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
            ExpressionKind::Access { subject, key } => {
                let subject = self.eval_expr(subject, scope);
                match (subject.as_ref(), key.as_ref()) {
                    (ScriptValue::State(state), "get") => {
                        let v = state.read().unwrap();
                        Arc::clone(v.deref())
                    }
                    (ScriptValue::State(state), "set") => {
                        if let Some(val) = arguments.args.first() {
                            let mut v = state.write().unwrap();
                            *v = Arc::clone(val);
                        }
                        Arc::new(ScriptValue::identity())
                    }
                    (ScriptValue::List(list), "push") => {
                        // This is the Copy on Write feature of the language in play.
                        // Can we avoid copy, if we see that the original will not be used again?
                        let mut res = list.clone();
                        for value in &arguments.args {
                            res.push(Arc::clone(value));
                        }
                        Arc::new(ScriptValue::List(res))
                    }
                    (ScriptValue::Rec { rec, values }, "with") => {
                        let mut new_values = values.clone();

                        // XXX Should be possible to use "apply" here as well

                        for (param, v) in rec.params.iter().zip(new_values.iter_mut()) {
                            if let Some((_, value)) =
                                arguments.kwargs.iter().find(|(k, _)| *k == param.name)
                            {
                                *v = Arc::clone(value);
                            }
                        }

                        Arc::new(ScriptValue::Rec {
                            rec: Arc::clone(rec),
                            values: new_values,
                        })
                    }
                    _ => panic!("Unknown method: {key} on {subject:?}"),
                }
            }
            _ => panic!("Call on something that's not a ref"),
        }
    }

    fn eval_args(&self, arguments: &Arguments, scope: &Scope) -> EvaluatedArguments {
        let args: Vec<_> = arguments
            .args
            .iter()
            .map(|a| self.eval_expr(a, scope))
            .collect();

        let kwargs: Vec<_> = arguments
            .kwargs
            .iter()
            .map(|(k, a)| (Arc::clone(k), self.eval_expr(a, scope)))
            .collect();

        EvaluatedArguments { args, kwargs }
    }

    fn apply_args_to_params(
        &self,
        params: &[Parameter],
        arguments: EvaluatedArguments,
    ) -> Vec<Arc<ScriptValue>> {
        let mut values = arguments.args;
        for param in &params[values.len()..] {
            if let Some((_, value)) = arguments
                .kwargs
                .iter()
                .find(|(name, _)| *name == param.name)
            {
                values.push(Arc::clone(value));
            } else {
                panic!("Missing argument: {}", param.name);
            }
        }

        values
    }
}
