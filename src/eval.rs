use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter, Write as _},
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use crate::parser::{
    Assignmee, AstNode, Enumeration, Expression, ExpressionKind, Function, Parameter, Record,
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
            (Self::Enum { def: dl, index: il }, Self::Enum { def: dr, index: ir }) => {
                Arc::ptr_eq(dl, dr) && il == ir
            }
            _ => todo!("Equality for {self:?}"),
        }
    }
}

impl Display for ScriptValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ScriptValue::String(s) => write!(f, "{s}"),
            ScriptValue::Number(n) => write!(f, "{n}"),
            ScriptValue::Tuple(values) => write!(f, "({values:?})"), // XXX
            ScriptValue::Boolean(b) => match b {
                true => write!(f, "true"),
                false => write!(f, "false"),
            },
            ScriptValue::Enum { def, index } => {
                let var = &def.variants[*index];
                write!(f, "{}", var.name)
            }
            _ => todo!("Display impl for {self:?}"),
        }
    }
}

#[derive(Debug)]
pub enum Completion {
    EndOfBlock,
    Return(Arc<ScriptValue>),
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
                            panic!("Expected tuple, found: {rhs:?}");
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
                                self.eval_block(body, scope);
                            }
                        }
                        ScriptValue::Range(lhs, rhs) => {
                            for v in lhs..=rhs {
                                let mut scope = scope.clone();
                                scope
                                    .set_local(Arc::clone(ident), Arc::new(ScriptValue::Number(v)));
                                self.eval_block(body, scope);
                            }
                        }
                        _ => panic!("Expected iterable, found: {iterable:?}"),
                    }
                }
                AstNode::Condition {
                    cond,
                    body,
                    else_body,
                } => {
                    let val = self.eval_expr(cond, &scope);

                    if let ScriptValue::Boolean(b) = *val {
                        if b {
                            let scope = scope.clone();
                            self.eval_block(body, scope);
                        } else if let Some(else_body) = else_body {
                            let scope = scope.clone();
                            self.eval_block(else_body, scope);
                        }
                    } else {
                        panic!("Not a boolean");
                    }
                }
                AstNode::Expression(expr) => {
                    let val = self.eval_expr(expr, &scope);

                    // Implied return if and only if the block consist of exactly one expression
                    // XXX This is probably wrong inside if/else/for, when return is handled
                    if ast.len() == 1 {
                        return Completion::Return(val);
                    }
                }
                AstNode::Return(expr) => {
                    let val = self.eval_expr(expr, &scope);
                    return Completion::Return(val);
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
            ExpressionKind::Call {
                subject,
                args,
                kwargs,
            } => match &subject.kind {
                ExpressionKind::Ref(name) => match name.as_ref() {
                    "state" => {
                        let arg = args.first().expect("state arg");
                        let value = self.eval_expr(arg, scope);
                        Arc::new(ScriptValue::State(RwLock::new(value)))
                    }
                    _ => {
                        if let Some((fun, captured_scope)) = scope.functions.get(name) {
                            let values = self.eval_args(&fun.params, args, kwargs, scope);

                            let mut inner_scope = captured_scope.clone();

                            for (ident, val) in fun.params.iter().zip(values) {
                                inner_scope.set_local(ident.name.clone(), val);
                            }

                            let c = self.eval_block(&fun.body, inner_scope);

                            match c {
                                Completion::EndOfBlock => Arc::new(ScriptValue::identity()),
                                Completion::Return(v) => v,
                            }
                        } else if let Some(rec) = scope.records.get(name) {
                            let values = self.eval_args(&rec.params, args, kwargs, scope);

                            let instance = ScriptValue::Rec {
                                rec: Arc::clone(rec),
                                values,
                            };

                            Arc::new(instance)
                        } else if let Some(func) = self.globals.get(name) {
                            // XXX eval_args like script function
                            let values: Vec<_> =
                                args.iter().map(|e| self.eval_expr(e, scope)).collect();
                            let mut func = func.lock().unwrap();
                            let ret = func(&values);
                            Arc::new(ret)
                        } else {
                            panic!("Undefined function: {subject:?}")
                        }
                    }
                },
                ExpressionKind::Access { subject, key } => {
                    let subject = self.eval_expr(subject, scope);
                    match (subject.as_ref(), key.as_ref()) {
                        (ScriptValue::State(state), "get") => {
                            let v = state.read().unwrap();
                            Arc::clone(v.deref())
                        }
                        (ScriptValue::State(state), "set") => {
                            let val = args.first().expect("get arg");
                            let val = self.eval_expr(val, scope);
                            let mut v = state.write().unwrap();
                            *v = val;
                            Arc::new(ScriptValue::identity())
                        }
                        (ScriptValue::List(list), "push") => {
                            let item = args.first().expect("push item");
                            let value = self.eval_expr(item, scope);
                            // This is the Copy on Write feature of the language in play.
                            // Can we avoid copy, if we see that the original will not be used again?
                            let mut res = list.clone();
                            res.push(value);
                            Arc::new(ScriptValue::List(res))
                        }
                        (ScriptValue::Rec { rec, values }, "with") => {
                            let mut new_values = values.clone();

                            for (param, v) in rec.params.iter().zip(new_values.iter_mut()) {
                                if let Some((_, expr)) =
                                    kwargs.iter().find(|(k, _)| *k == param.name)
                                {
                                    *v = self.eval_expr(expr, scope);
                                }
                            }

                            let instance = ScriptValue::Rec {
                                rec: Arc::clone(rec),
                                values: new_values,
                            };
                            Arc::new(instance)
                        }
                        _ => panic!("Unknown method: {key} on {subject:?}"),
                    }
                }
                _ => panic!("Call on something that's not a ref"),
            },
            // _ => unimplemented!("Expression: {expr:?}"),
        }
    }

    fn eval_args(
        &self,
        params: &Vec<Parameter>,
        arguments: &Vec<Expression>,
        kwargs: &Vec<(Arc<str>, Expression)>,
        scope: &Scope,
    ) -> Vec<Arc<ScriptValue>> {
        // FIXME This should already be checked during type checking phase
        if params.len() != arguments.len() + kwargs.len() {
            panic!(
                "Expected {} arguments, found {}",
                params.len(),
                arguments.len()
            );
        }

        let mut values: Vec<_> = arguments.iter().map(|a| self.eval_expr(a, scope)).collect();

        for param in &params[arguments.len()..] {
            if let Some((_, value)) = kwargs.iter().find(|(name, _)| *name == param.name) {
                values.push(self.eval_expr(value, scope));
            } else {
                panic!("Missing argument: {}", param.name);
            }
        }

        values
    }
}
