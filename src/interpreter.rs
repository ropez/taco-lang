use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter, Write as _},
    result,
    sync::Arc,
};

use crate::{
    error::{ScriptError, ScriptErrorKind},
    fmt::{fmt_inner_list, fmt_tuple},
    ident::{Ident, global},
    lexer::Src,
    parser::{
        Assignee, CallExpression, Enumeration, Expression, Function, MatchArm, MatchPattern,
        ParamExpression, Record, Statement, TypeExpression,
    },
    stdlib::{ExternalValue, NativeFunctionRef, NativeMethodRef, list::List, parse::ParseFunc},
    validate::ExternalType,
};

#[derive(Debug, Clone)]
pub enum ScriptValue {
    Boolean(bool),
    String(Arc<str>),
    Int(i64),
    Range(i64, i64),
    List(Arc<List>),
    Tuple(Arc<Tuple>),

    NaN,

    // The "opt" type is only checked in validation. During evaluation, it's equvalent to it's
    // inner value, or the special 'None' value.
    None,

    // The *instance* of a record. Not the record itself (which is a callable)
    Rec {
        def: Arc<Record>,
        value: Arc<Tuple>,
    },

    Enum {
        def: Arc<Enumeration>,
        index: usize,
        value: Arc<Tuple>,
    },

    ScriptFunction {
        function: Arc<Function>,
        captured_scope: Arc<Scope>, // XXX Fix warning with dedicated struct type
    },
    Record(Arc<Record>),
    EnumVariant {
        def: Arc<Enumeration>,
        index: usize,
    },
    NativeFunction(NativeFunctionRef),
    NativeMethodBound(NativeMethodRef, Box<ScriptValue>),

    Ext(External),
}

impl ScriptValue {
    pub fn identity() -> Self {
        Self::Tuple(Arc::new(Tuple::identity()))
    }

    pub fn is_nan(&self) -> bool {
        matches!(self, Self::NaN)
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn as_int(&self) -> Result<i64> {
        match self {
            Self::Int(num) => Ok(*num),
            _ => Err(ScriptError::panic(format!(
                "Expected integer, found {self}"
            ))),
        }
    }

    pub fn as_boolean(&self) -> Result<bool> {
        match self {
            Self::Boolean(b) => Ok(*b),
            _ => Err(ScriptError::panic(format!(
                "Expected boolean, found {self}"
            ))),
        }
    }

    pub fn as_string(&self) -> Result<Arc<str>> {
        match self {
            Self::String(s) => Ok(Arc::clone(s)),
            _ => Err(ScriptError::panic(format!("Expected string, found {self}"))),
        }
    }

    pub fn as_tuple(&self) -> Option<Arc<Tuple>> {
        match self {
            Self::Tuple(tuple) => Some(Arc::clone(tuple)),
            Self::Rec { value, .. } => Some(Arc::clone(value)),
            _ => None,
        }
    }

    pub fn as_iterable(&self) -> Vec<ScriptValue> {
        match self {
            Self::List(list) => Vec::from(list.items()),
            Self::Range(l, r) => (*l..=*r).map(ScriptValue::Int).collect(),
            _ => panic!("Expected iterable, found {self}"),
        }
    }

    pub fn to_single_argument(&self) -> Tuple {
        Tuple(vec![TupleItem::unnamed(self.clone())])
    }
}

impl PartialEq for ScriptValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l), Self::String(r)) => l == r,
            (Self::Boolean(l), Self::Boolean(r)) => l == r,
            (Self::Int(l), Self::Int(r)) => l == r,
            (Self::Tuple(l), Self::Tuple(r)) => l == r,
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
            (Self::List(l), Self::List(r)) => l.items() == r.items(),
            (Self::Rec { def: ld, value: lv }, Self::Rec { def: rd, value: rv }) => {
                Arc::ptr_eq(ld, rd) && *lv == *rv
            }
            (Self::None, Self::None) => true,
            (Self::None, _) | (_, Self::None) => false,
            _ => todo!("Equality for {self:?}"),
        }
    }
}

impl Display for ScriptValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ScriptValue::None => write!(f, "<no value>"),
            ScriptValue::String(s) => write!(f, "{s}"),
            ScriptValue::Int(n) => write!(f, "{n}"),
            ScriptValue::Boolean(b) => match b {
                true => write!(f, "true"),
                false => write!(f, "false"),
            },
            ScriptValue::Range(l, r) => write!(f, "{l}..{r}"),
            ScriptValue::Tuple(t) => write!(f, "{t}"),
            ScriptValue::List(list) => {
                write!(f, "[")?;
                fmt_inner_list(f, list.items())?;
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
    pub name: Option<Ident>,
    pub value: ScriptValue,
}

impl TupleItem {
    pub fn new(name: Option<Ident>, value: impl Into<ScriptValue>) -> Self {
        Self {
            name,
            value: value.into(),
        }
    }

    pub fn named(name: Ident, value: impl Into<ScriptValue>) -> Self {
        Self::new(Some(name), value)
    }

    pub fn unnamed(value: impl Into<ScriptValue>) -> Self {
        Self::new(None, value)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Tuple(Vec<TupleItem>);

impl Tuple {
    pub const fn identity() -> Self {
        Self(Vec::new())
    }

    pub fn new(items: Vec<TupleItem>) -> Self {
        Self(items)
    }

    pub fn items(&self) -> &[TupleItem] {
        &self.0
    }

    pub fn mut_items(&mut self) -> &mut Vec<TupleItem> {
        &mut self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn first(&self) -> Option<&ScriptValue> {
        self.0.first().map(|item| &item.value)
    }

    pub fn get_named_item(&self, key: &Ident) -> Option<&TupleItem> {
        self.0.iter().find(|i| i.name.as_ref() == Some(key))
    }

    pub fn at(&self, index: usize) -> Option<&ScriptValue> {
        self.0.get(index).map(|item| &item.value)
    }

    pub fn single(&self) -> Result<&ScriptValue> {
        self.0
            .iter()
            .find(|i| i.name.is_none())
            .map(|item| &item.value)
            .ok_or_else(|| ScriptError::panic("Expected argument"))
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_tuple(f, self.0.iter().map(|a| (a.name.clone(), &a.value)))
    }
}

#[derive(Debug)]
pub enum Completion {
    EndOfBlock(Scope),
    ExplicitReturn(ScriptValue),
    ImpliedReturn(ScriptValue),
}

#[derive(Debug, Clone)]
pub struct External {
    pub(crate) typ: Arc<ExternalType>,
    pub(crate) value: Arc<dyn ExternalValue + Send + Sync>,
}

impl External {
    pub fn new(typ: Arc<ExternalType>, value: Arc<dyn ExternalValue + Send + Sync>) -> Self {
        Self { typ, value }
    }

    pub fn downcast_ref<T: 'static>(&self) -> Option<&T> {
        self.value.as_any().downcast_ref::<T>()
    }
}

type Result<T> = result::Result<T, ScriptError>;

#[derive(Default, Clone)]
pub(crate) struct Scope {
    locals: HashMap<Ident, ScriptValue>,

    // XXX Use some "types" like in validation?
    records: HashMap<Ident, Arc<Record>>,
    enums: HashMap<Ident, Arc<Enumeration>>,

    arguments: Arc<Tuple>,
}

impl Scope {
    fn set_local(&mut self, name: Ident, value: ScriptValue) {
        // Make sure we never assign a value to '_'
        if name.as_str() != "_" {
            self.locals.insert(name, value);
        }
    }

    fn with_locals(&self, locals: HashMap<Ident, ScriptValue>) -> Self {
        let mut scope = self.clone();
        scope.locals.extend(locals);
        scope
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope").finish()
    }
}

#[derive(Default)]
pub struct Interpreter {
    globals: HashMap<Ident, ScriptValue>,

    methods: HashMap<(Ident, Ident), NativeMethodRef>,
}

impl Interpreter {
    pub(crate) fn with_functions(self, functions: HashMap<Ident, NativeFunctionRef>) -> Self {
        let mut globals = self.globals;
        for (name, f) in functions {
            globals.insert(name, ScriptValue::NativeFunction(f));
        }
        Self { globals, ..self }
    }

    pub(crate) fn with_methods(self, more: HashMap<(Ident, Ident), NativeMethodRef>) -> Self {
        let mut methods = self.methods;
        methods.extend(more);

        Self { methods, ..self }
    }

    fn get_method<'a>(
        &'a self,
        subject: &'a ScriptValue,
        name: &Ident,
    ) -> Option<&'a NativeMethodRef> {
        let ns = match subject {
            ScriptValue::Int(_) => global::INT.into(),
            ScriptValue::String(_) => global::STRING.into(),
            ScriptValue::Range(_, _) => global::RANGE.into(),
            ScriptValue::List(_) => global::LIST.into(),
            ScriptValue::Tuple(_) => global::TUPLE.into(),
            ScriptValue::Rec { .. } => global::REC.into(),
            ScriptValue::Ext(ext) => return ext.typ.get_method(name),
            _ => todo!("NS for {subject}"),
        };
        self.methods.get(&(ns, name.clone()))
    }

    pub fn execute(&self, ast: &[Statement]) -> Result<HashMap<Ident, ScriptValue>> {
        let mut scope = Scope::default();
        scope.locals.extend(self.globals.clone());
        let end = self.execute_block(ast, scope)?;

        Ok(match end {
            Completion::EndOfBlock(scope) => scope.locals,
            _ => Default::default(),
        })
    }

    fn execute_block(&self, ast: &[Statement], mut scope: Scope) -> Result<Completion> {
        for node in ast {
            match node {
                Statement::Assignment { assignee, value } => {
                    let rhs = self.eval_expr(value, &scope)?;
                    eval_assignment(assignee, &rhs, &mut scope);
                }
                Statement::Function { name, fun, .. } => {
                    let fun = ScriptValue::ScriptFunction {
                        function: Arc::clone(fun),
                        captured_scope: Arc::new(scope.clone()),
                    };
                    scope.set_local(name.clone(), fun);
                }
                Statement::Rec(rec) => {
                    scope.records.insert(rec.name.clone(), Arc::clone(rec));
                }
                Statement::Enum(rec) => {
                    scope.enums.insert(rec.name.clone(), Arc::clone(rec));
                }
                Statement::Iteration {
                    ident,
                    iterable,
                    body,
                } => {
                    let iterable = self.eval_expr(iterable, &scope)?;
                    match iterable {
                        ScriptValue::List(ref list) => {
                            for item in list.items() {
                                let mut scope = scope.clone();
                                scope.set_local(ident.clone(), item.clone());
                                if let Completion::ExplicitReturn(val) =
                                    self.execute_block(body, scope)?
                                {
                                    return Ok(Completion::ExplicitReturn(val));
                                }
                            }
                        }
                        ScriptValue::Range(lhs, rhs) => {
                            for v in lhs..=rhs {
                                let mut scope = scope.clone();
                                scope.set_local(ident.clone(), ScriptValue::Int(v));
                                if let Completion::ExplicitReturn(val) =
                                    self.execute_block(body, scope)?
                                {
                                    return Ok(Completion::ExplicitReturn(val));
                                }
                            }
                        }
                        _ => panic!("Expected iterable, found: {iterable}"),
                    }
                }
                Statement::IfIn {
                    assignee,
                    value,
                    body,
                    else_body,
                } => {
                    let val = self.eval_expr(value, &scope)?;

                    let (branch, scope) = if let ScriptValue::None = val {
                        (else_body.as_ref(), scope.clone())
                    } else {
                        let mut inner_scope = scope.clone();
                        eval_assignment(assignee, &val, &mut inner_scope);
                        (Some(body), inner_scope)
                    };

                    if let Some(branch) = branch {
                        match self.execute_block(branch, scope)? {
                            Completion::ExplicitReturn(val) => {
                                return Ok(Completion::ExplicitReturn(val));
                            }
                            Completion::ImpliedReturn(val) if ast.len() == 1 => {
                                return Ok(Completion::ImpliedReturn(val));
                            }
                            _ => (),
                        }
                    }
                }
                Statement::Condition {
                    cond,
                    body,
                    else_body,
                } => {
                    let val = self.eval_expr(cond, &scope)?;
                    let ScriptValue::Boolean(val) = val else {
                        panic!("Not a boolean");
                    };

                    let branch = if val { Some(body) } else { else_body.as_ref() };
                    if let Some(block) = branch {
                        let mut scope = scope.clone();
                        match self.execute_block(block, scope)? {
                            Completion::ExplicitReturn(val) => {
                                return Ok(Completion::ExplicitReturn(val));
                            }
                            Completion::ImpliedReturn(val) if ast.len() == 1 => {
                                return Ok(Completion::ImpliedReturn(val));
                            }
                            _ => (),
                        }
                    }
                }
                Statement::Expression(expr) => {
                    let val = self.eval_expr(expr, &scope)?;

                    // Implied return if and only if the block consist of exactly one expression
                    if ast.len() == 1 {
                        return Ok(Completion::ImpliedReturn(val));
                    }
                }
                Statement::Return(expr) => {
                    if let Some(expr) = expr {
                        let val = self.eval_expr(expr, &scope)?;
                        return Ok(Completion::ExplicitReturn(val));
                    } else {
                        return Ok(Completion::ExplicitReturn(ScriptValue::None));
                    }
                }
                Statement::Assert(expr) => {
                    self.eval_assert_expr(expr, &scope)?;
                }
            }
        }

        Ok(Completion::EndOfBlock(scope))
    }

    fn eval_assert_expr(&self, expr: &Src<Expression>, scope: &Scope) -> Result<()> {
        match expr.as_ref() {
            Expression::Equal(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope)?;
                let rhs = self.eval_expr(rhs, scope)?;

                // TODO Smart handling of complex type equality (which list item is different etc)

                if lhs.eq(&rhs) {
                    Ok(())
                } else {
                    Err(ScriptError::new(ScriptErrorKind::AssertionFailed(format!(
                        "\n        + {lhs} == {rhs}"
                    )))
                    .at(expr.loc))
                }
            }
            Expression::NotEqual(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope)?;
                let rhs = self.eval_expr(rhs, scope)?;

                if !lhs.eq(&rhs) {
                    Ok(())
                } else {
                    Err(ScriptError::new(ScriptErrorKind::AssertionFailed(format!(
                        "\n        + {lhs} != {rhs}"
                    )))
                    .at(expr.loc))
                }
            }
            _ => {
                let val = self.eval_expr(expr, scope)?;
                if val.as_boolean()? {
                    Ok(())
                } else {
                    Err(ScriptError::new(ScriptErrorKind::AssertionFailed("".into())).at(expr.loc))
                }
            }
        }
    }

    fn eval_expr(&self, expr: &Src<Expression>, scope: &Scope) -> Result<ScriptValue> {
        let value = match expr.as_ref() {
            Expression::Str(s) => ScriptValue::String(Arc::clone(s)),
            Expression::String(parts) => {
                // TODO Lazy evaluation (StringInterpolate ScriptValue variant with scope)
                let mut builder = String::new();
                for (expr, _) in parts {
                    let val = self.eval_expr(expr, scope)?;
                    write!(builder, "{val}").unwrap();
                }
                ScriptValue::String(builder.into())
            }
            Expression::Int(n) => ScriptValue::Int(*n),
            Expression::True => ScriptValue::Boolean(true),
            Expression::False => ScriptValue::Boolean(false),
            Expression::Arguments => ScriptValue::Tuple(Arc::clone(&scope.arguments)),
            Expression::List(s) => {
                let values = s
                    .iter()
                    .map(|i| self.eval_expr(i, scope))
                    .collect::<Result<Vec<_>>>()?;
                ScriptValue::List(Arc::new(List::new(values)))
            }
            Expression::Tuple(s) => {
                let items = s
                    .iter()
                    .map(|arg| {
                        let value = self.eval_expr(&arg.expr, scope)?;
                        Ok(TupleItem::new(arg.name.clone(), value))
                    })
                    .collect::<Result<Vec<_>>>()?;

                ScriptValue::Tuple(Arc::new(Tuple(items)))
            }
            Expression::Ref(ident) => {
                if let Some(value) = scope.locals.get(ident) {
                    value.clone()
                } else if let Some(typ) = scope.records.get(ident) {
                    ScriptValue::Record(Arc::clone(typ))
                } else {
                    panic!("Undefined reference: {ident}")
                }
            }
            Expression::PrefixedName(prefix, name) => {
                if let Some(v) = scope.records.get(prefix) {
                    match name.as_str() {
                        "parse" => {
                            let func = ParseFunc::new(Arc::clone(v));
                            ScriptValue::NativeFunction(NativeFunctionRef::from(func))
                        }
                        _ => panic!("Unexpected expression {prefix}::{name}"),
                    }
                } else if let Some(v) = scope.enums.get(prefix) {
                    if let Some((index, variant)) =
                        v.variants.iter().enumerate().find(|(_, v)| v.name == *name)
                    {
                        if variant.params.is_none() {
                            ScriptValue::Enum {
                                def: Arc::clone(v),
                                index,
                                value: Arc::new(Tuple::identity()),
                            }
                        } else {
                            ScriptValue::EnumVariant {
                                def: Arc::clone(v),
                                index,
                            }
                        }
                    } else {
                        panic!("Enum variant not found: {name} in {prefix}");
                    }
                } else {
                    // XXX Little bit hackish to re-combine the full name like this
                    let full_ident = format!("{prefix}::{name}").into();
                    if let Some(value) = scope.locals.get(&full_ident) {
                        value.clone()
                    } else {
                        panic!("Enum not found: {prefix}")
                    }
                }
            }
            Expression::Access { subject, key } => {
                let subject = self.eval_expr(subject, scope)?;

                if let Some(tuple) = subject.as_tuple()
                    && let Some(it) = tuple.get_named_item(key)
                {
                    return Ok(it.value.clone());
                }

                if let Some(method) = self.get_method(&subject, key) {
                    ScriptValue::NativeMethodBound(method.clone(), subject.into())
                } else {
                    panic!("No such attribute {key} for {subject}");
                }
            }
            Expression::Function(fun) => ScriptValue::ScriptFunction {
                function: Arc::clone(fun),
                captured_scope: Arc::new(scope.clone()),
            },
            Expression::Not(expr) => {
                let val = self.eval_expr(expr, scope)?;
                if let ScriptValue::Boolean(b) = val {
                    ScriptValue::Boolean(!b)
                } else {
                    panic!("Not a boolean")
                }
            }
            Expression::Equal(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope)?;
                let rhs = self.eval_expr(rhs, scope)?;

                ScriptValue::Boolean(lhs.eq(&rhs))
            }
            Expression::NotEqual(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope)?;
                let rhs = self.eval_expr(rhs, scope)?;

                ScriptValue::Boolean(!lhs.eq(&rhs))
            }
            Expression::Range(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope)?.as_int()?;
                let rhs = self.eval_expr(rhs, scope)?.as_int()?;

                ScriptValue::Range(lhs, rhs)
            }
            Expression::Negate(expr) => {
                let val = self.eval_expr(expr, scope)?;
                match val {
                    ScriptValue::Int(n) => ScriptValue::Int(-n),
                    ScriptValue::NaN => val,
                    _ => panic!("Expected number"),
                }
            }
            Expression::Addition(lhs, rhs) => {
                self.eval_arithmetic(i64::checked_add, lhs, rhs, scope)?
            }
            Expression::Subtraction(lhs, rhs) => {
                self.eval_arithmetic(i64::checked_sub, lhs, rhs, scope)?
            }
            Expression::Multiplication(lhs, rhs) => {
                self.eval_arithmetic(i64::checked_mul, lhs, rhs, scope)?
            }
            Expression::Division(lhs, rhs) => {
                self.eval_arithmetic(i64::checked_div, lhs, rhs, scope)?
            }
            Expression::Modulo(lhs, rhs) => {
                self.eval_arithmetic(i64::checked_rem_euclid, lhs, rhs, scope)?
            }
            Expression::LessThan(lhs, rhs) => {
                self.eval_comparison(|a, b| a < b, lhs, rhs, scope)?
            }
            Expression::GreaterThan(lhs, rhs) => {
                self.eval_comparison(|a, b| a > b, lhs, rhs, scope)?
            }
            Expression::LessOrEqual(lhs, rhs) => {
                self.eval_comparison(|a, b| a <= b, lhs, rhs, scope)?
            }
            Expression::GreaterOrEqual(lhs, rhs) => {
                self.eval_comparison(|a, b| a >= b, lhs, rhs, scope)?
            }
            Expression::Try(inner) => {
                let val = self.eval_expr(inner, scope)?;
                if let ScriptValue::None = val {
                    return Err(ScriptError::no_value());
                }
                val
            }
            Expression::Call { subject, arguments } => {
                let subject = self.eval_expr(subject, scope)?;
                let arguments = self.eval_args(arguments, scope)?;
                try_save(self.eval_callable(subject, &arguments)).map_err(|err| err.at(expr.loc))?
            }
            Expression::Match { expr, arms, is_opt } => {
                self.eval_match(expr, arms, *is_opt, scope)?
            }
        };

        Ok(value)
    }

    fn eval_arithmetic<F>(
        &self,
        op: F,
        lhs: &Src<Expression>,
        rhs: &Src<Expression>,
        scope: &Scope,
    ) -> Result<ScriptValue>
    where
        F: FnOnce(i64, i64) -> Option<i64>,
    {
        let lhs = self.eval_expr(lhs, scope)?;
        let rhs = self.eval_expr(rhs, scope)?;
        if lhs.is_nan() || rhs.is_nan() {
            Ok(ScriptValue::NaN)
        } else {
            Ok(op(lhs.as_int()?, rhs.as_int()?)
                .map(ScriptValue::Int)
                .unwrap_or(ScriptValue::NaN))
        }
    }

    fn eval_comparison<F>(
        &self,
        op: F,
        lhs: &Src<Expression>,
        rhs: &Src<Expression>,
        scope: &Scope,
    ) -> Result<ScriptValue>
    where
        F: FnOnce(i64, i64) -> bool,
    {
        let lhs = self.eval_expr(lhs, scope)?;
        let rhs = self.eval_expr(rhs, scope)?;
        match (lhs, rhs) {
            (ScriptValue::Int(lhs), ScriptValue::Int(rhs)) => {
                Ok(ScriptValue::Boolean(op(lhs, rhs)))
            }
            (ScriptValue::NaN, ScriptValue::Int(_)) => Ok(ScriptValue::NaN),
            (ScriptValue::Int(_), ScriptValue::NaN) => Ok(ScriptValue::NaN),
            _ => panic!("Expected numbers"),
        }
    }

    pub(crate) fn eval_callable(
        &self,
        callable: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue> {
        let return_value = match callable {
            ScriptValue::ScriptFunction {
                function,
                captured_scope,
            } => {
                let mut inner_scope = Scope::clone(&captured_scope);
                let values = transform_args(&function.params, arguments);
                for item in &values.0 {
                    if let Some(name) = &item.name {
                        inner_scope.set_local(name.clone(), item.value.clone());
                    }
                }
                inner_scope.arguments = Arc::new(values);

                match self.execute_block(&function.body, inner_scope)? {
                    Completion::EndOfBlock(_) => ScriptValue::None,
                    Completion::ExplicitReturn(v) => v,
                    Completion::ImpliedReturn(v) => v,
                }
            }
            ScriptValue::Record(rec) => {
                let values = transform_args(&rec.params, arguments);
                ScriptValue::Rec {
                    def: Arc::clone(&rec),
                    value: Arc::new(values),
                }
            }
            ScriptValue::EnumVariant { def, index } => {
                let variant = &def.variants[index];
                // XXX Should be non-option
                let params = variant.params.as_ref().unwrap();
                let values = transform_args(params, arguments);
                ScriptValue::Enum {
                    def: Arc::clone(&def),
                    index,
                    value: Arc::new(values),
                }
            }
            // XXX Arguments aren't "transformed" for native calls, but passed as-is!
            // E.g. fun(a: int, b: int), called as fun(b: 2, 1), receives (b: 2, 1), not (a: 1, b: 2)
            // RecordWith kind-of works by accident for this reason.
            ScriptValue::NativeFunction(func) => func.call(self, arguments)?,
            ScriptValue::NativeMethodBound(method, subject) => {
                method.call(self, *subject, arguments)?
            }
            _ => panic!("Expected a callable, got: {callable:?}"),
        };

        Ok(return_value)
    }

    fn eval_args(&self, arguments: &CallExpression, scope: &Scope) -> Result<Arc<Tuple>> {
        let tuple = match arguments {
            CallExpression::Inline(arguments) => {
                let items: Vec<_> = arguments
                    .iter()
                    .map(|a| {
                        let val = self.eval_expr(&a.expr, scope)?;
                        Ok(TupleItem::new(a.name.clone(), val))
                    })
                    .collect::<Result<Vec<_>>>()?;

                Arc::new(Tuple(items))
            }
            CallExpression::Destructure(expr) => {
                let arg = self.eval_expr(expr, scope)?;
                match &arg {
                    ScriptValue::Tuple(tuple) => Arc::clone(tuple),
                    ScriptValue::Rec { value: values, .. } => Arc::clone(values),
                    _ => panic!("Expected a tuple, found {arg}"),
                }
            }
            CallExpression::DestructureImplicit(_) => Arc::clone(&scope.arguments),
        };

        Ok(tuple)
    }

    fn eval_match(
        &self,
        expr: &Src<Expression>,
        arms: &Vec<MatchArm>,
        is_opt: bool,
        scope: &Scope,
    ) -> Result<ScriptValue> {
        let value = self.eval_expr(expr, scope)?;
        for arm in arms {
            if let Some(locals) = self.eval_match_pattern(&arm.pattern, &value, scope)? {
                let ret = self.eval_expr(&arm.expr, &scope.with_locals(locals))?;
                return Ok(ret);
            }
        }

        if is_opt {
            Ok(ScriptValue::None)
        } else {
            Err(ScriptError::panic("No match found"))
        }
    }

    fn eval_match_pattern(
        &self,
        pattern: &MatchPattern,
        val: &ScriptValue,
        scope: &Scope,
    ) -> Result<Option<HashMap<Ident, ScriptValue>>> {
        match pattern {
            MatchPattern::Discard => Ok(Some(HashMap::new())),
            MatchPattern::True => {
                if val.as_boolean()? {
                    Ok(Some(HashMap::new()))
                } else {
                    Ok(None)
                }
            }
            MatchPattern::False => {
                if !val.as_boolean()? {
                    Ok(Some(HashMap::new()))
                } else {
                    Ok(None)
                }
            }
            MatchPattern::Int(n) => {
                if *n == val.as_int()? {
                    Ok(Some(HashMap::new()))
                } else {
                    Ok(None)
                }
            }
            MatchPattern::Str(s) => {
                if *s == val.as_string()? {
                    Ok(Some(HashMap::new()))
                } else {
                    Ok(None)
                }
            }
            MatchPattern::Assignee(name) => {
                if val.is_none() {
                    Ok(None)
                } else {
                    let mut locals = HashMap::new();
                    locals.insert(name.clone(), val.clone());
                    Ok(Some(locals))
                }
            }
            MatchPattern::EnumVariant(prefix, name, assignee) => {
                if let ScriptValue::Enum { def, index, .. } = val {
                    let def = {
                        if let Some(ident) = prefix {
                            match scope.enums.get(ident) {
                                Some(e) => Ok(e),
                                None => Err(ScriptError::panic(format!("Enum not found: {ident}"))),
                            }
                        } else {
                            Ok(def)
                        }
                    }?;

                    if let Some((idx, var)) = def.find_variant(name) {
                        if Arc::ptr_eq(def, def) && *index == idx {
                            if let Some(assignee) = assignee {
                                let mut locals = Scope::default();
                                eval_assignment(assignee, val, &mut locals);
                                Ok(Some(locals.locals))
                            } else {
                                Ok(Some(HashMap::new()))
                            }
                        } else {
                            Ok(None)
                        }
                    } else {
                        Err(ScriptError::panic(format!(
                            "Enum variant not found: {name} in {}",
                            def.name
                        )))
                    }
                } else {
                    Err(ScriptError::panic("Not an enum"))
                }
            }
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
fn transform_args(params: &[ParamExpression], args: &Tuple) -> Tuple {
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
            } else if par.is_optional() {
                items.push(TupleItem::named(name, ScriptValue::None));
            } else {
                panic!("Missing argument {name}");
            }
        } else if let Some(arg) = positional.next() {
            let val = transform_value(par, arg);
            items.push(TupleItem::unnamed(val));
        } else if par.is_optional() {
            items.push(TupleItem::unnamed(ScriptValue::None));
        } else {
            panic!("Missing positional argument");
        }
    }

    fn transform_value(par: &ParamExpression, arg: &TupleItem) -> ScriptValue {
        match (&*par.type_expr, &arg.value) {
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
    match (&lhs.name, &lhs.pattern) {
        (None, None) => {}
        (Some(name), None) => scope.set_local(name.clone(), rhs.clone()),
        (_, Some(pattern)) => match rhs {
            ScriptValue::Tuple(value) => eval_destructure(pattern, value, scope),
            ScriptValue::Rec { value, .. } => eval_destructure(pattern, value, scope),
            ScriptValue::Enum { value, .. } => eval_destructure(pattern, value, scope),
            _ => panic!("Expected tuple, found: {rhs}"),
        },
    }
}

fn eval_destructure(lhs: &[Src<Assignee>], rhs: &Tuple, scope: &mut Scope) {
    let mut positional = rhs.0.iter().filter(|arg| arg.name.is_none());
    for par in lhs.iter() {
        if let Some(name) = &par.name {
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

pub(crate) fn try_save(res: Result<ScriptValue>) -> Result<ScriptValue> {
    match res {
        Ok(v) => Ok(v),
        Err(err) => match err.kind {
            ScriptErrorKind::NoValue => Ok(ScriptValue::None),
            _ => Err(err),
        },
    }
}
