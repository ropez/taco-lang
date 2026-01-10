use std::{
    collections::HashMap,
    fmt::{self, Write as _},
    result,
    sync::Arc,
};

use crate::{
    error::{ScriptError, ScriptErrorKind},
    ext::{NativeFunctionRef, NativeMethodRef},
    ident::{Ident, global},
    lexer::Src,
    parser::{Assignee, CallExpression, Expression, Literal, MatchArm, MatchPattern, Statement},
    script_type::{ScriptType, TupleType},
    script_value::{Fallible, ScriptFunction, ScriptValue, Tuple, TupleItem},
    stdlib::{list::List, parse::ParseFunc},
    type_scope::{TypeDefinition, TypeScope, eval_function},
};

#[cfg(feature = "pipe")]
use crate::{
    ext::ReadableExt,
    stdlib::pipe::{PipeImpl, PipeType, Tracker, exec_pipe},
};

#[derive(Debug)]
enum Completion {
    EndOfBlock(Scope),
    ExplicitReturn(ScriptValue),
    ImpliedReturn(ScriptValue),

    // TODO: See if we can implement the 'Try' trait, and simplify calls
    Break,
    Continue,
}

type Result<T> = result::Result<T, ScriptError>;

#[derive(Default, Clone)]
pub(crate) struct Scope {
    locals: HashMap<Ident, ScriptValue>,

    types: TypeScope,

    arguments: Arc<Tuple>,
}

impl Scope {
    fn set_local(&mut self, name: impl Into<Ident>, value: ScriptValue) {
        // Make sure we never assign a value to '_'
        let key = name.into();
        if key.as_str() != "_" {
            self.locals.insert(key, value);
        }
    }

    fn with_locals(&self, locals: HashMap<Ident, ScriptValue>) -> Self {
        let mut scope = self.clone();
        scope.locals.extend(locals);
        scope
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope").finish()
    }
}

#[derive(Clone, Default)]
pub struct Interpreter {
    globals: HashMap<Ident, ScriptValue>,

    methods: HashMap<(Ident, Ident), NativeMethodRef>,

    #[cfg(feature = "pipe")]
    pub(crate) tracker: Tracker,
}

impl Interpreter {
    pub(crate) fn with_globals<T>(self, iter: T) -> Self
    where
        T: IntoIterator<Item = (Ident, ScriptValue)>,
    {
        let mut globals = self.globals;
        globals.extend(iter);
        Self { globals, ..self }
    }

    pub(crate) fn with_methods(self, more: HashMap<(Ident, Ident), NativeMethodRef>) -> Self {
        let mut methods = self.methods;
        methods.extend(more);

        Self { methods, ..self }
    }

    fn get_method(&self, subject: &ScriptValue, name: &Ident) -> Option<NativeMethodRef> {
        let ns = match subject {
            ScriptValue::Int(_) => global::INT.into(),
            ScriptValue::String { .. } => global::STRING.into(),
            ScriptValue::Range(_, _) => global::RANGE.into(),
            ScriptValue::List(_) => global::LIST.into(),
            ScriptValue::Tuple(_) => global::TUPLE.into(),
            ScriptValue::Rec { .. } => global::REC.into(),
            ScriptValue::Opt(_) => global::OPT.into(),
            ScriptValue::Fallible(_) => global::FALLIBLE.into(),
            ScriptValue::Ext(typ, _) => return typ.get_method(name),
            _ => todo!("NS for {subject}"),
        };
        self.methods.get(&(ns, name.clone())).cloned()
    }

    pub fn execute(&self, ast: &[Statement]) -> Result<HashMap<Ident, ScriptValue>> {
        let mut scope = Scope::default();

        // During evaluation, we don't really care about type inferrence.
        scope.types = scope.types.with_expected(ScriptType::Unknown);

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
                    let function = eval_function(fun, &scope.types).map_err(ScriptError::panic)?;

                    let fun = ScriptValue::ScriptFunction(ScriptFunction::new(
                        function,
                        Arc::clone(fun),
                        Arc::new(scope.clone()),
                    ));
                    scope.set_local(name, fun);
                }
                Statement::Rec(rec) => {
                    scope.types.eval_rec(rec).map_err(ScriptError::panic)?;
                }
                Statement::Enum(def) => {
                    scope.types.eval_enum(def).map_err(ScriptError::panic)?;
                }
                Statement::Iteration {
                    ident,
                    iterable,
                    body,
                } => {
                    let value = self.eval_expr(iterable, &scope)?;
                    match value {
                        ScriptValue::List(ref list) => {
                            for item in list.items() {
                                let mut scope = scope.clone();
                                scope.set_local(ident, item.clone());
                                match self.execute_block(body, scope)? {
                                    Completion::ExplicitReturn(val) => {
                                        return Ok(Completion::ExplicitReturn(val));
                                    }
                                    Completion::Break => break,
                                    _ => (),
                                }
                            }
                        }
                        ScriptValue::Range(lhs, rhs) => {
                            for v in lhs..=rhs {
                                let mut scope = scope.clone();
                                scope.set_local(ident, ScriptValue::Int(v));
                                match self.execute_block(body, scope)? {
                                    Completion::ExplicitReturn(val) => {
                                        return Ok(Completion::ExplicitReturn(val));
                                    }
                                    Completion::Break => break,
                                    _ => (),
                                }
                            }
                        }
                        #[cfg(feature = "pipe")]
                        ScriptValue::Ext(_, val) => {
                            if let Some(readable) = val.as_readable() {
                                while let Some(v) = readable
                                    .blocking_read_next(self)
                                    .map_err(|err| err.at(iterable.loc))?
                                {
                                    let mut scope = scope.clone();
                                    scope.set_local(ident, v);
                                    if let Completion::ExplicitReturn(val) =
                                        self.execute_block(body, scope)?
                                    {
                                        return Ok(Completion::ExplicitReturn(val));
                                    }
                                }
                            } else {
                                return Err(
                                    ScriptError::panic("Expected iterable").at(iterable.loc)
                                );
                            }
                        }
                        _ => panic!("Expected iterable, found: {value}"),
                    }
                }
                Statement::IfIn {
                    assignee,
                    value,
                    body,
                    else_body,
                } => {
                    let val = self.eval_expr(value, &scope)?;

                    let ScriptValue::Opt(opt) = val else {
                        Err(ScriptError::panic("Expected option"))?
                    };

                    let (branch, scope) = if let Some(val) = opt {
                        let mut inner_scope = scope.clone();
                        eval_assignment(assignee, &val, &mut inner_scope);
                        (Some(body), inner_scope)
                    } else {
                        (else_body.as_ref(), scope.clone())
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
                        match self.execute_block(block, scope.clone())? {
                            Completion::ExplicitReturn(val) => {
                                return Ok(Completion::ExplicitReturn(val));
                            }
                            Completion::ImpliedReturn(val) if ast.len() == 1 => {
                                return Ok(Completion::ImpliedReturn(val));
                            }
                            Completion::Break => return Ok(Completion::Break),
                            Completion::Continue => return Ok(Completion::Continue),
                            _ => (),
                        }
                    }
                }
                Statement::While { cond, body } => {
                    while self.eval_expr(cond, &scope)?.as_boolean()? {
                        let scope = scope.clone();
                        match self.execute_block(body, scope)? {
                            Completion::ExplicitReturn(val) => {
                                return Ok(Completion::ExplicitReturn(val));
                            }
                            Completion::ImpliedReturn(val) if ast.len() == 1 => {
                                return Ok(Completion::ImpliedReturn(val));
                            }
                            Completion::Break => break,
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
                        return Ok(Completion::ExplicitReturn(ScriptValue::opt(None)));
                    }
                }
                Statement::Assert(expr) => {
                    self.eval_assert_expr(expr, &scope)?;
                }
                Statement::Break => {
                    return Ok(Completion::Break);
                }
                Statement::Continue => {
                    return Ok(Completion::Continue);
                }
            }
        }

        Ok(Completion::EndOfBlock(scope))
    }

    fn eval_expr(&self, expr: &Src<Expression>, scope: &Scope) -> Result<ScriptValue> {
        let value = match expr.as_ref() {
            Expression::String(parts) => {
                // TODO Lazy evaluation (StringInterpolate ScriptValue variant with scope)
                let mut builder = String::new();
                for (expr, _) in parts {
                    let val = self.eval_expr(expr, scope)?;
                    write!(builder, "{val}").unwrap();
                }
                ScriptValue::string(builder)
            }
            Expression::Literal(literal) => match literal {
                Literal::True => ScriptValue::Boolean(true),
                Literal::False => ScriptValue::Boolean(false),
                Literal::Int(n) => ScriptValue::Int(*n),
                Literal::Char(c) => ScriptValue::Char(*c),
                Literal::Str(s) => ScriptValue::string(Arc::clone(s)),
            },
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

                ScriptValue::Tuple(Arc::new(Tuple::new(items)))
            }
            Expression::Ref(ident) => {
                if let Some(value) = scope.locals.get(ident) {
                    value.clone()
                } else if let Some(TypeDefinition::RecDefinition(typ)) = scope.types.get(ident) {
                    ScriptValue::Record(Arc::clone(typ))
                } else {
                    panic!("Undefined reference: {ident}")
                }
            }
            Expression::PrefixedName(prefix, name) => {
                match scope.types.get(prefix) {
                    Some(TypeDefinition::RecDefinition(v)) => match name.as_str() {
                        "parse" => {
                            let func = ParseFunc::new(Arc::clone(v));
                            ScriptValue::NativeFunction(NativeFunctionRef::from(func))
                        }
                        _ => panic!("Unexpected expression {prefix}::{name}"),
                    },
                    Some(TypeDefinition::EnumDefinition(v)) => {
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
                    }
                    _ => {
                        // XXX Little bit hackish to re-combine the full name like this
                        let full_ident = format!("{prefix}::{name}").into();
                        if let Some(value) = scope.locals.get(&full_ident) {
                            value.clone()
                        } else {
                            panic!("Enum not found: {prefix}")
                        }
                    }
                }
            }
            Expression::Access { subject, key } => {
                let subject = self.eval_expr(subject, scope)?;

                if let Some(tuple) = subject.as_tuple()
                    && let Some(val) = tuple.get_named(key)
                {
                    return Ok(val.clone());
                }

                if let Some(method) = self.get_method(&subject, key) {
                    ScriptValue::NativeMethodBound(method.clone(), subject.into())
                } else {
                    panic!("No such attribute {key} for {subject}");
                }
            }
            Expression::Function(fun) => {
                let function = eval_function(fun, &scope.types).map_err(ScriptError::panic)?;

                ScriptValue::ScriptFunction(ScriptFunction::new(
                    function,
                    Arc::clone(fun),
                    Arc::new(scope.clone()),
                ))
            }
            Expression::LogicNot(expr) => {
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
            #[cfg(feature = "pipe")]
            Expression::Pipe(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope).map_err(|err| err.at(lhs.loc))?;
                let rhs = self.eval_expr(rhs, scope).map_err(|err| err.at(rhs.loc))?;

                exec_pipe(self, lhs.as_ext()?, rhs.as_ext()?).map_err(|err| err.at(expr.loc))?;

                ScriptValue::Ext(
                    Arc::new(PipeType::new(None, None)), // XXX Type not really used in runtine
                    Arc::new(PipeImpl::new(lhs.as_ext()?, rhs.as_ext()?)),
                )
            }
            #[cfg(not(feature = "pipe"))]
            Expression::Pipe(_lhs, _rhs) => {
                // XXX Fix error
                panic!("pipes are not enabled");
            }
            Expression::LogicAnd(lhs, rhs) => self.eval_logic(|a, b| a && b, lhs, rhs, scope)?,
            Expression::LogicOr(lhs, rhs) => self.eval_logic(|a, b| a || b, lhs, rhs, scope)?,
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
                match val {
                    ScriptValue::Opt(opt) => {
                        if let Some(inner) = opt {
                            ScriptValue::clone(&inner)
                        } else {
                            return Err(ScriptError::no_value());
                        }
                    }
                    ScriptValue::Fallible(f) => match f {
                        Fallible::Ok(inner) => ScriptValue::clone(&inner),
                        Fallible::Err(err) => Err(ScriptError::error(*err))?,
                    },
                    _ => return Err(ScriptError::panic("Invalid question operator").at(expr.loc)),
                }
            }
            Expression::Unwrap(inner) => {
                let val = self.eval_expr(inner, scope)?;
                match val {
                    ScriptValue::Opt(opt) => match opt {
                        Some(inner) => ScriptValue::clone(&inner),
                        None => Err(ScriptError::panic("No value in assertion").at(expr.loc))?,
                    },
                    ScriptValue::Fallible(f) => match f {
                        Fallible::Ok(inner) => ScriptValue::clone(&inner),
                        Fallible::Err(err) => Err(ScriptError::panic(format!(
                            "Unwrap operator (!) on error: {err}"
                        ))
                        .at(expr.loc))?,
                    },
                    _ => Err(ScriptError::panic("Invalid question operator"))?,
                }
            }
            Expression::Coalesce(lhs, rhs) => {
                let val = self.eval_expr(lhs, scope)?;
                match val {
                    ScriptValue::Opt(opt) => {
                        if let Some(inner) = opt {
                            ScriptValue::clone(&inner)
                        } else {
                            self.eval_expr(rhs, scope)?
                        }
                    }
                    _ => return Err(ScriptError::panic("Invalid coalescion operator")),
                }
            }
            Expression::Call { subject, arguments } => {
                let subject = self.eval_expr(subject, scope)?;
                let arguments = self.eval_args(arguments, scope)?;
                self.eval_callable(subject, &arguments)
                    .map_err(|err| err.at(expr.loc))?
            }
            Expression::Match { expr, arms, is_opt } => {
                self.eval_match(expr, arms, *is_opt, scope)?
            }
        };

        Ok(value)
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
            Expression::LessThan(lhs, rhs) => self
                .eval_assert_comparison(|a, b| a < b, "<", lhs, rhs, scope)
                .map_err(|err| err.at(expr.loc)),
            Expression::GreaterThan(lhs, rhs) => self
                .eval_assert_comparison(|a, b| a > b, ">", lhs, rhs, scope)
                .map_err(|err| err.at(expr.loc)),
            Expression::LessOrEqual(lhs, rhs) => self
                .eval_assert_comparison(|a, b| a <= b, "<=", lhs, rhs, scope)
                .map_err(|err| err.at(expr.loc)),
            Expression::GreaterOrEqual(lhs, rhs) => self
                .eval_assert_comparison(|a, b| a >= b, ">=", lhs, rhs, scope)
                .map_err(|err| err.at(expr.loc)),
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

    fn eval_logic<F>(
        &self,
        op: F,
        lhs: &Src<Expression>,
        rhs: &Src<Expression>,
        scope: &Scope,
    ) -> Result<ScriptValue>
    where
        F: FnOnce(bool, bool) -> bool,
    {
        let lhs = self.eval_expr(lhs, scope)?.as_boolean()?;
        let rhs = self.eval_expr(rhs, scope)?.as_boolean()?;
        Ok(ScriptValue::Boolean(op(lhs, rhs)))
    }

    fn eval_assert_comparison<F>(
        &self,
        op: F,
        op_symb: &str,
        lhs: &Src<Expression>,
        rhs: &Src<Expression>,
        scope: &Scope,
    ) -> Result<()>
    where
        F: FnOnce(i64, i64) -> bool,
    {
        let lhs = self.eval_expr(lhs, scope)?;
        let rhs = self.eval_expr(rhs, scope)?;
        match (&lhs, &rhs) {
            (ScriptValue::Int(lhs), ScriptValue::Int(rhs)) if op(*lhs, *rhs) => Ok(()),
            _ => Err(ScriptError::new(ScriptErrorKind::AssertionFailed(format!(
                "\n        + {lhs} {op_symb} {rhs}"
            )))),
        }
    }

    pub(crate) fn eval_callable(
        &self,
        callable: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue> {
        let return_value = match callable {
            ScriptValue::ScriptFunction(f) => {
                let mut inner_scope = f.clone_captured_scope();
                let values = transform_args(&f.function.params, arguments);
                for item in values.items() {
                    if let Some(name) = &item.name {
                        inner_scope.set_local(name, item.value.clone());
                    }
                }
                inner_scope.arguments = Arc::new(values);

                let ret = self
                    .execute_block(&f.source.body, inner_scope)
                    .map(|ret| match ret {
                        Completion::EndOfBlock(_) => ScriptValue::opt(None),
                        Completion::ExplicitReturn(v) => v,
                        Completion::ImpliedReturn(v) => v,
                        _ => panic!("Script function ended with break/continue"),
                    });

                try_save(ret, &f.function.ret)?
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
                // XXX Shouldn't be an option at this point
                let params = variant.params.as_ref().unwrap();
                let values = transform_args(params, arguments);
                ScriptValue::Enum {
                    def: Arc::clone(&def),
                    index,
                    value: Arc::new(values),
                }
            }
            // Arguments aren't "transformed" for native calls, but passed as-is!
            // Native callables must use `iter_args()` to extract positional/named arguments.
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

                Arc::new(Tuple::new(items))
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
                return Ok(if is_opt {
                    ScriptValue::opt(Some(ret))
                } else {
                    ret
                });
            }
        }

        if is_opt {
            Ok(ScriptValue::opt(None))
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
            MatchPattern::Literal(lit) => match lit {
                Literal::True => {
                    if val.as_boolean()? {
                        Ok(Some(HashMap::new()))
                    } else {
                        Ok(None)
                    }
                }
                Literal::False => {
                    if !val.as_boolean()? {
                        Ok(Some(HashMap::new()))
                    } else {
                        Ok(None)
                    }
                }
                Literal::Int(n) => {
                    if *n == val.as_int()? {
                        Ok(Some(HashMap::new()))
                    } else {
                        Ok(None)
                    }
                }
                Literal::Char(n) => {
                    if *n == val.as_char()? {
                        Ok(Some(HashMap::new()))
                    } else {
                        Ok(None)
                    }
                }
                Literal::Str(s) => {
                    if *s == val.as_string()? {
                        Ok(Some(HashMap::new()))
                    } else {
                        Ok(None)
                    }
                }
            },
            MatchPattern::Assignee(name) => {
                if val.is_none() {
                    Ok(None)
                } else {
                    let mut locals = HashMap::new();
                    locals.insert(name.clone(), val.clone());
                    Ok(Some(locals))
                }
            }
            MatchPattern::Variant(prefix, name, assignee)
                if prefix.is_none() && name.as_str() == "Ok" =>
            {
                match val.as_fallible()? {
                    Fallible::Ok(value) => {
                        let mut locals = Scope::default();
                        if let Some(name) = assignee
                            .as_ref()
                            .and_then(|a| a.pattern.as_ref())
                            .and_then(|p| p.first())
                        {
                            eval_assignment(name, value, &mut locals);
                        }
                        Ok(Some(locals.locals))
                    }
                    Fallible::Err(_) => Ok(None),
                }
            }
            MatchPattern::Variant(prefix, name, assignee)
                if prefix.is_none() && name.as_str() == "Err" =>
            {
                match val.as_fallible()? {
                    Fallible::Ok(_) => Ok(None),
                    Fallible::Err(value) => {
                        let mut locals = Scope::default();
                        if let Some(name) = assignee
                            .as_ref()
                            .and_then(|a| a.pattern.as_ref())
                            .and_then(|p| p.first())
                        {
                            eval_assignment(name, value, &mut locals);
                        }
                        Ok(Some(locals.locals))
                    }
                }
            }
            MatchPattern::Variant(prefix, name, assignee) => {
                if let ScriptValue::Enum { def, index, .. } = val {
                    let def = {
                        if let Some(ident) = prefix {
                            match scope.types.get(ident) {
                                Some(TypeDefinition::EnumDefinition(e)) => Ok(e),
                                _ => Err(ScriptError::panic(format!("Enum not found: {ident}"))),
                            }
                        } else {
                            Ok(def)
                        }
                    }?;

                    if let Some((idx, _var)) = def.find_variant(name) {
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
fn transform_args(params: &TupleType, arguments: &Tuple) -> Tuple {
    let mut items = Vec::new();

    let mut args = arguments.iter_args();
    for par in params.items() {
        if let Some(arg) = args.resolve(par.name.as_ref()) {
            let val = transform_value(&par.value, &arg);
            items.push(TupleItem::new(par.name.clone(), val));
        } else if par.is_optional() {
            items.push(TupleItem::new(par.name.clone(), ScriptValue::opt(None)));
        } else {
            panic!("Missing argument");
        }
    }

    fn transform_value(par: &ScriptType, value: &ScriptValue) -> ScriptValue {
        match (&par, value) {
            (ScriptType::Tuple(t), ScriptValue::Tuple(tup)) => {
                let applied = transform_args(t, tup);
                ScriptValue::Tuple(Arc::new(applied))
            }
            (ScriptType::Tuple(t), ScriptValue::Rec { value: values, .. }) => {
                let applied = transform_args(t, values);
                ScriptValue::Tuple(Arc::new(applied))
            }
            (ScriptType::Opt(_), val) if !val.is_opt() => ScriptValue::opt(Some(val.clone())),
            _ => value.clone(),
        }
    }

    Tuple::new(items)
}

fn eval_assignment(lhs: &Src<Assignee>, rhs: &ScriptValue, scope: &mut Scope) {
    match (&lhs.name, &lhs.pattern) {
        (None, None) => {}
        (Some(name), None) => scope.set_local(name, rhs.clone()),
        (_, Some(pattern)) => match rhs {
            ScriptValue::Tuple(value) => eval_destructure(pattern, value, scope),
            ScriptValue::Rec { value, .. } => eval_destructure(pattern, value, scope),
            ScriptValue::Enum { value, .. } => eval_destructure(pattern, value, scope),
            _ => panic!("Expected tuple, found: {rhs}"),
        },
    }
}

fn eval_destructure(lhs: &[Src<Assignee>], rhs: &Tuple, scope: &mut Scope) {
    let mut args = rhs.iter_args();
    for par in lhs.iter() {
        if let Some(arg) = args.resolve(par.name.as_ref()) {
            eval_assignment(par, &arg, scope);
        } else {
            panic!("Missing argument");
        }
    }
}

// Catches cases where the ? operator is used inside a function (possibly nested blocks),
// on a value that was either None or Err, and makes the function return the variant.
fn try_save(ret: Result<ScriptValue>, expected: &ScriptType) -> Result<ScriptValue> {
    match ret {
        Ok(ret) => {
            // Automatically wrap value with Ok/Some.
            // I'm honestly not sure about this feature...
            // Explicit is better than implicit.
            if expected.is_optional() && !ret.is_opt() {
                Ok(ScriptValue::opt(Some(ret)))
            } else if expected.is_fallible() && !ret.is_fallible() {
                Ok(ScriptValue::ok(ret))
            } else {
                Ok(ret)
            }
        }
        Err(err) => match err.kind {
            ScriptErrorKind::NoValue => Ok(ScriptValue::opt(None)),
            ScriptErrorKind::Error(val) => Ok(ScriptValue::err(val)),
            _ => Err(err),
        },
    }
}
