use std::{
    collections::HashMap,
    fmt::{self, Write as _},
    sync::Arc,
};

use async_lock::RwLock;

use crate::{
    error::{ScriptError, ScriptErrorKind, ScriptResult},
    ext::{NativeMethodRef, NativeTypeMethodRef},
    ident::{Ident, global},
    lexer::Src,
    parser::{Assignee, CallExpression, Expression, Literal, MatchArm, MatchPattern, Statement},
    script_type::{ScriptType, TupleType},
    script_value::{Fallible, ScriptFunction, ScriptValue, Tuple, TupleItem},
    stdlib::{list::List, pipe::exec_spawn},
    type_scope::{TypeDefinition, TypeScope, eval_function},
};

#[cfg(feature = "pipe")]
use crate::{
    ext::ReadableExt,
    stdlib::pipe::{PipeImpl, PipeType, Tracker, exec_pipe},
};

#[derive(Debug)]
pub(crate) enum Completion {
    EndOfBlock(Scope),
    ExplicitReturn(ScriptValue),
    ImpliedReturn(ScriptValue),

    // TODO: See if we can implement the 'Try' trait, and simplify calls
    Break,
    Continue,
}

pub(crate) struct Scope {
    locals: HashMap<Ident, ScriptValue>,

    // Special case for functions declared in the current scope.
    //
    // In order to call a function 'b' from inside a function 'a', if 'a' is declared before 'b' in
    // the block, we need to "update" the scope of 'a', so that it contains 'b' with fully
    // evaluated scope when 'a' is called. This is solved by having this map shared between all
    // functions in the current scope.
    pub(crate) local_functions: Arc<RwLock<HashMap<Ident, ScriptFunction>>>,

    types: TypeScope,

    arguments: Arc<Tuple>,
}

impl Clone for Scope {
    fn clone(&self) -> Self {
        Self {
            locals: self.locals.clone(),
            local_functions: Default::default(),
            types: self.types.clone(),
            arguments: self.arguments.clone(),
        }
    }
}

impl Scope {
    fn new(types: HashMap<Ident, TypeDefinition>, globals: HashMap<Ident, ScriptValue>) -> Self {
        Self {
            locals: globals,
            local_functions: Default::default(),
            types: TypeScope::new(types),
            arguments: Default::default(),
        }
    }

    pub(crate) fn set_local(&mut self, name: impl Into<Ident>, value: ScriptValue) {
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

    fn capture(&self) -> Scope {
        Self {
            locals: self.locals.clone(),
            local_functions: self.local_functions.clone(),
            types: self.types.clone(),
            arguments: Default::default(),
        }
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope").finish()
    }
}

#[derive(Clone, Default)]
pub struct Interpreter {
    types: HashMap<Ident, TypeDefinition>,
    globals: HashMap<Ident, ScriptValue>,

    methods: HashMap<(Ident, Ident), NativeMethodRef>,
    type_methods: HashMap<Ident, NativeTypeMethodRef>,

    #[cfg(feature = "pipe")]
    pub(crate) tracker: Tracker,
}

impl Interpreter {
    pub(crate) fn with_types<T>(self, iter: T) -> Self
    where
        T: IntoIterator<Item = (Ident, TypeDefinition)>,
    {
        let mut types = self.types;
        types.extend(iter);
        Self { types, ..self }
    }

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

    pub(crate) fn with_type_methods(self, more: HashMap<Ident, NativeTypeMethodRef>) -> Self {
        let mut type_methods = self.type_methods;
        type_methods.extend(more);

        Self {
            type_methods,
            ..self
        }
    }

    fn get_method(&self, subject: &ScriptValue, name: &Ident) -> Option<NativeMethodRef> {
        let ns = match subject {
            ScriptValue::Int(_) => global::INT.into(),
            ScriptValue::String { .. } => global::STRING.into(),
            ScriptValue::Range(_, _) => global::RANGE.into(),
            ScriptValue::List(_) => global::LIST.into(),
            ScriptValue::Tuple(_) => global::TUPLE.into(),
            ScriptValue::Rec { .. } => global::REC.into(),
            ScriptValue::Union { .. } => global::UNION.into(),
            ScriptValue::Opt(_) => global::OPT.into(),
            ScriptValue::Fallible(_) => global::FALLIBLE.into(),
            ScriptValue::Ext(typ, _) => return typ.get_method(name),
            _ => todo!("NS for {subject}"),
        };
        self.methods.get(&(ns, name.clone())).cloned()
    }

    pub fn execute(&self, ast: &[Statement]) -> ScriptResult<HashMap<Ident, ScriptValue>> {
        let mut scope = Scope::new(self.types.clone(), self.globals.clone());

        // During evaluation, we don't really care about type inferrence.
        scope.types = scope.types.with_expected(ScriptType::Unknown);

        let end = self.execute_block(ast, scope)?;

        Ok(match end {
            Completion::EndOfBlock(scope) => scope.locals,
            _ => Default::default(),
        })
    }

    pub(crate) fn execute_block(
        &self,
        ast: &[Statement],
        mut scope: Scope,
    ) -> ScriptResult<Completion> {
        for node in ast {
            match node {
                Statement::Assignment { assignee, value } => {
                    let rhs = self.eval_expr(value, &scope)?;
                    eval_assignment(assignee, &rhs, &mut scope.locals);
                }
                Statement::Function { prefix, name, fun } => {
                    // XXX Tracking methods by name, which is incorrect if a type is redefined in an inner scope
                    let full_name = match prefix {
                        Some(prefix) => Ident::from(format!("{}::{}", prefix, name)),
                        None => name.clone(),
                    };

                    let type_scope = scope.types.resolve_self_type(prefix);
                    let function = eval_function(fun, &type_scope).map_err(ScriptError::panic)?;

                    let script_function =
                        ScriptFunction::new(function, Arc::clone(fun), Arc::new(scope.capture()));

                    let fun = ScriptValue::ScriptFunction(script_function.clone());
                    scope.set_local(&full_name, fun);
                    scope
                        .local_functions
                        .write_blocking()
                        .insert(full_name, script_function);
                }
                Statement::Rec(rec) => {
                    scope.types.eval_rec(rec).map_err(ScriptError::panic)?;
                }
                Statement::Union(def) => {
                    scope.types.eval_union(def).map_err(ScriptError::panic)?;
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
                        eval_assignment(assignee, &val, &mut inner_scope.locals);
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
                    let (val, inner_scope) = self.eval_condition_expr(cond, &scope)?;

                    let branch = if val { Some(body) } else { else_body.as_ref() };
                    if let Some(block) = branch {
                        match self.execute_block(block, inner_scope)? {
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
                Statement::While { cond, body } => loop {
                    let (val, inner_scope) = self.eval_condition_expr(cond, &scope)?;
                    if !val {
                        break;
                    }

                    match self.execute_block(body, inner_scope)? {
                        Completion::ExplicitReturn(val) => {
                            return Ok(Completion::ExplicitReturn(val));
                        }
                        Completion::Break => break,
                        _ => (),
                    }
                },
                Statement::WhileIn {
                    assignee,
                    value,
                    body,
                } => loop {
                    let val = self.eval_expr(value, &scope)?;

                    let val = match val {
                        ScriptValue::Opt(opt) => match opt {
                            Some(v) => v.as_ref().clone(),
                            _ => break,
                        },
                        o => o,
                    };

                    let mut inner_scope = scope.clone();
                    eval_assignment(assignee, &val, &mut inner_scope.locals);

                    match self.execute_block(body, inner_scope)? {
                        Completion::ExplicitReturn(val) => {
                            return Ok(Completion::ExplicitReturn(val));
                        }
                        Completion::Break => break,
                        _ => (),
                    }
                },
                Statement::Spawn { body } => {
                    exec_spawn(self, Arc::clone(body), scope.clone())?;
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

    fn eval_expr(&self, expr: &Src<Expression>, scope: &Scope) -> ScriptResult<ScriptValue> {
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
                    .collect::<ScriptResult<Vec<_>>>()?;
                ScriptValue::List(Arc::new(List::new(values)))
            }
            Expression::Tuple(s) => {
                let items = s
                    .iter()
                    .map(|arg| {
                        let value = self.eval_expr(&arg.expr, scope)?;
                        Ok(TupleItem::new(arg.name.clone(), value))
                    })
                    .collect::<ScriptResult<Vec<_>>>()?;

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
                    Some(typedef) => {
                        let prefixed_name = Ident::from(format!("{}::{}", prefix, name));
                        if let Some(v) = scope.locals.get(&prefixed_name) {
                            v.clone()
                        } else if let Some(method) = self.type_methods.get(name) {
                            ScriptValue::NativeTypeMethodBound(method.clone(), typedef.clone())
                        } else {
                            if let TypeDefinition::UnionDefinition(v) = typedef {
                                if let Some((index, variant)) =
                                    v.variants.iter().enumerate().find(|(_, v)| v.name == *name)
                                {
                                    if variant.params.is_none() {
                                        ScriptValue::Union {
                                            def: Arc::clone(v),
                                            index,
                                            value: Arc::new(Tuple::identity()),
                                        }
                                    } else {
                                        ScriptValue::UnionVariant {
                                            def: Arc::clone(v),
                                            index,
                                        }
                                    }
                                } else {
                                    panic!("Union variant not found: {name} in {prefix}");
                                }
                            } else {
                                panic!("Unexpected expression {prefix}::{name}")
                            }
                        }
                    }
                    _ => {
                        // XXX Little bit hackish to re-combine the full name like this
                        let full_ident = format!("{prefix}::{name}").into();
                        if let Some(value) = scope.locals.get(&full_ident) {
                            value.clone()
                        } else {
                            panic!("Union not found: {prefix}")
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
                    let prefix = match &subject {
                        ScriptValue::Rec { def, .. } => Some(Ident::clone(&def.name)),
                        ScriptValue::Union { def, .. } => Some(Ident::clone(&def.name)),
                        _ => None,
                    };

                    if let Some(prefix) = prefix {
                        let prefixed_name = Ident::from(format!("{}::{}", prefix, key));
                        if let Some(local) = scope.locals.get(&prefixed_name) {
                            if let ScriptValue::ScriptFunction(f) = local {
                                let bound_args = Tuple::new(vec![TupleItem::unnamed(subject)]);
                                return Ok(ScriptValue::ScriptFunctionBound(
                                    f.clone(),
                                    Arc::new(bound_args),
                                ));
                            } else {
                                panic!("Expected a script function here");
                            }
                        }
                    }

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
            Expression::Matches(lhs, pattern) => {
                let value = self.eval_expr(lhs, scope)?;
                if self.eval_match_pattern(pattern, &value, scope)?.is_some() {
                    ScriptValue::Boolean(true)
                } else {
                    ScriptValue::Boolean(false)
                }
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
                    ScriptValue::List(list) => {
                        let fallible_items = list
                            .items()
                            .iter()
                            .map(|i| Ok(Fallible::clone(i.as_fallible()?)))
                            .collect::<ScriptResult<Vec<_>>>()?;

                        let items = fallible_items
                            .into_iter()
                            .map(|i| match i {
                                Fallible::Ok(v) => Ok(*v),
                                Fallible::Err(e) => Err(ScriptError::error(*e)),
                            })
                            .collect::<ScriptResult<Vec<_>>>()?;

                        ScriptValue::List(Arc::new(List::new(items)))
                    }
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
                    ScriptValue::List(list) => {
                        let fallible_items = list
                            .items()
                            .iter()
                            .map(|i| Ok(Fallible::clone(i.as_fallible()?)))
                            .collect::<ScriptResult<Vec<_>>>()?;

                        let items = fallible_items
                            .into_iter()
                            .map(|i| match i {
                                Fallible::Ok(v) => Ok(*v),
                                Fallible::Err(e) => Err(ScriptError::panic(*e)),
                            })
                            .collect::<ScriptResult<Vec<_>>>()?;

                        ScriptValue::List(Arc::new(List::new(items)))
                    }
                    _ => Err(ScriptError::panic("Invalid unrwap operator"))?,
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

    // Evaluate the expression as a boolean, potentially binding locals to the inner scope
    fn eval_condition_expr(
        &self,
        cond: &Src<Expression>,
        scope: &Scope,
    ) -> ScriptResult<(bool, Scope)> {
        let mut inner_scope = scope.clone();
        let val = if let Expression::Matches(lhs, pattern) = cond.as_ref() {
            let value = self.eval_expr(lhs, scope)?;
            if let Some(locals) = self.eval_match_pattern(pattern, &value, scope)? {
                inner_scope.locals.extend(locals);
                true
            } else {
                false
            }
        } else {
            let val = self.eval_expr(cond, scope)?;
            let ScriptValue::Boolean(val) = val else {
                panic!("Not a boolean");
            };
            val
        };

        Ok((val, inner_scope))
    }

    fn eval_assert_expr(&self, expr: &Src<Expression>, scope: &Scope) -> ScriptResult<()> {
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
            Expression::Matches(lhs, pattern) => {
                let value = self.eval_expr(lhs, scope)?;
                if self.eval_match_pattern(pattern, &value, scope)?.is_some() {
                    Ok(())
                } else {
                    Err(ScriptError::new(ScriptErrorKind::AssertionFailed(format!(
                        "\n        + {value} does not match the expected pattern"
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

    fn eval_arithmetic<F>(
        &self,
        op: F,
        lhs: &Src<Expression>,
        rhs: &Src<Expression>,
        scope: &Scope,
    ) -> ScriptResult<ScriptValue>
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
    ) -> ScriptResult<ScriptValue>
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
    ) -> ScriptResult<ScriptValue>
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
    ) -> ScriptResult<()>
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
    ) -> ScriptResult<ScriptValue> {
        let return_value = match callable {
            ScriptValue::ScriptFunction(f) => {
                let mut inner_scope = self.clone_captured_scope(&f);

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

                try_wrap_err(wrap_retval(ret, &f.function.ret))?
            }
            ScriptValue::ScriptFunctionBound(f, bound_args) => {
                let mut inner_scope = self.clone_captured_scope(&f);

                let final_args = Tuple::new(
                    bound_args
                        .items()
                        .iter()
                        .cloned()
                        .chain(arguments.items().iter().cloned())
                        .collect(),
                );

                let values = transform_args(&f.function.params, &final_args);
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

                try_wrap_err(wrap_retval(ret, &f.function.ret))?
            }
            ScriptValue::Record(rec) => {
                let values = transform_args(&rec.params, arguments);
                ScriptValue::Rec {
                    def: Arc::clone(&rec),
                    value: Arc::new(values),
                }
            }
            ScriptValue::UnionVariant { def, index } => {
                let variant = &def.variants[index];
                // XXX Shouldn't be an option at this point
                let params = variant.params.as_ref().unwrap();
                let values = transform_args(params, arguments);
                ScriptValue::Union {
                    def: Arc::clone(&def),
                    index,
                    value: Arc::new(values),
                }
            }
            // Arguments aren't "transformed" for native calls, but passed as-is!
            // Native callables must use `iter_args()` to extract positional/named arguments.
            ScriptValue::NativeFunction(func) => try_wrap_err(func.call(self, arguments))?,
            ScriptValue::NativeMethodBound(method, subject) => {
                try_wrap_err(method.call(self, *subject, arguments))?
            }
            ScriptValue::NativeTypeMethodBound(method, typedef) => {
                try_wrap_err(method.call(self, &typedef, arguments))?
            }
            _ => panic!("Expected a callable, got: {callable:?}"),
        };

        Ok(return_value)
    }

    fn clone_captured_scope(&self, fun: &ScriptFunction) -> Scope {
        let mut scope = Scope::clone(&fun.captured_scope);

        // "Promote" functions
        let functions = fun.captured_scope.local_functions.read_blocking();
        for (name, func) in functions.iter() {
            scope.set_local(name, ScriptValue::ScriptFunction(func.clone()));
        }

        scope
    }

    fn eval_args(&self, arguments: &CallExpression, scope: &Scope) -> ScriptResult<Arc<Tuple>> {
        let tuple = match arguments {
            CallExpression::Inline(arguments) => {
                let items: Vec<_> = arguments
                    .iter()
                    .map(|a| {
                        let val = self.eval_expr(&a.expr, scope)?;
                        Ok(TupleItem::new(a.name.clone(), val))
                    })
                    .collect::<ScriptResult<Vec<_>>>()?;

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
    ) -> ScriptResult<ScriptValue> {
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
    ) -> ScriptResult<Option<HashMap<Ident, ScriptValue>>> {
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
                        let mut locals = HashMap::new();
                        if let Some(name) = assignee
                            .as_ref()
                            .and_then(|a| a.pattern.as_ref())
                            .and_then(|p| p.first())
                        {
                            eval_assignment(name, value, &mut locals);
                        }
                        Ok(Some(locals))
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
                        let mut locals = HashMap::new();
                        if let Some(name) = assignee
                            .as_ref()
                            .and_then(|a| a.pattern.as_ref())
                            .and_then(|p| p.first())
                        {
                            eval_assignment(name, value, &mut locals);
                        }
                        Ok(Some(locals))
                    }
                }
            }
            MatchPattern::Variant(prefix, name, assignee) => {
                if let ScriptValue::Union { def, index, .. } = val {
                    let def = {
                        if let Some(ident) = prefix {
                            match scope.types.get(ident) {
                                Some(TypeDefinition::UnionDefinition(e)) => Ok(e),
                                _ => Err(ScriptError::panic(format!("Union not found: {ident}"))),
                            }
                        } else {
                            Ok(def)
                        }
                    }?;

                    if let Some((idx, _var)) = def.find_variant(name) {
                        if Arc::ptr_eq(def, def) && *index == idx {
                            if let Some(assignee) = assignee {
                                let mut locals = HashMap::new();
                                eval_assignment(assignee, val, &mut locals);
                                Ok(Some(locals))
                            } else {
                                Ok(Some(HashMap::new()))
                            }
                        } else {
                            Ok(None)
                        }
                    } else {
                        Err(ScriptError::panic(format!(
                            "Union variant not found: {name} in {}",
                            def.name
                        )))
                    }
                } else {
                    Err(ScriptError::panic("Not a union"))
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

fn eval_assignment(
    lhs: &Src<Assignee>,
    rhs: &ScriptValue,
    scope: &mut HashMap<Ident, ScriptValue>,
) {
    match (&lhs.name, &lhs.pattern) {
        (None, None) => {}
        (Some(name), None) => {
            scope.insert(name.clone(), rhs.clone());
        }
        (_, Some(pattern)) => match rhs {
            ScriptValue::Tuple(value) => eval_destructure(pattern, value, scope),
            ScriptValue::Rec { value, .. } => eval_destructure(pattern, value, scope),
            ScriptValue::Union { value, .. } => eval_destructure(pattern, value, scope),
            _ => panic!("Expected tuple, found: {rhs}"),
        },
    }
}

fn eval_destructure(lhs: &[Src<Assignee>], rhs: &Tuple, scope: &mut HashMap<Ident, ScriptValue>) {
    let mut args = rhs.iter_args();
    for par in lhs.iter() {
        if let Some(arg) = args.resolve(par.name.as_ref()) {
            eval_assignment(par, &arg, scope);
        } else {
            panic!("Missing argument");
        }
    }
}

// Automatically wrap returned value with Ok/Some.
// I'm honestly not sure about this feature...
// Explicit is better than implicit.
fn wrap_retval(ret: ScriptResult<ScriptValue>, expected: &ScriptType) -> ScriptResult<ScriptValue> {
    ret.map(|ret| {
        if expected.is_optional() && !ret.is_opt() {
            ScriptValue::opt(Some(ret))
        } else if expected.is_fallible() && !ret.is_fallible() {
            ScriptValue::ok(ret)
        } else {
            ret
        }
    })
}

// Catches cases where the ? operator is used inside a function (possibly nested blocks),
// on a value that was either None or Err, and makes the function return the variant.
//
// Native functions can also return Err(NoValue) or Err(Error), and it's automatically
// converted to Ok(None) or Ok(Fallible::Err) here. This effectively makes the Rust ?
// operator behave like the ? operator inside a Taco script function.
fn try_wrap_err(ret: ScriptResult<ScriptValue>) -> ScriptResult<ScriptValue> {
    match ret {
        Ok(v) => Ok(v),
        Err(err) => match err.kind {
            ScriptErrorKind::NoValue => Ok(ScriptValue::opt(None)),
            ScriptErrorKind::Error(val) => Ok(ScriptValue::err(val)),
            _ => Err(err),
        },
    }
}
