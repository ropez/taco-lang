use std::{collections::HashMap, fmt::Display, sync::Arc};

use crate::{
    error::{TypeError, TypeErrorKind, TypeResult},
    ext::{NativeFunctionRef, NativeMethodRef},
    fmt::fmt_tuple,
    ident::{Ident, global},
    lexer::{Loc, Src},
    parser::{
        ArgumentExpression, Assignee, CallExpression, Expression, Function, Literal, MatchArm,
        MatchPattern, Statement,
    },
    script_type::{FunctionType, ScriptType, TupleItemType, TupleType},
    stdlib::{self, parse::ParseFunc},
    type_scope::{TypeDefinition, TypeScope, eval_params, eval_type_expr},
};

#[derive(Debug, Clone)]
pub struct ArgumentExpressionType {
    pub name: Option<Ident>,
    pub value: Src<ScriptType>,
}

impl Display for Src<Vec<ArgumentExpressionType>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_tuple(f, self.iter().map(|a| (a.name.clone(), &*a.value)))
    }
}

#[derive(Clone)]
struct Scope {
    locals: HashMap<Ident, ScriptType>,
    types: TypeScope,
    ret: Option<ScriptType>,
    arguments: TupleType,
}

impl Scope {
    fn new(types: HashMap<Ident, TypeDefinition>, globals: HashMap<Ident, ScriptType>) -> Self {
        Self {
            locals: globals,
            types: TypeScope::new(types),
            ret: None,
            arguments: TupleType::identity(),
        }
    }

    fn with_expected(&self, exp: impl Into<Option<ScriptType>>) -> Self {
        Self {
            types: self.types.with_expected(exp),
            ..self.clone()
        }
    }

    fn get_local(&self, name: &Ident) -> Option<&ScriptType> {
        self.locals.get(name)
    }

    fn set_local(&mut self, name: impl Into<Ident>, value: ScriptType) {
        let name = name.into();
        // Make sure we never assign a type to '_'
        if name.as_str() != "_" {
            self.locals.insert(name, value);
        }
    }
}

#[derive(Debug)]
struct ReturnType {
    typ: Src<ScriptType>,
    is_explicit: bool,
}

impl ReturnType {
    fn implicit(typ: Src<ScriptType>) -> Self {
        Self {
            typ,
            is_explicit: false,
        }
    }

    fn explicit(typ: Src<ScriptType>) -> Self {
        Self {
            typ,
            is_explicit: true,
        }
    }

    fn into_opt(self) -> Self {
        Self {
            typ: Src::new(
                ScriptType::Opt(Box::new(ScriptType::clone(&self.typ))),
                self.typ.loc,
            ),
            ..self
        }
    }
}

#[derive(Default)]
pub struct Validator {
    types: HashMap<Ident, TypeDefinition>,
    globals: HashMap<Ident, ScriptType>,
    methods: HashMap<(Ident, Ident), NativeMethodRef>,
}

impl Validator {
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
        T: IntoIterator<Item = (Ident, ScriptType)>,
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

    fn get_method(&self, subject: &ScriptType, name: &Ident) -> Option<NativeMethodRef> {
        let ns = match subject {
            ScriptType::Int => global::INT.into(),
            ScriptType::Str => global::STRING.into(),
            ScriptType::Range => global::RANGE.into(),
            ScriptType::RecInstance(_) => global::REC.into(), // XXX Should also support user-defined methods on the rec's own name
            ScriptType::Tuple(_) => global::TUPLE.into(),
            ScriptType::EmptyList | ScriptType::List(_) => global::LIST.into(),
            ScriptType::Opt(_) => global::OPT.into(),
            ScriptType::Fallible(_, _) => global::FALLIBLE.into(),
            ScriptType::Ext(ext) => return ext.get_method(name),
            _ => todo!("NS for {subject}"),
        };
        self.methods.get(&(ns, name.clone())).cloned()
    }

    pub fn validate(&self, ast: &[Statement]) -> TypeResult<()> {
        self.validate_block(ast, Scope::new(self.types.clone(), self.globals.clone()))?;
        Ok(())
    }

    fn validate_block(&self, ast: &[Statement], mut scope: Scope) -> TypeResult<Scope> {
        for node in ast {
            match node {
                Statement::Assignment { assignee, value } => {
                    let typ = self.validate_expr(value, &scope)?;
                    self.eval_assignment(assignee, &typ, &mut scope)?;
                }
                Statement::Function { name, fun } => {
                    let fun = self.eval_function(fun, &scope)?;

                    scope.set_local(name, ScriptType::Function(fun));
                }
                Statement::Rec(rec) => {
                    scope.types.eval_rec(rec)?;
                }
                Statement::Union(def) => {
                    scope.types.eval_union(def)?;
                }
                Statement::Iteration {
                    ident,
                    iterable,
                    body,
                } => {
                    let iterable_typ = self.validate_expr(iterable, &scope)?;

                    match iterable_typ {
                        ScriptType::EmptyList => {
                            return Err(TypeError::new(TypeErrorKind::EmptyList).at(iterable.loc));
                        }
                        ScriptType::List(inner) => {
                            let mut inner_scope = scope.clone();
                            inner_scope.set_local(ident, *inner);
                            self.validate_block(body, inner_scope)?;
                        }
                        ScriptType::Range => {
                            let mut inner_scope = scope.clone();
                            inner_scope.set_local(ident, ScriptType::Int);
                            self.validate_block(body, inner_scope)?;
                        }
                        ScriptType::Ext(ref ext) => {
                            if let Some(inner) = ext.as_readable() {
                                let mut inner_scope = scope.clone();
                                inner_scope.set_local(ident, inner);
                                self.validate_block(body, inner_scope)?;
                            } else {
                                return Err(TypeError::new(TypeErrorKind::InvalidIterable(
                                    iterable_typ,
                                ))
                                .at(iterable.loc));
                            }
                        }
                        _ => {
                            return Err(TypeError::new(TypeErrorKind::InvalidIterable(
                                iterable_typ,
                            ))
                            .at(iterable.loc));
                        }
                    }
                }
                Statement::While { cond, body } => {
                    let typ = self.validate_expr(cond, &scope)?;
                    if !ScriptType::Bool.accepts(&typ) {
                        return Err(TypeError::new(TypeErrorKind::InvalidArgumentType {
                            expected: ScriptType::Bool,
                            actual: typ.clone(),
                        })
                        .at(cond.loc));
                    }

                    self.validate_block(body, scope.clone())?;
                }
                Statement::Condition {
                    cond,
                    body,
                    else_body,
                } => {
                    let typ = self.validate_expr(cond, &scope)?;
                    if !ScriptType::Bool.accepts(&typ) {
                        return Err(TypeError::new(TypeErrorKind::InvalidArgumentType {
                            expected: ScriptType::Bool,
                            actual: typ.clone(),
                        })
                        .at(cond.loc));
                    }

                    self.validate_block(body, scope.clone())?;

                    if let Some(else_body) = else_body.as_ref() {
                        self.validate_block(else_body, scope.clone())?;
                    }
                }
                Statement::IfIn {
                    assignee,
                    value,
                    body,
                    else_body,
                } => {
                    let opt_typ = self.validate_expr(value, &scope)?;
                    let ScriptType::Opt(typ) = opt_typ else {
                        return Err(
                            TypeError::new(TypeErrorKind::InvalidQuestion(opt_typ)).at(value.loc)
                        );
                    };

                    let mut inner_scope = scope.clone();
                    self.eval_assignment(assignee, &typ, &mut inner_scope)?;
                    self.validate_block(body, inner_scope)?;

                    if let Some(else_body) = else_body.as_ref() {
                        self.validate_block(else_body, scope.clone())?;
                    }
                }
                Statement::Expression(expr) => {
                    self.validate_expr(expr, &scope)?;
                }
                Statement::Return(expr) => {
                    if let Some(expr) = expr {
                        let typ = self.validate_expr(expr, &scope)?;
                        match &scope.ret {
                            None => {
                                return Err(TypeError::new(TypeErrorKind::InvalidReturnType {
                                    expected: ScriptType::identity(),
                                    actual: typ,
                                })
                                .at(expr.loc));
                            }
                            Some(r) => {
                                if !r.accepts(&typ) {
                                    return Err(TypeError::new(TypeErrorKind::InvalidReturnType {
                                        expected: r.clone(),
                                        actual: typ,
                                    })
                                    .at(expr.loc));
                                }
                            }
                        }
                    } else {
                        match &scope.ret {
                            None => {}
                            Some(ScriptType::Opt(_)) => {}
                            Some(_) => {
                                return Err(TypeError::new(TypeErrorKind::MissingReturnStatement));
                            }
                        }
                    }
                }
                Statement::Assert(expr) => {
                    let typ = self.validate_expr(expr, &scope)?;
                    if !matches!(typ, ScriptType::Bool) {
                        return Err(TypeError::expected_bool(typ).at(expr.loc));
                    }

                    self.try_static_assert(expr, &scope)?;
                }
                Statement::Break => {}    // XXX Not allowed outside loop
                Statement::Continue => {} // XXX Not allowed outside loop
            }
        }

        Ok(scope)
    }

    fn eval_expr(&self, expr: &Src<Expression>, scope: &Scope) -> TypeResult<Src<ScriptType>> {
        let typ = self.validate_expr(expr, scope)?;
        Ok(Src::new(typ, expr.loc))
    }

    fn validate_expr(&self, expr: &Src<Expression>, scope: &Scope) -> TypeResult<ScriptType> {
        match expr.as_ref() {
            Expression::Literal(literal) => match literal {
                Literal::True => Ok(ScriptType::Bool),
                Literal::False => Ok(ScriptType::Bool),
                Literal::Int(_) => Ok(ScriptType::Int),
                Literal::Char(_) => Ok(ScriptType::Char),
                Literal::Str(_) => Ok(ScriptType::Str),
            },
            Expression::Arguments => Ok(ScriptType::Tuple(scope.arguments.clone())),
            Expression::String(parts) => {
                for (part, offset) in parts {
                    let typ = self
                        .validate_expr(part, scope)
                        .map_err(|err| err.at_offset(*offset))?;
                    match &typ {
                        ScriptType::Opt(t) => {
                            return Err(TypeError::new(TypeErrorKind::InvalidArgumentType {
                                expected: *t.clone(),
                                actual: typ,
                            })
                            .at(part.loc)
                            .at_offset(*offset));
                        }
                        ScriptType::Ext(_) => {
                            return Err(TypeError::new(TypeErrorKind::InvalidArgument {
                                expected: "printable value".into(),
                                actual: typ,
                            })
                            .at(part.loc)
                            .at_offset(*offset));
                        }
                        _ => (),
                    }
                }
                Ok(ScriptType::Str)
            }
            Expression::List(expressions) => {
                let types = expressions
                    .iter()
                    .map(|i| self.eval_expr(i, scope))
                    .collect::<TypeResult<Vec<_>>>()?;

                if let Some(inner_type) = self.most_specific_type(&types)? {
                    Ok(ScriptType::list_of(inner_type.cloned()))
                } else {
                    Ok(ScriptType::EmptyList)
                }
            }
            Expression::Tuple(args) => {
                let tuple = self.eval_tuple(args, scope)?;

                Ok(ScriptType::Tuple(tuple))
            }
            Expression::Ref(ident) => {
                if let Some(value) = scope.get_local(ident) {
                    Ok(value.clone())
                } else if let Some(typ) = scope.types.get(ident) {
                    match typ {
                        TypeDefinition::RecDefinition(rec) => {
                            // XXX Returning record as function up-front
                            // Should probably return a record type instead,
                            // and provide something like ScriptType::as_callable()
                            let ret = ScriptType::RecInstance(Arc::clone(rec));
                            Ok(ScriptType::Function(FunctionType::new(
                                rec.params.clone(),
                                ret,
                            )))
                        }
                        TypeDefinition::UnionDefinition(_) => {
                            Err(TypeError::new(TypeErrorKind::InvalidExpression).at(expr.loc))
                        }
                    }
                } else {
                    Err(
                        TypeError::new(TypeErrorKind::UndefinedReference(ident.clone()))
                            .at(expr.loc),
                    )
                }
            }
            Expression::PrefixedName(prefix, name) => match scope.types.get(prefix) {
                Some(TypeDefinition::RecDefinition(rec)) => {
                    // TODO Associated methods like Record::foo()
                    match name.as_str() {
                        "parse" => {
                            // XXX Add to globals somehow
                            let func =
                                NativeFunctionRef::new(Arc::new(ParseFunc::new(Arc::clone(rec))));
                            Ok(ScriptType::NativeFunction(func))
                        }
                        _ => Err(TypeError::new(TypeErrorKind::UndefinedMethod {
                            type_name: rec.name.clone(),
                            method_name: name.clone(),
                        })
                        .at(expr.loc)),
                    }
                }
                Some(TypeDefinition::UnionDefinition(def)) => {
                    if let Some((_, var)) = def.find_variant(name) {
                        if let Some(params) = &var.params {
                            Ok(ScriptType::UnionVariant {
                                def: Arc::clone(def),
                                params: params.clone(),
                            })
                        } else {
                            Ok(ScriptType::UnionInstance(Arc::clone(def)))
                        }
                    } else {
                        Err(TypeError::new(TypeErrorKind::UndefinedVariant {
                            type_name: def.name.clone(),
                            variant_name: name.clone(),
                        })
                        .at(expr.loc))
                    }
                }
                None => {
                    // XXX Little bit hackish to re-combine the full name like this
                    let full_ident = format!("{prefix}::{name}").into();
                    if let Some(value) = scope.locals.get(&full_ident) {
                        Ok(value.clone())
                    } else {
                        Err(
                            TypeError::new(TypeErrorKind::UndefinedReference(prefix.clone()))
                                .at(expr.loc),
                        )
                    }
                }
            },
            Expression::Access { subject, key } => {
                let subject = self.validate_expr(subject, scope)?;
                if let Some(tuple) = subject.as_tuple()
                    && let Some(item) = tuple.get_named(key)
                {
                    return Ok(item.clone());
                }

                if let Some(method) = self.get_method(&subject, key) {
                    Ok(ScriptType::NativeMethodBound(
                        method.clone(),
                        subject.into(),
                    ))
                } else {
                    Err(TypeError::new(TypeErrorKind::UndefinedAttribute {
                        subject,
                        attr_name: key.clone(),
                    })
                    .at(expr.loc))
                }
            }
            Expression::Function(fun) => {
                let fun = self.eval_function(fun, scope)?;
                Ok(ScriptType::Function(fun))
            }
            Expression::LogicNot(expr) => {
                let typ = self.validate_expr(expr, scope)?;
                if !ScriptType::Bool.accepts(&typ) {
                    return Err(TypeError::expected_bool(typ).at(expr.loc));
                }
                Ok(ScriptType::Bool)
            }
            Expression::Equal(lhs, rhs) | Expression::NotEqual(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if !(l.accepts(&r) || r.accepts(&l)) {
                    return Err(TypeError::new(TypeErrorKind::InvalidArgumentType {
                        expected: l,
                        actual: r,
                    })
                    .at(rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            #[cfg(feature = "pipe")]
            Expression::Pipe(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                let src = l
                    .as_readable()
                    .ok_or_else(|| TypeError::invalid_argument("readable", l.clone()))
                    .map_err(|err| err.at(lhs.loc))?;
                let dst = r
                    .as_writable()
                    .ok_or_else(|| TypeError::invalid_argument("writable", r.clone()))
                    .map_err(|err| err.at(rhs.loc))?;
                if !dst.accepts(&src) {
                    return Err(TypeError::expected_type(dst, src).at(expr.loc)); // XXX Needs pipe error
                }

                let pipe = stdlib::pipe::PipeType::new(l.as_writable(), r.as_readable());

                Ok(ScriptType::Ext(Arc::new(pipe)))
            }
            #[cfg(not(feature = "pipe"))]
            Expression::Pipe(_, _) => Err(TypeError::new(TypeErrorKind::InvalidExpression))?,
            Expression::LogicAnd(lhs, rhs) | Expression::LogicOr(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if !ScriptType::Bool.accepts(&l) {
                    return Err(TypeError::expected_bool(l).at(lhs.loc));
                }
                if !ScriptType::Bool.accepts(&r) {
                    return Err(TypeError::expected_bool(r).at(rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            Expression::LessThan(lhs, rhs)
            | Expression::GreaterThan(lhs, rhs)
            | Expression::LessOrEqual(lhs, rhs)
            | Expression::GreaterOrEqual(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if !ScriptType::Int.accepts(&l) {
                    return Err(TypeError::expected_number(l).at(lhs.loc));
                }
                if !ScriptType::Int.accepts(&r) {
                    return Err(TypeError::expected_number(r).at(rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            Expression::Range(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if !ScriptType::Int.accepts(&l) {
                    return Err(TypeError::expected_number(l).at(lhs.loc));
                }
                if !ScriptType::Int.accepts(&r) {
                    return Err(TypeError::expected_number(r).at(rhs.loc));
                }

                Ok(ScriptType::Range)
            }
            Expression::Negate(expr) => {
                let typ = self.validate_expr(expr, scope)?;
                if !ScriptType::Int.accepts(&typ) {
                    return Err(TypeError::expected_number(typ).at(expr.loc));
                }
                Ok(typ)
            }
            Expression::Addition(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Subtraction(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Multiplication(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Division(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Modulo(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Try(expr) => {
                let expr_type = self.validate_expr(expr, scope)?;
                eval_question(expr_type, scope)
            }
            Expression::Unwrap(inner) => {
                let inner_typ = self.validate_expr(inner, scope)?;
                match inner_typ {
                    ScriptType::Opt(inner) => Ok(*inner),
                    ScriptType::Fallible(inner, _) => Ok(*inner),
                    _ => {
                        Err(TypeError::new(TypeErrorKind::InvalidQuestion(inner_typ)).at(inner.loc))
                    }
                }
            }
            Expression::Coalesce(lhs, rhs) => {
                let lhs_typ = self.validate_expr(lhs, scope)?;
                let rhs_typ = self.validate_expr(rhs, scope)?;
                match lhs_typ {
                    ScriptType::Opt(inner) => {
                        if !inner.accepts(&rhs_typ) {
                            return Err(TypeError::new(TypeErrorKind::InvalidArgumentType {
                                expected: inner.as_ref().clone(),
                                actual: rhs_typ,
                            })
                            .at(rhs.loc));
                        }
                        Ok(rhs_typ)
                    }
                    _ => Err(TypeError::new(TypeErrorKind::InvalidQuestion(lhs_typ)).at(lhs.loc)),
                }
            }
            Expression::Call { subject, arguments } => {
                self.eval_call_expr(subject, arguments, scope)
            }
            Expression::Match {
                expr: match_expr,
                arms,
                is_opt,
            } => {
                let expr_type = self.eval_expr(match_expr, scope)?;

                let types = arms
                    .iter()
                    .map(|a| self.eval_match_arm(a, &expr_type, scope))
                    .collect::<TypeResult<Vec<_>>>()?;

                // XXX TODO Check that patterns accept expr type

                // XXX TODO Check exhaustiveness

                let mut is_exhausted = false;
                let patterns: Vec<_> = arms.iter().map(|a| a.pattern.as_ref()).collect();
                for (i, pat) in patterns.iter().enumerate() {
                    // XXX Must check if this specific pattern is exhausted by all previous
                    // patterns, even if all possible patterns aren't exhausted yet.
                    // e.g.: (true, a), (true, b)
                    if is_exhausted {
                        return Err(TypeError::new(TypeErrorKind::PatternAlreadyExhausted(
                            pat.cloned(),
                        ))
                        .at(pat.loc));
                    }

                    if expr_type.is_exhausted_by(&patterns[..=i]) {
                        is_exhausted = true;
                    }
                }

                if !is_exhausted && !is_opt {
                    return Err(TypeError::new(TypeErrorKind::PatternNotExhausted(
                        ScriptType::clone(&expr_type),
                    ))
                    .at(match_expr.loc));
                }

                let arms_type = self.most_specific_type(&types)?;

                match arms_type.map(|t| t.into_inner()) {
                    Some(t) => Ok(if *is_opt {
                        ScriptType::Opt(t.into())
                    } else {
                        t
                    }),
                    None => Err(TypeError::new(TypeErrorKind::EmptyList).at(expr.loc)),
                }
            }
        }
        .map_err(|err| err.at(expr.loc))
    }

    fn eval_function(&self, fun: &Arc<Function>, scope: &Scope) -> TypeResult<Arc<FunctionType>> {
        let params = eval_params(&fun.params, &scope.types)?;
        let declared_type = fun
            .type_expr
            .as_ref()
            .map(|expr| eval_type_expr(expr, &scope.types))
            .transpose()?;
        let mut inner = scope.clone();
        for arg in params.items() {
            if let Some(name) = &arg.name {
                inner.set_local(name, arg.value.clone());
            }
        }
        inner.arguments = params.clone();
        inner.ret = declared_type.clone();
        let inner = self.validate_block(&fun.body, inner.clone())?;
        let found_ret_type = self.eval_return_type(&fun.body, &inner)?;
        let found_type = found_ret_type
            .as_ref()
            .map(|r| ScriptType::clone(&r.typ))
            .unwrap_or(ScriptType::identity());
        if let Some(typ) = &declared_type
            && !typ.accepts(&found_type)
        {
            let loc = fun.type_expr.as_ref().map(|t| t.loc);
            if found_ret_type.is_some() {
                return Err(TypeError::new(TypeErrorKind::InvalidReturnType {
                    expected: typ.clone(),
                    actual: found_type,
                })
                .at(loc));
            } else {
                let loc = fun.type_expr.as_ref().map(|t| t.loc);
                return Err(TypeError::new(TypeErrorKind::MissingReturnStatement).at(loc));
            }
        }
        let ret = match (declared_type, found_type) {
            (Some(r), _) => r,
            (None, r) => r,
        };

        Ok(FunctionType::new(params, ret))
    }

    fn eval_return_type(&self, ast: &[Statement], scope: &Scope) -> TypeResult<Option<ReturnType>> {
        let typ = match ast.last() {
            None => None,
            Some(Statement::Return(expr)) => {
                if let Some(expr) = expr {
                    let typ = self.validate_expr(expr, scope)?;
                    Some(ReturnType::explicit(Src::new(typ, expr.loc)))
                } else {
                    todo!("return without value");
                }
            }
            Some(Statement::Expression(expr)) => {
                if ast.len() == 1 {
                    let typ = self.validate_expr(expr, scope)?;
                    Some(ReturnType::implicit(Src::new(typ, expr.loc)))
                } else {
                    None
                }
            }
            Some(Statement::Condition {
                body, else_body, ..
            }) => {
                let body_ret = self.eval_return_type(body, scope)?;
                let else_ret = else_body
                    .as_ref()
                    .map(|else_body| self.eval_return_type(else_body, scope))
                    .transpose()?
                    .flatten();

                match (body_ret, else_ret) {
                    (Some(l), Some(r)) if l.is_explicit && r.is_explicit => {
                        let types = vec![l.typ.clone(), r.typ.clone()];
                        let typ = self
                            .most_specific_type(&types)?
                            .unwrap_or(Src::new(ScriptType::identity(), Loc::new(0, 0)));
                        Some(ReturnType::explicit(typ))
                    }
                    (Some(typ), None) | (None, Some(typ)) if typ.is_explicit => {
                        Some(typ.into_opt())
                    }
                    _ => None,
                }
            }
            Some(Statement::IfIn {
                assignee,
                value,
                body,
                else_body,
            }) => {
                let opt_typ = self.validate_expr(value, scope)?;
                let ScriptType::Opt(typ) = opt_typ else {
                    return Err(
                        TypeError::new(TypeErrorKind::InvalidQuestion(opt_typ)).at(value.loc)
                    );
                };

                let mut inner_scope = scope.clone();
                self.eval_assignment(assignee, &typ, &mut inner_scope)?;

                // XXX DRY
                let body_ret = self.eval_return_type(body, &inner_scope)?;
                let else_ret = else_body
                    .as_ref()
                    .map(|else_body| self.eval_return_type(else_body, scope))
                    .transpose()?
                    .flatten();

                match (body_ret, else_ret) {
                    (Some(l), Some(r)) if l.is_explicit && r.is_explicit => {
                        let types = vec![l.typ.clone(), r.typ.clone()];
                        let typ = self
                            .most_specific_type(&types)?
                            .unwrap_or(Src::new(ScriptType::identity(), Loc::new(0, 0)));
                        Some(ReturnType::explicit(typ))
                    }
                    (Some(typ), None) | (None, Some(typ)) if typ.is_explicit => {
                        Some(typ.into_opt())
                    }
                    _ => None,
                }
            }
            Some(_) => None,
        };

        Ok(typ)
    }

    fn eval_call_expr(
        &self,
        subject: &Src<Expression>,
        arguments: &CallExpression,
        scope: &Scope,
    ) -> TypeResult<ScriptType> {
        let subject = self.eval_expr(subject, scope)?;

        if let ScriptType::NativeFunction(f) = subject.as_ref() {
            // XXX Support destructure
            let args = match arguments {
                CallExpression::Inline(inline) => self.eval_tuple(inline, scope),
                CallExpression::Destructure(src) => todo!(),
                CallExpression::DestructureImplicit(loc) => todo!(),
            }?;

            let params = subject.as_callable_params(&args)?;

            // Extensions can validate args, or just return expected args, for validation here
            self.validate_arguments(&params, arguments, scope)?;

            // XXX If extension always validated, we would only need this call
            let ret = subject.as_callable_ret(&args)?;

            Ok(ret)
        } else {
            // XXX For script functions, all validation must be done here
            // XXX Is the infer stuff really even needed for script?
            let params = subject.as_callable_params(&TupleType::identity())?;

            // XXX Change this to return arguments as a resolved TupleType, and remove all the
            // "infer by index" stuff
            let found_types = self.validate_arguments(&params, arguments, scope)?;

            let (arguments_inferred, complete) = apply_inferred_types_tuple(&params, &found_types)?;
            let ret = subject.as_callable_ret(&arguments_inferred)?;

            // XXX Fixes "bad" extensions returning Infer from return_type
            // let (ret, _) = apply_inferred_types(&ret, &found_types)?;

            if complete {
                Ok(ret)
            } else {
                // if let CallExpression::Inline(args) = arguments {
                //     let as_tuple = self.eval_tuple(args, scope)?;
                //     eprintln!("Looked for {params} using {as_tuple}, and found: {found_types:?}");
                // }
                Err(TypeError::new(TypeErrorKind::TypeNotInferred))
            }
        }
    }

    fn validate_arithmetic(
        &self,
        lhs: &Src<Expression>,
        rhs: &Src<Expression>,
        scope: &Scope,
    ) -> TypeResult<ScriptType> {
        let l = self.validate_expr(lhs, scope)?;
        let r = self.validate_expr(rhs, scope)?;

        if !ScriptType::Int.accepts(&l) {
            return Err(TypeError::expected_number(l).at(lhs.loc));
        }
        if !ScriptType::Int.accepts(&r) {
            return Err(TypeError::expected_number(r).at(rhs.loc));
        }

        Ok(ScriptType::Int)
    }

    fn eval_tuple(
        &self,
        arguments: &Src<Vec<ArgumentExpression>>,
        scope: &Scope,
    ) -> TypeResult<TupleType> {
        let items = arguments
            .iter()
            .map(|arg| {
                let value = self.eval_expr(&arg.expr, scope)?;
                Ok(TupleItemType::new(arg.name.clone(), value.into_inner()))
            })
            .collect::<TypeResult<Vec<_>>>()?;

        Ok(TupleType::new(items))
    }

    fn validate_arguments(
        &self,
        params: &TupleType,
        arguments: &CallExpression,
        scope: &Scope,
    ) -> TypeResult<HashMap<u16, ScriptType>> {
        let found_types = match arguments {
            CallExpression::Inline(arguments) => {
                self.validate_inline_args(params, arguments, scope)?
            }
            CallExpression::Destructure(expr) => {
                let t = self.eval_expr(expr, scope)?;
                let arg = ArgumentExpressionType {
                    name: None,
                    value: t.clone(),
                };
                let mut found_types = HashMap::new();
                self.validate_single_arg(
                    &ScriptType::Tuple(params.clone()),
                    &arg.value,
                    &mut found_types,
                )?;
                found_types
            }
            CallExpression::DestructureImplicit(loc) => {
                let typ = scope.arguments.clone();
                let arg = ArgumentExpressionType {
                    name: None,
                    value: Src::new(ScriptType::Tuple(typ), *loc),
                };
                let mut found_types = HashMap::new();
                self.validate_single_arg(
                    &ScriptType::Tuple(params.clone()),
                    &arg.value,
                    &mut found_types,
                )?;
                found_types
            }
        };
        Ok(found_types)
    }

    fn validate_inline_args(
        &self,
        formal: &TupleType,
        arguments: &Src<Vec<ArgumentExpression>>,
        scope: &Scope,
    ) -> TypeResult<HashMap<u16, ScriptType>> {
        // For each formal: If args contain named, take it, else take next unnamed
        let mut positional = arguments.iter().filter(|arg| arg.name.is_none());
        let mut found_types = HashMap::new();

        for par in formal.items() {
            let opt_arg = if let Some(name) = &par.name {
                arguments
                    .iter()
                    .find(|a| a.name.as_ref() == Some(name))
                    .or_else(|| positional.next())
            } else {
                positional.next()
            };

            if let Some(arg) = opt_arg {
                let actual = self.eval_expr(&arg.expr, &scope.with_expected(par.value.clone()))?;
                self.validate_single_arg(&par.value, &actual, &mut found_types)?;
            } else if !par.value.is_optional() {
                return Err(TypeError::new(TypeErrorKind::MissingArgument {
                    name: par.name.clone(),
                    expected: formal.clone(),
                    actual: self.eval_tuple(arguments, scope)?,
                })
                .at(arguments.loc));
            }
        }

        if positional.next().is_some() {
            return Err(TypeError::new(TypeErrorKind::UnexpectedArgument {
                expected: formal.clone(),
                actual: self.eval_tuple(arguments, scope)?,
            })
            .at(arguments.loc));
        }

        Ok(found_types)
    }

    fn validate_single_arg(
        &self,
        formal: &ScriptType,
        actual: &Src<ScriptType>,
        found_types: &mut HashMap<u16, ScriptType>,
    ) -> TypeResult<()> {
        let (expected, _) = apply_inferred_types(formal, found_types)?;
        if !expected.accepts(actual) {
            return Err(TypeError::new(TypeErrorKind::InvalidArgumentType {
                expected: formal.clone(),
                actual: actual.cloned(),
            })
            .at(actual.loc));
        }
        found_types.extend(infer_types(&expected, actual)?);
        Ok(())
    }

    // Check that all the given types are "compatible", meaning that it's possible
    // to construct a [list] containing elements of these types. Return the type
    // the element type that such a list would have.
    // Locations are included for error formatting. The returned tuple contains
    // the location of the first found element with the returned type.
    fn most_specific_type(&self, types: &[Src<ScriptType>]) -> TypeResult<Option<Src<ScriptType>>> {
        let mut iter = types.iter();
        if let Some(first) = iter.next() {
            let mut typ = first;
            for t in iter {
                if t.accepts(typ) {
                    typ = t;
                } else if !typ.accepts(t) {
                    return Err(TypeError::new(TypeErrorKind::InvalidArgumentType {
                        expected: typ.cloned(),
                        actual: t.cloned(),
                    })
                    .at(t.loc));
                }
            }
            Ok(Some(typ.clone()))
        } else {
            Ok(None)
        }
    }

    // This must recursively traverse patterns like `(a, (b, (c, d))) = foo`
    // For tuple-to-tuple checking, like function calls, this recursion happens in
    // ScriptType::accepts
    fn eval_assignment(
        &self,
        lhs: &Src<Assignee>,
        other: &ScriptType,
        scope: &mut Scope,
    ) -> TypeResult<()> {
        match (&lhs.name, &lhs.pattern) {
            (None, None) => {}
            (Some(name), None) => scope.set_local(name, other.clone()),
            (_, Some(pattern)) => match other {
                ScriptType::Tuple(tuple) => {
                    self.eval_destruction(pattern, lhs.loc, tuple, scope)?
                }
                ScriptType::RecInstance(rec) => {
                    self.eval_destruction(pattern, lhs.loc, &rec.params, scope)?
                }
                _ => {
                    return Err(
                        TypeError::new(TypeErrorKind::InvalidDestructure(other.clone()))
                            .at(lhs.loc),
                    );
                }
            },
        }

        Ok(())
    }

    fn eval_destruction(
        &self,
        lhs: &[Src<Assignee>],
        loc: Loc,
        other: &TupleType,
        scope: &mut Scope,
    ) -> TypeResult<()> {
        let mut positional = other.positional();

        for assignee in lhs.iter() {
            let opt_item = if let Some(name) = &assignee.name {
                other.get_named(name).or_else(|| positional.next())
            } else {
                positional.next()
            };

            if let Some(item) = opt_item {
                self.eval_assignment(assignee, item, scope)?;
            } else {
                return Err(TypeError::new(TypeErrorKind::MissingDestructureArgument {
                    name: assignee.name.clone(),
                    actual: other.clone(),
                })
                .at(assignee.loc));
            }
        }

        if positional.next().is_some() {
            return Err(
                TypeError::new(TypeErrorKind::UnexpectedDestructureArgument {
                    actual: other.clone(),
                })
                .at(loc),
            );
        }

        Ok(())
    }

    fn eval_match_arm(
        &self,
        arm: &MatchArm,
        expr_type: &ScriptType,
        scope: &Scope,
    ) -> TypeResult<Src<ScriptType>> {
        let mut inner_scope = scope.clone();

        match arm.pattern.as_ref().as_ref() {
            MatchPattern::Assignee(name) => {
                inner_scope.set_local(name, expr_type.flatten().clone());
            }
            MatchPattern::Variant(prefix, name, Some(assignee))
                if prefix.is_none() && name.as_str() == "Ok" =>
            {
                let Some(pattern) = &assignee.pattern else {
                    todo!("Handle missing args for Ok");
                };
                if pattern.len() != 1 {
                    todo!("Handle wrong args for Ok");
                }
                let arg = pattern.first().expect("we just checked");
                if arg.pattern.is_some() {
                    unimplemented!("Destructure Ok(): {:?}", assignee)
                }
                let (inner_value, _) = expr_type.as_fallible()?;
                if let Some(name) = &arg.name {
                    inner_scope.set_local(name, inner_value.clone());
                }
            }
            MatchPattern::Variant(prefix, name, Some(assignee))
                if prefix.is_none() && name.as_str() == "Err" =>
            {
                let Some(pattern) = &assignee.pattern else {
                    todo!("Handle missing args for Err");
                };
                if pattern.len() != 1 {
                    todo!("Handle wrong args for Err");
                }
                let arg = pattern.first().expect("we just checked");
                if arg.pattern.is_some() {
                    unimplemented!("Destructure Err(): {:?}", assignee)
                }
                let (_, inner_err) = expr_type.as_fallible()?;
                if let Some(name) = &arg.name {
                    inner_scope.set_local(name, inner_err.clone());
                }
            }
            MatchPattern::Variant(_, name, Some(assignee)) => {
                let var_typ = if let ScriptType::UnionInstance(e) = expr_type.flatten() {
                    e.find_variant(name)
                } else {
                    todo!("complex error pattern")
                };

                // Need to "fake" variant type to tuple type
                let var_params = var_typ.and_then(|(_, t)| t.params.as_ref());
                if let Some(tuple) = var_params {
                    let as_type = ScriptType::Tuple(tuple.clone());
                    self.eval_assignment(assignee, &as_type, &mut inner_scope)?;
                }
            }
            _ => (),
        }

        self.eval_expr(&arm.expr, &inner_scope)
    }

    fn try_static_assert(&self, expr: &Src<Expression>, scope: &Scope) -> TypeResult<()> {
        if let Expression::Equal(lhs, rhs) = expr.as_ref() {
            let rhs = self.try_static_eval(rhs.as_ref(), scope)?;
            let lhs = self.try_static_eval(lhs.as_ref(), scope)?;

            if let Some(lhs) = lhs
                && let Some(rhs) = rhs
                && lhs != rhs
            {
                return Err(TypeError::new(TypeErrorKind::TypeAssertionFailed(format!(
                    "{lhs} == {rhs}"
                )))
                .at(expr.loc));
            }
        }

        Ok(())
    }

    // XXX For general purpose, this should return ScriptValue instead of String
    fn try_static_eval(&self, expr: &Expression, scope: &Scope) -> TypeResult<Option<String>> {
        let opt = match expr {
            Expression::Literal(literal) => match literal {
                Literal::Str(s) => Some(s.to_string()),
                _ => None,
            },
            Expression::String(s) => {
                if s.len() == 1
                    && let Some(Expression::Literal(Literal::Str(f))) = s.first().map(|k| &*k.0)
                {
                    Some(f.to_string())
                } else {
                    None
                }
            }
            Expression::Call { subject, arguments } => {
                if let Expression::Ref(f) = subject.as_ref().as_ref()
                    && f.as_str() == "typeof"
                {
                    let CallExpression::Inline(arguments) = arguments.as_ref() else {
                        return Ok(None);
                    };
                    let inline = self.eval_tuple(arguments, scope)?;
                    let actual = inline.items().first();
                    let Some(actual) = actual else {
                        return Ok(None);
                    };

                    Some(actual.value.to_string())
                } else {
                    None
                }
            }
            _ => None,
        };

        Ok(opt)
    }
}

fn eval_question(expr_type: ScriptType, scope: &Scope) -> TypeResult<ScriptType> {
    match expr_type {
        ScriptType::Opt(inner) => {
            if let Some(ScriptType::Opt(_)) = &scope.ret {
                Ok(*inner)
            } else {
                Err(TypeError::new(TypeErrorKind::TryNotAllowed))
            }
        }
        ScriptType::Fallible(inner_value, inner_error) => {
            if let Some(ScriptType::Fallible(_, exp)) = &scope.ret {
                if exp.accepts(&inner_error) {
                    Ok(*inner_value)
                } else {
                    Err(TypeError::expected_type(
                        ScriptType::clone(exp),
                        ScriptType::clone(&inner_error),
                    ))?
                }
            } else {
                Err(TypeError::new(TypeErrorKind::TryNotAllowed))
            }
        }
        _ => Err(TypeError::new(TypeErrorKind::InvalidQuestion(expr_type))),
    }
}

fn infer_types(formal: &ScriptType, actual: &ScriptType) -> TypeResult<HashMap<u16, ScriptType>> {
    let mut found = HashMap::new();

    match (formal, actual) {
        (ScriptType::Infer(n), _) => {
            found.insert(*n, actual.clone());
        }
        (ScriptType::List(formal), ScriptType::List(inner)) => {
            found.extend(infer_types(formal, inner)?);
        }
        (ScriptType::List(formal), ScriptType::Range) => {
            found.extend(infer_types(formal, &ScriptType::Int)?);
        }
        (ScriptType::Function(formal), ScriptType::Function(actual)) => {
            found.extend(infer_tuple_types(&formal.params, &actual.params)?);
            found.extend(infer_types(&formal.ret, &actual.ret)?);
        }
        (ScriptType::Function(formal), ScriptType::NativeFunction(fun)) => {
            let arguments = TupleType::identity(); // How to get actual args here?
            found.extend(infer_types(&formal.ret, &fun.return_type(&arguments)?)?);
        }
        (ScriptType::Function(formal), ScriptType::UnionVariant { def, .. }) => {
            found.extend(infer_types(
                &formal.ret,
                &ScriptType::UnionInstance(Arc::clone(def)),
            )?);
        }
        (ScriptType::Tuple(formal), ScriptType::Tuple(actual)) => {
            found.extend(infer_tuple_types(formal, actual)?);
        }
        _ => (),
    }

    Ok(found)
}

fn infer_tuple_types(
    formal: &TupleType,
    actual: &TupleType,
) -> TypeResult<HashMap<u16, ScriptType>> {
    let mut found = HashMap::new();
    for (f, g) in formal.items().iter().zip(actual.items()) {
        found.extend(infer_types(&f.value, &g.value)?);
    }
    Ok(found)
}

fn apply_inferred_types(
    t: &ScriptType,
    found_types: &HashMap<u16, ScriptType>,
) -> TypeResult<(ScriptType, bool)> {
    let ret = match t {
        ScriptType::Infer(n) => {
            if let Some(ret) = found_types.get(n) {
                (ret.clone(), true)
            } else {
                (t.clone(), false)
            }
        }
        ScriptType::Opt(inner) => {
            let (inner, complete) = apply_inferred_types(inner, found_types)?;
            (ScriptType::opt_of(inner), complete)
        }
        ScriptType::Fallible(inner_value, inner_error) => {
            let (inner_value, value_complete) = apply_inferred_types(inner_value, found_types)?;
            let (inner_error, error_complete) = apply_inferred_types(inner_error, found_types)?;
            (
                ScriptType::fallible_of(inner_value, inner_error),
                value_complete && error_complete,
            )
        }
        ScriptType::Tuple(tuple) => {
            let (tuple, complete) = apply_inferred_types_tuple(tuple, found_types)?;
            (ScriptType::Tuple(tuple), complete)
        }
        ScriptType::List(inner) => {
            let (inner, complete) = apply_inferred_types(inner, found_types)?;
            (ScriptType::list_of(inner), complete)
        }
        ScriptType::Function(fun) => {
            let (params, params_complete) = apply_inferred_types_tuple(&fun.params, found_types)?;
            let (ret, ret_complete) = apply_inferred_types(&fun.ret, found_types)?;
            (
                ScriptType::Function(FunctionType::new(params, ret)),
                params_complete && ret_complete,
            )
        }
        ScriptType::NativeFunction(f) => {
            // XXX Assuming no arguments here might be an issue
            let args = f.arguments_type(&TupleType::identity())?;
            let (params, params_complete) = apply_inferred_types_tuple(&args, found_types)?;
            let (ret, ret_complete) = apply_inferred_types(&f.return_type(&params)?, found_types)?;
            (
                ScriptType::Function(FunctionType::new(params, ret)),
                params_complete && ret_complete,
            )
        }
        ScriptType::NativeMethodBound(met, subj) => {
            let (params, params_complete) =
                apply_inferred_types_tuple(&met.arguments_type(subj)?, found_types)?;
            let (ret, ret_complete) =
                apply_inferred_types(&met.return_type(subj, &params)?, found_types)?;
            (
                ScriptType::Function(FunctionType::new(params, ret)),
                params_complete && ret_complete,
            )
        }
        other => (other.clone(), true),
    };

    Ok(ret)
}

fn apply_inferred_types_tuple(
    tuple: &TupleType,
    found_types: &HashMap<u16, ScriptType>,
) -> TypeResult<(TupleType, bool)> {
    let items: Vec<_> = tuple
        .items()
        .iter()
        .map(|it| {
            let (inferred, complete) = apply_inferred_types(&it.value, found_types)?;
            Ok((TupleItemType::new(it.name.clone(), inferred), complete))
        })
        .collect::<TypeResult<Vec<_>>>()?;

    let all_complete = items.iter().all(|(_, complete)| *complete);
    let items = items.into_iter().map(|(t, _)| t).collect();

    Ok((TupleType::new(items), all_complete))
}
