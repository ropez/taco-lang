use std::{collections::HashMap, fmt::Display, ops::Range, sync::Arc};

use crate::{
    error::{Error, Result},
    fmt::fmt_tuple,
    parser::{
        Arguments, Assignmee, AstNode, Expression, ExpressionKind, Parameter, TypeExpression,
        TypeExpressionKind,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum ScriptType {
    Bool,
    Int,
    Range,
    Str,
    EmptyList,
    List(Box<ScriptType>),
    Tuple(FormalArguments),
    Enum(Arc<str>),
    Function {
        params: FormalArguments,
        ret: Box<ScriptType>,
    },
    Rec {
        name: Arc<str>,
        params: FormalArguments,
    },
    State(Box<ScriptType>),
}

// TODO Validate that all branches in non-void functions return something
// TODO Validate unreachable code?

impl ScriptType {
    // Use () to represent nothing, like Rust
    pub const fn identity() -> Self {
        Self::Tuple(FormalArguments::empty())
    }

    fn accepts(&self, other: &ScriptType) -> bool {
        match (self, other) {
            (ScriptType::State(_), _) => false,
            (ScriptType::List(_), ScriptType::EmptyList) => true,
            (ScriptType::List(l), ScriptType::List(r)) => l.accepts(r),
            (ScriptType::Tuple(l), ScriptType::Tuple(r)) => l.accepts(r),
            _ => *self == *other,
        }
    }
}

impl Display for ScriptType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Int => write!(f, "int"),
            Self::Str => write!(f, "str"),
            Self::Enum(name) => write!(f, "{name}"),
            Self::EmptyList => write!(f, "[]"),
            Self::List(inner) => write!(f, "[{inner}]"),
            Self::Tuple(arguments) => write!(f, "{arguments}"),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    RecDefinition {
        name: Arc<str>,
        params: FormalArguments,
    },
    EnumDefinition(Arc<EnumDefinition>),
}

#[derive(Debug)]
pub struct EnumDefinition {
    name: Arc<str>,
    variants: Vec<EnumVariant>,
}

#[derive(Debug)]
struct EnumVariant {
    name: Arc<str>,
    params: FormalArguments,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormalArgument {
    name: Option<Arc<str>>,
    typ: ScriptType,
}

impl FormalArgument {
    pub fn new(name: Option<Arc<str>>, typ: ScriptType) -> Self {
        Self { name, typ }
    }

    pub fn named(name: Arc<str>, typ: ScriptType) -> Self {
        Self::new(Some(name), typ)
    }

    pub fn unnamed(typ: ScriptType) -> Self {
        Self::new(None, typ)
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct FormalArguments(Vec<FormalArgument>);

impl From<Vec<FormalArgument>> for FormalArguments {
    fn from(args: Vec<FormalArgument>) -> Self {
        FormalArguments(args)
    }
}

impl FormalArguments {
    const fn empty() -> Self {
        FormalArguments(Vec::new())
    }

    fn accepts(&self, other: &FormalArguments) -> bool {
        // Check params by strict position:
        // - Types must match
        // - If left has no name, right must have no name
        // - If left has name, right must have no name, or the same name

        for (l, r) in self.0.iter().zip(&other.0) {
            match (&l.name, &r.name) {
                (None, Some(_)) => return false,
                (Some(l), Some(r)) if *l != *r => return false,
                _ => {
                    if !l.typ.accepts(&r.typ) {
                        return false;
                    }
                }
            }
        }

        true
    }
}

impl Display for FormalArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // XXX FIXME Ineffective
        let items: Vec<_> = self
            .0
            .iter()
            .map(|a| match (&a.name, &a.typ) {
                (Some(name), t) => format!("{name}: {t}"),
                (None, t) => format!("{t}"),
            })
            .collect();
        fmt_tuple(f, &items)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvaluatedType(ScriptType, Range<usize>);

#[derive(Debug, Clone, PartialEq)]
pub struct AppliedArguments {
    pub(crate) args: Vec<EvaluatedType>,
    pub(crate) kwargs: Vec<(Arc<str>, EvaluatedType)>,
    pub(crate) loc: Range<usize>,
}

impl AppliedArguments {
    fn into_formal(self) -> FormalArguments {
        let args = self
            .args
            .into_iter()
            .map(|t| FormalArgument::unnamed(t.0))
            .chain(
                self.kwargs
                    .into_iter()
                    .map(|(n, t)| FormalArgument::named(n, t.0)),
            )
            .collect();

        FormalArguments(args)
    }
}

impl Display for AppliedArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // XXX FIXME Ineffective
        let items: Vec<_> = self
            .args
            .iter()
            .map(|a| format!("{}", a.0))
            .chain(self.kwargs.iter().map(|(n, a)| format!("{n}: {}", a.0)))
            .collect();
        fmt_tuple(f, &items)
    }
}

#[derive(Default, Clone)]
struct Scope {
    locals: HashMap<Arc<str>, ScriptType>,
    types: HashMap<Arc<str>, TypeDefinition>,
    ret: Option<ScriptType>,
}

impl Scope {
    fn with_globals(globals: &HashMap<Arc<str>, ScriptType>) -> Self {
        let locals = globals.clone();
        Self {
            locals,
            types: Default::default(),
            ret: None,
        }
    }

    fn get_local(&self, name: &str) -> Option<&ScriptType> {
        self.locals.get(name)
    }

    fn set_local(&mut self, name: Arc<str>, value: ScriptType) {
        // Make sure we never assign a type to '_'
        if name.as_ref() != "_" {
            self.locals.insert(name, value);
        }
    }
}

#[derive(Debug)]
struct ReturnType {
    typ: ScriptType,
    is_explicit: bool,
    loc: Range<usize>,
}

impl ReturnType {
    fn implicit(typ: ScriptType, loc: Range<usize>) -> Self {
        Self {
            typ,
            is_explicit: false,
            loc,
        }
    }

    fn explicit(typ: ScriptType, loc: Range<usize>) -> Self {
        Self {
            typ,
            is_explicit: true,
            loc,
        }
    }
}

pub struct Validator<'a> {
    src: &'a str,
    globals: HashMap<Arc<str>, ScriptType>,
}

impl<'a> Validator<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            globals: Default::default(),
        }
    }

    pub fn with_global(self, name: impl Into<Arc<str>>, typ: ScriptType) -> Self {
        let mut globals = self.globals;
        globals.insert(name.into(), typ);
        Self { globals, ..self }
    }

    pub fn validate(&self, ast: &[AstNode]) -> Result<()> {
        self.validate_block(ast, Scope::with_globals(&self.globals))?;
        Ok(())
    }

    fn validate_block(&self, ast: &[AstNode], mut scope: Scope) -> Result<Scope> {
        for node in ast {
            match node {
                AstNode::Assignment { assignee, value } => match assignee {
                    Assignmee::Scalar(name) => {
                        let typ = self.validate_expr(value, &scope)?;
                        scope.set_local(Arc::clone(name), typ);
                    }
                    Assignmee::Destructure(names) => {
                        let typ = self.validate_expr(value, &scope)?;

                        if let ScriptType::Tuple(params) = typ {
                            // XXX Support named formal args (lhs pattern)
                            // This should be the same as applying args when calling a function

                            let types: Vec<_> = params.0.iter().map(|a| a.typ.clone()).collect();

                            if names.len() == types.len() {
                                for (n, t) in names.iter().zip(types) {
                                    scope.set_local(Arc::clone(n), t.clone());
                                }
                            } else {
                                return Err(self.fail(
                                    format!(
                                        "Expected tuple with length {}, found {}",
                                        names.len(),
                                        types.len()
                                    ),
                                    &value.loc,
                                ));
                            }
                        } else {
                            return Err(self.fail("Expected tuple, found {typ}".into(), &value.loc));
                        }
                    }
                },
                AstNode::Function { name, fun } => {
                    let params = self.eval_params(&fun.params, &scope)?;

                    let declared_type = fun
                        .type_expr
                        .as_ref()
                        .map(|expr| self.eval_type_expr(expr, &scope))
                        .transpose()?;

                    let mut inner = scope.clone();
                    for arg in &params.0 {
                        if let Some(name) = &arg.name {
                            inner.set_local(Arc::clone(name), arg.typ.clone());
                        }
                    }
                    inner.ret = declared_type.clone();
                    let inner = self.validate_block(&fun.body, inner.clone())?;

                    let found_ret_type = self.eval_return_type(&fun.body, &inner)?;
                    let found_type = found_ret_type
                        .as_ref()
                        .map(|r| r.typ.clone())
                        .unwrap_or(ScriptType::identity());

                    if let Some(typ) = &declared_type
                        && !typ.accepts(&found_type)
                    {
                        if let Some(ret) = found_ret_type {
                            return Err(self.fail(
                                format!(
                                    "Incompatible return type: Expected {typ}, found {found_type}"
                                ),
                                &ret.loc,
                            ));
                        } else {
                            // XXX Needs 'loc' for entire block, or enclosing brace, or last
                            // statement
                            return Err(self.fail("Missing return statement".into(), &(0..0)));
                        }
                    }

                    let ret = match (declared_type, found_type) {
                        (Some(r), _) => r,
                        (None, r) => r,
                    };

                    scope.set_local(
                        Arc::clone(name),
                        ScriptType::Function {
                            params,
                            ret: ret.into(),
                        },
                    );
                }
                AstNode::Rec(rec) => {
                    let params = self.eval_params(&rec.params, &scope)?;

                    scope.types.insert(
                        Arc::clone(&rec.name),
                        TypeDefinition::RecDefinition {
                            params,
                            name: Arc::clone(&rec.name),
                        },
                    );
                }
                AstNode::Enum(rec) => {
                    let def = EnumDefinition {
                        name: Arc::clone(&rec.name),
                        variants: rec
                            .variants
                            .iter()
                            .map(|v| {
                                let params = self.eval_params(&v.params, &scope)?;
                                Ok(EnumVariant {
                                    name: Arc::clone(&v.name),
                                    params,
                                })
                            })
                            .collect::<Result<Vec<EnumVariant>>>()?,
                    };
                    scope.types.insert(
                        Arc::clone(&rec.name),
                        TypeDefinition::EnumDefinition(def.into()),
                    );
                }
                AstNode::Iteration {
                    ident,
                    iterable,
                    body,
                } => {
                    let iterable_typ = self.validate_expr(iterable, &scope)?;

                    match iterable_typ {
                        ScriptType::EmptyList => {
                            return Err(self.fail("List is always empty".into(), &iterable.loc));
                        }
                        ScriptType::List(inner) => {
                            let mut inner_scope = scope.clone();
                            inner_scope.set_local(Arc::clone(ident), *inner);
                            self.validate_block(body, inner_scope)?;
                        }
                        ScriptType::Range => {
                            let mut inner_scope = scope.clone();
                            inner_scope.set_local(Arc::clone(ident), ScriptType::Int);
                            self.validate_block(body, inner_scope)?;
                        }
                        _ => {
                            let msg = format!("Expected iterable, found {iterable_typ}");
                            return Err(self.fail(msg, &iterable.loc));
                        }
                    }
                }
                AstNode::Condition {
                    cond,
                    body,
                    else_body,
                } => {
                    let typ = self.validate_expr(cond, &scope)?;
                    if !ScriptType::Bool.accepts(&typ) {
                        return Err(self.fail(format!("Expected boolean, found {typ}"), &cond.loc));
                    }

                    self.validate_block(body, scope.clone())?;

                    if let Some(else_body) = else_body.as_ref() {
                        self.validate_block(else_body, scope.clone())?;
                    }
                }
                AstNode::Expression(expr) => {
                    self.validate_expr(expr, &scope)?;
                }
                AstNode::Return(expr) => {
                    let typ = self.validate_expr(expr, &scope)?;
                    match &scope.ret {
                        None => return Err(self.fail("Unexpected return value".into(), &expr.loc)),
                        Some(r) => {
                            if !r.accepts(&typ) {
                                return Err(
                                    self.fail(format!("Expected {r}, found {typ}"), &expr.loc)
                                );
                            }
                        }
                    }
                }
            }
        }

        Ok(scope)
    }

    fn eval_return_type(&self, ast: &[AstNode], scope: &Scope) -> Result<Option<ReturnType>> {
        let typ = match ast.last() {
            None => None,
            Some(AstNode::Return(expr)) => {
                let typ = self.validate_expr(expr, scope)?;
                Some(ReturnType::explicit(typ, expr.loc.clone()))
            }
            Some(AstNode::Expression(expr)) => {
                if ast.len() == 1 {
                    let typ = self.validate_expr(expr, scope)?;
                    Some(ReturnType::implicit(typ, expr.loc.clone()))
                } else {
                    None
                }
            }
            Some(AstNode::Condition {
                body, else_body, ..
            }) => {
                let ret = self.eval_return_type(body, scope)?;

                if let Some(else_body) = else_body.as_ref() {
                    let else_ret = self.eval_return_type(else_body, scope)?;

                    match (ret, else_ret) {
                        (Some(l), Some(r)) => {
                            let types = vec![
                                EvaluatedType(l.typ.clone(), l.loc.clone()),
                                EvaluatedType(r.typ.clone(), r.loc.clone()),
                            ];
                            let typ = self
                                .most_specific_type(&types)?
                                .unwrap_or(EvaluatedType(ScriptType::identity(), 0..0));
                            if l.is_explicit && r.is_explicit {
                                Some(ReturnType::explicit(typ.0, typ.1))
                            } else if ast.len() == 1 {
                                Some(ReturnType::implicit(typ.0, typ.1))
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                } else {
                    ret
                }
            }
            Some(_) => None,
        };

        Ok(typ)
    }

    fn eval_params(&self, params: &[Parameter], scope: &Scope) -> Result<FormalArguments> {
        let mut res = FormalArguments::empty();

        for param in params {
            let typ = self.eval_type_expr(&param.type_expr, scope)?;
            res.0.push(FormalArgument::new(param.name.clone(), typ))
        }

        Ok(res)
    }

    fn eval_expr(&self, expr: &Expression, scope: &Scope) -> Result<EvaluatedType> {
        let typ = self.validate_expr(expr, scope)?;
        Ok(EvaluatedType(typ, expr.loc.clone()))
    }

    fn validate_expr(&self, expr: &Expression, scope: &Scope) -> Result<ScriptType> {
        match &expr.kind {
            ExpressionKind::Number(_) => Ok(ScriptType::Int),
            ExpressionKind::String(_) => Ok(ScriptType::Str),
            ExpressionKind::True => Ok(ScriptType::Bool),
            ExpressionKind::False => Ok(ScriptType::Bool),
            ExpressionKind::StringInterpolate(parts) => {
                // TODO Validate internal expressions
                Ok(ScriptType::Str)
                // let mut builder = String::new();
                // for expr in parts {
                //     let val = self.eval_expr(expr, scope);
                //     write!(builder, "{val}").unwrap();
                // }
                // Arc::new(ScriptValue::String(builder.into()))
            }
            ExpressionKind::List(expressions) => {
                let types = expressions
                    .iter()
                    .map(|i| {
                        self.validate_expr(i, scope)
                            .map(|t| EvaluatedType(t, i.loc.clone()))
                    })
                    .collect::<Result<Vec<_>>>()?;

                if let Some(inner_type) = self.most_specific_type(&types)? {
                    Ok(ScriptType::List(inner_type.0.into()))
                } else {
                    Ok(ScriptType::EmptyList)
                }
            }
            ExpressionKind::Tuple(args) => {
                let applied = self.eval_args(args, scope)?;

                Ok(ScriptType::Tuple(applied.into_formal()))
            }
            ExpressionKind::Ref(ident) => match scope.get_local(ident) {
                None => Err(self.fail(format!("Undefined reference: {ident}"), &expr.loc)),
                Some(value) => Ok(value.clone()),
            },
            ExpressionKind::PrefixedName(prefix, name) => match scope.types.get(prefix) {
                Some(TypeDefinition::RecDefinition { .. }) => todo!("rec access"),
                Some(TypeDefinition::EnumDefinition(def)) => {
                    if def.variants.iter().any(|v| v.name == *name) {
                        Ok(ScriptType::Enum(Arc::clone(prefix)))
                    } else {
                        Err(self.fail(
                            format!("Variant not found: {name} in {}", def.name),
                            &expr.loc,
                        ))
                    }
                }
                None => Err(self.fail(format!("Undefined type: {prefix}"), &expr.loc)),
            },
            ExpressionKind::Access { subject, key } => {
                let subject_typ = self.validate_expr(subject, scope)?;
                match &subject_typ {
                    ScriptType::Rec { params, .. } => {
                        match params.0.iter().find(|a| a.name.as_ref() == Some(key)) {
                            Some(a) => Ok(a.typ.clone()),
                            None => Err(self.fail(
                                format!("Unknown attribute: {key} on {subject_typ}"),
                                &expr.loc,
                            )),
                        }
                    }
                    _ => Err(self.fail(
                        format!("Unknown attribute: {key} on {subject_typ}"),
                        &expr.loc,
                    )),
                }
            }
            ExpressionKind::Not(expr) => {
                let typ = self.validate_expr(expr, scope)?;
                if ScriptType::Bool.accepts(&typ) {
                    return Err(self.fail(format!("Expected boolean, found {typ}"), &expr.loc));
                }
                Ok(ScriptType::Bool)
            }
            ExpressionKind::Equal(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if r != l {
                    return Err(self.fail(format!("Expected {l}, found {r}"), &rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            ExpressionKind::NotEqual(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if r != l {
                    return Err(self.fail(format!("Expected {l}, found {r}"), &rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            ExpressionKind::Range(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if l != ScriptType::Int {
                    return Err(self.fail(format!("Expected number, found {l}"), &lhs.loc));
                }
                if r != ScriptType::Int {
                    return Err(self.fail(format!("Expected number, found {r}"), &rhs.loc));
                }

                Ok(ScriptType::Range)
            }
            ExpressionKind::Addition(lhs, rhs) => self.validate_arithmetic(scope, lhs, rhs),
            ExpressionKind::Subtraction(lhs, rhs) => self.validate_arithmetic(scope, lhs, rhs),
            ExpressionKind::Multiplication(lhs, rhs) => self.validate_arithmetic(scope, lhs, rhs),
            ExpressionKind::Division(lhs, rhs) => self.validate_arithmetic(scope, lhs, rhs),
            ExpressionKind::Call { subject, arguments } => match &subject.kind {
                ExpressionKind::Ref(name) => match name.as_ref() {
                    "state" => {
                        if arguments.args.len() != 1 {
                            return Err(self.fail(
                                format!("Expected 1 argument, got {}", arguments.args.len()),
                                &expr.loc,
                            ));
                        }
                        let arg = arguments.args.first().expect("state arg");
                        let typ = self.validate_expr(arg, scope)?;

                        Ok(ScriptType::State(typ.into()))
                    }
                    _ => match scope.get_local(name) {
                        Some(ScriptType::Function { params, ret }) => {
                            let arguments = self.eval_args(arguments, scope)?;
                            self.validate_args(params, &arguments)?;

                            Ok(*ret.clone())
                        }
                        Some(t) => {
                            Err(self.fail(format!("Expected a callable, found {t}"), &subject.loc))
                        }
                        None => match scope.types.get(name) {
                            Some(TypeDefinition::RecDefinition { params, name }) => {
                                let arguments = self.eval_args(arguments, scope)?;
                                self.validate_args(params, &arguments)?;

                                Ok(ScriptType::Rec {
                                    params: params.clone(),
                                    name: Arc::clone(name),
                                })
                            }
                            Some(TypeDefinition::EnumDefinition(_)) => Err(self.fail(
                                format!("Expected a callable, found enum {name}"),
                                &subject.loc,
                            )),
                            None => {
                                Err(self.fail(format!("Undefined reference: {name}"), &subject.loc))
                            }
                        },
                    },
                },
                ExpressionKind::PrefixedName(prefix, name) => {
                    match scope.types.get(prefix) {
                        Some(TypeDefinition::RecDefinition { name: rec_name, .. }) => {
                            // TODO Associated methods like Record::foo()
                            Err(self.fail(
                                format!("Method not found: '{name}' for {rec_name}"),
                                &expr.loc,
                            ))
                        }
                        Some(TypeDefinition::EnumDefinition(def)) => {
                            if let Some(var) = def.variants.iter().find(|v| v.name == *name) {
                                let arguments = self.eval_args(arguments, scope)?;
                                self.validate_args(&var.params, &arguments)?;
                                Ok(ScriptType::Enum(Arc::clone(prefix)))
                            } else {
                                Err(self.fail(
                                    format!("Variant not found: {name} in {}", def.name),
                                    &expr.loc,
                                ))
                            }
                        }
                        None => Err(self.fail(format!("Undefined type: {prefix}"), &expr.loc)),
                    }
                }
                ExpressionKind::Access { subject, key } => {
                    let subject_typ = self.validate_expr(subject, scope)?;
                    match (&subject_typ, key.as_ref()) {
                        (ScriptType::State(typ), "get") => {
                            let arguments = self.eval_args(arguments, scope)?;
                            self.validate_args(&FormalArguments::empty(), &arguments)?;
                            Ok(*typ.clone())
                        }
                        (ScriptType::State(typ), "set") => {
                            // Simulate normal function call
                            let formal =
                                FormalArguments(vec![FormalArgument::unnamed(*typ.clone())]);
                            let arguments = self.eval_args(arguments, scope)?;
                            self.validate_args(&formal, &arguments)?;
                            Ok(ScriptType::identity())
                        }
                        (ScriptType::EmptyList, "push") => {
                            // "Promote" list based on the first argument type
                            if let Some(typ) = arguments
                                .args
                                .first()
                                .map(|i| self.validate_expr(i, scope))
                                .transpose()?
                            {
                                // XXX Variadic args not supported (but allowed in runtime)
                                let formal =
                                    FormalArguments(vec![FormalArgument::unnamed(typ.clone())]);
                                let arguments = self.eval_args(arguments, scope)?;
                                self.validate_args(&formal, &arguments)?;
                                Ok(ScriptType::List(typ.into()))
                            } else {
                                Ok(ScriptType::EmptyList)
                            }
                        }
                        (ScriptType::List(typ), "push") => {
                            // XXX Variadic args not supported (but allowed in runtime)
                            let formal =
                                FormalArguments(vec![FormalArgument::unnamed(*typ.clone())]);
                            let arguments = self.eval_args(arguments, scope)?;
                            self.validate_args(&formal, &arguments)?;
                            Ok(ScriptType::List(typ.clone()))
                        }
                        (ScriptType::Rec { params, .. }, "with") => {
                            let args = self.eval_args(arguments, scope)?;

                            // XXX FIXME Non-mandatory args:
                            // self.validate_applied_args(params, &args)?;

                            Ok(subject_typ.clone())
                        }
                        _ => {
                            Err(self
                                .fail(format!("Unknown method: {key} on {subject_typ}"), &expr.loc))
                        }
                    }
                }
                _ => Err(self.fail("Call not allowed here".into(), &expr.loc)),
            },
        }
    }

    fn validate_arithmetic(
        &self,
        scope: &Scope,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<ScriptType> {
        let l = self.validate_expr(lhs, scope)?;
        let r = self.validate_expr(rhs, scope)?;

        if l != ScriptType::Int {
            return Err(self.fail(format!("Expected number, found {l}"), &lhs.loc));
        }
        if r != ScriptType::Int {
            return Err(self.fail(format!("Expected number, found {r}"), &rhs.loc));
        }

        Ok(ScriptType::Int)
    }

    fn eval_args(&self, arguments: &Arguments, scope: &Scope) -> Result<AppliedArguments> {
        let args = arguments
            .args
            .iter()
            .map(|a| self.eval_expr(a, scope))
            .collect::<Result<Vec<_>>>()?;

        let kwargs = arguments
            .kwargs
            .iter()
            .map(|(k, a)| {
                let typ = self.eval_expr(a, scope)?;
                Ok((Arc::clone(k), typ))
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(AppliedArguments {
            args,
            kwargs,
            loc: arguments.loc.clone(),
        })
    }

    fn validate_args(&self, formal: &FormalArguments, other: &AppliedArguments) -> Result<()> {
        for (i, formal_arg) in formal.0.iter().enumerate() {
            if let Some(arg) = other.args.get(i) {
                if !formal_arg.typ.accepts(&arg.0) {
                    return Err(
                        self.fail(format!("Expected {} got {}", formal_arg.typ, arg.0), &arg.1)
                    );
                }
            } else if let Some(name) = &formal_arg.name {
                // Assuming formal args have unique names
                if let Some((_, arg)) = other.kwargs.iter().find(|(n, _)| *n == *name) {
                    if !formal_arg.typ.accepts(&arg.0) {
                        return Err(
                            self.fail(format!("Expected {} got {}", formal_arg.typ, arg.0), &arg.1)
                        );
                    }
                } else {
                    return Err(self.fail(
                        format!("Missing argument {name}, expected {formal} got {other}"),
                        &other.loc,
                    ));
                }
            } else {
                return Err(self.fail(
                    format!("Missing positional argument {i}, expected {formal} got {other}"),
                    &other.loc,
                ));
            }
        }

        // XXX Fail if there are additional kwargs?

        Ok(())
    }

    fn eval_type_expr(&self, type_expr: &TypeExpression, scope: &Scope) -> Result<ScriptType> {
        match &type_expr.kind {
            TypeExpressionKind::Scalar(ident) => match ident.as_ref() {
                "str" => Ok(ScriptType::Str),
                "int" => Ok(ScriptType::Int),
                "bool" => Ok(ScriptType::Bool),
                e => match scope.types.get(e) {
                    Some(TypeDefinition::RecDefinition { name, params }) => Ok(ScriptType::Rec {
                        params: params.clone(),
                        name: Arc::clone(name),
                    }),
                    Some(TypeDefinition::EnumDefinition(def)) => {
                        Ok(ScriptType::Enum(Arc::clone(&def.name)))
                    }
                    None => Err(self.fail(format!("Unknown type: {e}"), &type_expr.loc)),
                },
            },
            TypeExpressionKind::Tuple(params) => {
                let types = self.eval_params(params, scope)?;
                Ok(ScriptType::Tuple(types))
            }
            TypeExpressionKind::List(inner) => {
                let inner = self.eval_type_expr(inner.as_ref(), scope)?;
                Ok(ScriptType::List(inner.into()))
            }
        }
    }

    // Check that all the given types are "compatible", meaning that it's possible
    // to construct a [list] containing elements of these types. Return the type
    // the element type that such a list would have.
    // Locations are included for error formatting. The returned tuple contains
    // the location of the first found element with the returned type.
    fn most_specific_type(&self, types: &[EvaluatedType]) -> Result<Option<EvaluatedType>> {
        let mut iter = types.iter();
        if let Some(first) = iter.next() {
            let mut typ = first;
            for t in iter {
                if t.0.accepts(&typ.0) {
                    typ = t;
                } else if !typ.0.accepts(&t.0) {
                    return Err(self.fail(
                        format!("Found incompatible types: {} and {}", t.0, typ.0),
                        &t.1,
                    ));
                }
            }
            Ok(Some(typ.clone()))
        } else {
            Ok(None)
        }
    }

    fn fail(&self, msg: String, loc: &Range<usize>) -> Error {
        Error::new(msg, self.src, loc)
    }
}
