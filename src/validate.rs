use std::{collections::HashMap, fmt::Display, ops::Range, sync::Arc};

use crate::{
    error::{Error, Result},
    parser::{
        Assignmee, AstNode, Expression, ExpressionKind, Parameter, TypeExpression,
        TypeExpressionKind,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptType {
    Bool,
    Int,
    Range,
    Str,
    EmptyList,
    List(Box<ScriptType>),
    Tuple(Vec<ScriptType>),
    Enum(Arc<str>),
    Function {
        params: Vec<(Arc<str>, ScriptType)>,
        ret: Box<ScriptType>,
    },
    Rec {
        name: Arc<str>,
        params: Vec<(Arc<str>, ScriptType)>,
    },
    State(Box<ScriptType>),
}

// TODO Validate that all branches in non-void functions return something
// TODO Validate unreachable code?

impl ScriptType {
    // Use () to represent nothing, like Rust
    pub const fn identity() -> Self {
        Self::Tuple(Vec::new())
    }

    fn accepts(&self, other: &ScriptType) -> bool {
        match (self, other) {
            (ScriptType::State(_), _) => false,
            (ScriptType::List(_), ScriptType::EmptyList) => true,
            (ScriptType::List(l), ScriptType::List(r)) => l.accepts(r),
            _ => *self == *other,
        }
    }

    fn most_specific(types: &[Self]) -> Option<Self> {
        if let Some(first) = types.first() {
            let mut typ = first;
            for t in types.iter().skip(1) {
                if t.accepts(typ) {
                    typ = t;
                }
            }
            Some(typ.clone())
        } else {
            None
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
            Self::List(inner) => {
                write!(f, "[{inner}]")
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    RecDefinition {
        name: Arc<str>,
        params: Vec<(Arc<str>, ScriptType)>,
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
    params: Vec<ScriptType>,
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
        self.validate_block(ast, Scope::with_globals(&self.globals))
    }

    fn validate_block(&self, ast: &[AstNode], mut scope: Scope) -> Result<()> {
        for node in ast {
            match node {
                AstNode::Assignment { assignee, value } => match assignee {
                    Assignmee::Scalar(name) => {
                        let typ = self.validate_expr(value, &scope)?;
                        scope.set_local(Arc::clone(name), typ);
                    }
                    Assignmee::Destructure(names) => {
                        let typ = self.validate_expr(value, &scope)?;
                        if let ScriptType::Tuple(types) = typ {
                            if names.len() == types.len() {
                                for (n, t) in names.iter().zip(types) {
                                    scope.set_local(Arc::clone(n), t);
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

                    let mut inner = scope.clone();
                    for (name, typ) in &params {
                        inner.set_local(Arc::clone(name), typ.clone());
                    }

                    let ret = match &fun.type_expr {
                        Some(expr) => self.eval_type_expr(expr, &scope)?,
                        None => {
                            // TODO Refactor implied return, and support if/else
                            if fun.body.len() == 1
                                && let Some(AstNode::Expression(expr)) = fun.body.first()
                            {
                                self.validate_expr(expr, &inner)?
                            } else {
                                ScriptType::identity()
                            }
                        }
                    };

                    inner.ret = Some(ret.clone());
                    self.validate_block(&fun.body, inner)?;

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
                                let params = v
                                    .type_exprs
                                    .iter()
                                    .map(|expr| self.eval_type_expr(expr, &scope))
                                    .collect::<Result<Vec<ScriptType>>>()?;
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
                    let typ = self.validate_expr(expr, &scope)?;

                    // Check implied return
                    if ast.len() == 1
                        && let Some(r) = &scope.ret
                        && !r.accepts(&typ)
                    {
                        return Err(self.fail(format!("Expected {r}, found {typ}"), &expr.loc));
                    }
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

        Ok(())
    }

    fn eval_params(
        &self,
        params: &[Parameter],
        scope: &Scope,
    ) -> Result<Vec<(Arc<str>, ScriptType)>> {
        params
            .iter()
            .map(|p| {
                let typ = self.eval_type_expr(&p.type_expr, scope)?;
                Ok((Arc::clone(&p.name), typ))
            })
            .collect()
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
                    .map(|i| self.validate_expr(i, scope))
                    .collect::<Result<Vec<ScriptType>>>()?;

                if let Some(inner_type) = ScriptType::most_specific(&types) {
                    for (typ, expr) in types.iter().zip(expressions) {
                        if !inner_type.accepts(typ) {
                            return Err(
                                self.fail(format!("Expected {inner_type}, found {typ}"), &expr.loc)
                            );
                        }
                    }

                    Ok(ScriptType::List(inner_type.into()))
                } else {
                    Ok(ScriptType::EmptyList)
                }
            }
            ExpressionKind::Tuple(expressions) => {
                let types = expressions
                    .iter()
                    .map(|i| self.validate_expr(i, scope))
                    .collect::<Result<Vec<ScriptType>>>()?;

                Ok(ScriptType::Tuple(types))
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
                println!("Access {subject:?} -> {key}");
                // if let Some(ExpressionKind::Ref(name)) = subject.kind {
                // }

                let subject_typ = self.validate_expr(subject, scope)?;
                match &subject_typ {
                    ScriptType::Rec { params, .. } => {
                        match params.iter().find(|(k, _)| *k == *key) {
                            Some((_, typ)) => Ok(typ.clone()),
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
            ExpressionKind::Call {
                subject,
                args,
                kwargs,
            } => match &subject.kind {
                ExpressionKind::Ref(name) => match name.as_ref() {
                    "state" => {
                        if args.len() != 1 {
                            return Err(self.fail(
                                format!("Expected 1 argument, got {}", args.len()),
                                &expr.loc,
                            ));
                        }
                        let arg = args.first().expect("state arg");
                        let typ = self.validate_expr(arg, scope)?;

                        Ok(ScriptType::State(typ.into()))
                    }
                    _ => match scope.get_local(name) {
                        Some(ScriptType::Function { params, ret }) => {
                            self.validate_args(params, args, kwargs, scope, &expr.loc)?;

                            Ok(*ret.clone())
                        }
                        Some(t) => {
                            Err(self.fail(format!("Expected a callable, found {t}"), &subject.loc))
                        }
                        None => match scope.types.get(name) {
                            Some(TypeDefinition::RecDefinition { params, name }) => {
                                self.validate_args(params, args, kwargs, scope, &expr.loc)?;

                                Ok(ScriptType::Rec {
                                    params: params.clone(),
                                    name: Arc::clone(name),
                                })
                            }
                            Some(TypeDefinition::EnumDefinition(_)) => {
                                todo!("call on enum variant")
                            }
                            None => {
                                Err(self.fail(format!("Undefined reference: {name}"), &subject.loc))
                            }
                        },
                    },
                },
                ExpressionKind::PrefixedName(prefix, name) => {
                    match scope.types.get(prefix) {
                        Some(TypeDefinition::RecDefinition { .. }) => todo!("rec call"),
                        Some(TypeDefinition::EnumDefinition(def)) => {
                            if let Some(var) = def.variants.iter().find(|v| v.name == *name) {
                                // XXX Need to get rid of kwargs here. Maybe this shouldn't be a Call expression, but something like the literal Tuple expression
                                // XXX Feels like we're re-implementing everything from Tuple
                                let expected_type = &var.params;
                                let actual_type = args
                                    .iter()
                                    .map(|expr| self.validate_expr(expr, scope))
                                    .collect::<Result<Vec<ScriptType>>>()?;

                                if *expected_type != actual_type {
                                    return Err(self.fail(
                                        format!(
                                            "Expected {expected_type:?}, found {actual_type:?}"
                                        ),
                                        &expr.loc,
                                    ));
                                }

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
                        (ScriptType::State(typ), "get") => Ok(*typ.clone()),
                        (ScriptType::State(typ), "set") => {
                            // Simulate normal function call
                            let params = vec![("".into(), *typ.clone())];
                            self.validate_args(&params, args, kwargs, scope, &expr.loc)?;
                            Ok(ScriptType::identity())
                        }
                        (ScriptType::EmptyList, "push") => {
                            // "Promote" list based on the first argument type
                            if let Some(typ) = args
                                .first()
                                .map(|i| self.validate_expr(i, scope))
                                .transpose()?
                            {
                                // XXX Variadic args not supported (but allowed in runtime)
                                let params = vec![("".into(), typ.clone())];
                                self.validate_args(&params, args, kwargs, scope, &expr.loc)?;
                                Ok(ScriptType::List(typ.into()))
                            } else {
                                Ok(ScriptType::EmptyList)
                            }
                        }
                        (ScriptType::List(typ), "push") => {
                            // XXX Variadic args not supported (but allowed in runtime)
                            let params = vec![("".into(), *typ.clone())];
                            self.validate_args(&params, args, kwargs, scope, &expr.loc)?;
                            Ok(ScriptType::List(typ.clone()))
                        }
                        (ScriptType::Rec { params, .. }, "with") => {
                            // TODO: Check the args is empty
                            self.validate_kwargs(params, kwargs, scope)?;

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

    fn validate_args(
        &self,
        params: &[(Arc<str>, ScriptType)],
        args: &[Expression],
        kwargs: &[(Arc<str>, Expression)],
        scope: &Scope,
        loc: &Range<usize>,
    ) -> Result<()> {
        // Check positional arguments
        for ((_, param_typ), expr) in params.iter().zip(args) {
            let typ = self.validate_expr(expr, scope)?;
            if !param_typ.accepts(&typ) {
                let msg = format!("Expected {param_typ}, found {typ}");
                return Err(self.fail(msg, &expr.loc));
            }
        }

        // Check number of positional args
        if args.len() > params.len() {
            return Err(self.fail(
                format!(
                    "Too many positional arguments: {} found, {} expected",
                    args.len(),
                    params.len()
                ),
                loc,
            ));
        }

        // Check kwargs against remaining params
        self.validate_kwargs(&params[args.len()..], kwargs, scope)?;

        // Check that all params are given
        for (name, _) in &params[args.len()..] {
            if !kwargs.iter().any(|(n, _)| *n == *name) {
                return Err(self.fail(format!("Missing argument: {}", name), loc));
            }
        }

        Ok(())
    }

    fn validate_kwargs(
        &self,
        params: &[(Arc<str>, ScriptType)],
        kwargs: &[(Arc<str>, Expression)],
        scope: &Scope,
    ) -> Result<()> {
        let mut found = Vec::new();

        for (name, expr) in kwargs {
            if found.contains(name) {
                return Err(self.fail(format!("Duplicate keyword argument: {name}"), &expr.loc));
            }
            found.push(Arc::clone(name));

            if let Some((_, param_typ)) = params.iter().find(|(n, _)| *n == *name) {
                let typ = self.validate_expr(expr, scope)?;
                if !param_typ.accepts(&typ) {
                    return Err(self.fail(format!("Expected {param_typ}, found {typ}"), &expr.loc));
                }
            } else {
                return Err(self.fail(format!("Argument not found, {name}"), &expr.loc));
            }
        }

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
            TypeExpressionKind::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|typ| self.eval_type_expr(typ, scope))
                    .collect::<Result<Vec<ScriptType>>>()?;
                Ok(ScriptType::Tuple(types))
            }
            TypeExpressionKind::List(inner) => {
                let inner = self.eval_type_expr(inner.as_ref(), scope)?;
                Ok(ScriptType::List(inner.into()))
            }
        }
    }

    fn fail(&self, msg: String, loc: &Range<usize>) -> Error {
        Error::new(msg, self.src, loc)
    }
}
