use std::{collections::HashMap, fmt::Display, sync::Arc};

use crate::{
    error::{Error, Result},
    fmt::fmt_tuple,
    ident::Ident,
    lexer::{Loc, Src},
    parser::{Arguments, ArgumentsKind, Assignee, AstNode, Expression, Parameter, TypeExpression},
};

#[derive(Debug, Clone, PartialEq)]
pub enum ScriptType {
    Bool,
    Int,
    Range,
    Str,
    EmptyList,
    Opt(Box<ScriptType>),
    List(Box<ScriptType>),
    Tuple(TupleType),
    Enum(Arc<EnumType>),
    EnumVariant(Arc<EnumType>, Ident),
    Function {
        params: TupleType,
        ret: Box<ScriptType>,
    },
    Rec {
        name: Ident,
        params: TupleType,
    },
    State(Box<ScriptType>),
}

impl ScriptType {
    // Use () to represent nothing, like Rust
    pub const fn identity() -> Self {
        Self::Tuple(TupleType::identity())
    }

    fn accepts(&self, other: &ScriptType) -> bool {
        match (self, other) {
            (ScriptType::State(_), _) => false,
            (ScriptType::Bool, ScriptType::Bool) => true,
            (ScriptType::List(_), ScriptType::EmptyList) => true,
            (ScriptType::List(l), ScriptType::List(r)) => l.accepts(r),
            (ScriptType::Tuple(l), ScriptType::Tuple(r)) => l.accepts(r),
            (ScriptType::Tuple(l), ScriptType::Rec { params, .. }) => l.accepts(params),
            (ScriptType::Enum(l), ScriptType::Enum(r)) => Arc::ptr_eq(l, r),
            (ScriptType::Enum(_), ScriptType::EnumVariant(_, _)) => false,

            // If a function returns str?, it can also optionally call another function that
            // returns str?. This doesn't turn into "str??", it's always just a value nor no value.
            // You can't have a situation where an expression like "if x in opt" sets 'x' to
            // another opt!
            (ScriptType::Opt(l), r) => l.accepts(r.flatten()),
            _ => *self == *other,
        }
    }

    fn flatten(&self) -> &Self {
        if let ScriptType::Opt(opt) = self {
            opt.flatten()
        } else {
            self
        }
    }

    pub fn as_tuple(&self) -> Option<&TupleType> {
        match &self {
            ScriptType::Tuple(tuple) => Some(tuple),
            ScriptType::Rec { params, .. } => Some(params),
            _ => None,
        }
    }

    pub fn as_callable(&self) -> Option<(&TupleType, Arc<ScriptType>)> {
        // XXX Too much cloning
        match &self {
            ScriptType::Function { params, ret } => {
                Some((params, Arc::new(ScriptType::clone(ret))))
            }
            ScriptType::EnumVariant(def, name) => {
                let var = def.variants.iter().find(|v| v.name == *name)?;
                // XXX We don't have EnumVariant if params is None, this should be part of the type system
                let params = var.params.as_ref().unwrap();
                Some((params, Arc::new(ScriptType::Enum(Arc::clone(def)))))
            }
            _ => None,
        }
    }
}

impl Display for ScriptType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Int => write!(f, "int"),
            Self::Str => write!(f, "str"),
            Self::Enum(typ) => write!(f, "{}", typ.name),
            Self::EnumVariant(typ, var) => write!(f, "{}::{}", typ.name, var), // Display as function?
            Self::EmptyList => write!(f, "[]"),
            Self::List(inner) => write!(f, "[{inner}]"),
            Self::Opt(inner) => write!(f, "{inner}?"),
            Self::Tuple(arguments) => write!(f, "{arguments}"),
            Self::Rec { name, params } => write!(f, "{name}{params}"),
            Self::Function { params, ret } => write!(f, "fun{params}: {ret}"),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    RecDefinition { name: Ident, params: TupleType },
    EnumDefinition(Arc<EnumType>),
}

#[derive(Debug, PartialEq)]
pub struct EnumType {
    name: Ident,
    variants: Vec<EnumVariantType>,
}

#[derive(Debug, Clone, PartialEq)]
struct EnumVariantType {
    name: Ident,
    params: Option<TupleType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleItemType {
    name: Option<Ident>,
    typ: ScriptType,
}

impl TupleItemType {
    pub fn new(name: Option<Ident>, typ: ScriptType) -> Self {
        Self { name, typ }
    }

    pub fn named(name: Ident, typ: ScriptType) -> Self {
        Self::new(Some(name), typ)
    }

    pub fn unnamed(typ: ScriptType) -> Self {
        Self::new(None, typ)
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct TupleType(Vec<TupleItemType>);

impl From<Vec<TupleItemType>> for TupleType {
    fn from(args: Vec<TupleItemType>) -> Self {
        TupleType(args)
    }
}

impl TupleType {
    const fn identity() -> Self {
        Self(Vec::new())
    }

    fn accepts(&self, other: &TupleType) -> bool {
        // Exact same algorithm as validate_args, but returning boolean
        // instead of Result with code reference.

        let mut positional = other.0.iter().filter(|arg| arg.name.is_none());
        for par in self.0.iter() {
            if let Some(name) = &par.name {
                if let Some(arg) = other.0.iter().find(|a| a.name.as_ref() == Some(name)) {
                    if !par.typ.accepts(&arg.typ) {
                        return false;
                    }
                } else if let Some(arg) = positional.next() {
                    if !par.typ.accepts(&arg.typ) {
                        return false;
                    }
                } else {
                    return false;
                }
            } else if let Some(arg) = positional.next() {
                if !par.typ.accepts(&arg.typ) {
                    return false;
                }
            } else {
                return false;
            }
        }

        // All positional must be consumed
        positional.next().is_none()
    }
}

impl Display for TupleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_tuple(f, self.0.iter().map(|a| (a.name.clone(), &a.typ)))
    }
}

#[derive(Debug, Clone)]
pub struct ArgumentType {
    name: Option<Ident>,
    typ: Src<ScriptType>,
}

impl ArgumentType {
    fn into_formal(self) -> TupleItemType {
        if let Some(name) = self.name {
            TupleItemType::named(name, self.typ.cloned())
        } else {
            TupleItemType::unnamed(self.typ.cloned())
        }
    }
}

impl Src<Vec<ArgumentType>> {
    fn into_formal(self) -> TupleType {
        let args = self
            .into_inner()
            .into_iter()
            .map(|a| a.into_formal())
            .collect();

        TupleType(args)
    }
}

impl Display for Src<Vec<ArgumentType>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_tuple(
            f,
            self.as_ref()
                .iter()
                .map(|a| (a.name.clone(), a.typ.as_ref())),
        )
    }
}

#[derive(Default, Clone)]
struct Scope {
    locals: HashMap<Ident, ScriptType>,
    types: HashMap<Ident, TypeDefinition>,
    ret: Option<ScriptType>,
    arguments: TupleType,
}

impl Scope {
    fn with_globals(globals: &HashMap<Ident, ScriptType>) -> Self {
        let locals = globals.clone();
        Self {
            locals,
            types: Default::default(),
            ret: None,
            arguments: TupleType::identity(),
        }
    }

    fn get_local(&self, name: &Ident) -> Option<&ScriptType> {
        self.locals.get(name)
    }

    fn set_local(&mut self, name: Ident, value: ScriptType) {
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
                ScriptType::Opt(Box::new(ScriptType::clone(self.typ.as_ref()))),
                self.typ.loc,
            ),
            ..self
        }
    }
}

pub struct Validator<'a> {
    src: &'a str,
    offset: usize,
    globals: HashMap<Ident, ScriptType>,
}

impl<'a> Validator<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            offset: 0,
            globals: Default::default(),
        }
    }

    pub fn with_global(self, name: impl Into<Ident>, typ: ScriptType) -> Self {
        let mut globals = self.globals;
        globals.insert(name.into(), typ);
        Self { globals, ..self }
    }

    pub fn validate(&self, ast: &[AstNode]) -> Result<()> {
        self.validate_block(ast, Scope::with_globals(&self.globals))?;
        Ok(())
    }

    fn with_offset(src: &'a str, offset: usize) -> Self {
        Self {
            src,
            offset,
            globals: Default::default(),
        }
    }

    fn validate_block(&self, ast: &[AstNode], mut scope: Scope) -> Result<Scope> {
        for node in ast {
            match node {
                AstNode::Assignment { assignee, value } => {
                    let typ = self.validate_expr(value, &scope)?;
                    self.eval_assignment(assignee, &typ, &mut scope)?;
                }
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
                            inner.set_local(name.clone(), arg.typ.clone());
                        }
                    }
                    inner.arguments = params.clone();
                    inner.ret = declared_type.clone();
                    let inner = self.validate_block(&fun.body, inner.clone())?;

                    let found_ret_type = self.eval_return_type(&fun.body, &inner)?;
                    let found_type = found_ret_type
                        .as_ref()
                        .map(|r| r.typ.as_ref().clone())
                        .unwrap_or(ScriptType::identity());

                    if let Some(typ) = &declared_type
                        && !typ.accepts(&found_type)
                    {
                        let loc = fun
                            .type_expr
                            .as_ref()
                            .map(|t| t.loc)
                            .unwrap_or(Loc::start());
                        if let Some(ret) = found_ret_type {
                            return Err(self.fail(
                                format!(
                                    "Incompatible return type: Expected {typ}, found {found_type}"
                                ),
                                loc,
                            ));
                        } else {
                            let loc = fun
                                .type_expr
                                .as_ref()
                                .map(|t| t.loc)
                                .unwrap_or(Loc::start());
                            return Err(self.fail("Missing return statement".into(), loc));
                        }
                    }

                    let ret = match (declared_type, found_type) {
                        (Some(r), _) => r,
                        (None, r) => r,
                    };

                    scope.set_local(
                        name.clone(),
                        ScriptType::Function {
                            params,
                            ret: ret.into(),
                        },
                    );
                }
                AstNode::Rec(rec) => {
                    let params = self.eval_params(&rec.params, &scope)?;

                    scope.types.insert(
                        rec.name.clone(),
                        TypeDefinition::RecDefinition {
                            params,
                            name: rec.name.clone(),
                        },
                    );
                }
                AstNode::Enum(rec) => {
                    let def = EnumType {
                        name: rec.name.clone(),
                        variants: rec
                            .variants
                            .iter()
                            .map(|v| {
                                let params = v
                                    .params
                                    .as_ref()
                                    .map(|p| self.eval_params(p, &scope))
                                    .transpose()?;
                                Ok(EnumVariantType {
                                    name: v.name.clone(),
                                    params,
                                })
                            })
                            .collect::<Result<Vec<EnumVariantType>>>()?,
                    };
                    scope
                        .types
                        .insert(rec.name.clone(), TypeDefinition::EnumDefinition(def.into()));
                }
                AstNode::Iteration {
                    ident,
                    iterable,
                    body,
                } => {
                    let iterable_typ = self.validate_expr(iterable, &scope)?;

                    match iterable_typ {
                        ScriptType::EmptyList => {
                            return Err(self.fail("List is always empty".into(), iterable.loc));
                        }
                        ScriptType::List(inner) => {
                            let mut inner_scope = scope.clone();
                            inner_scope.set_local(ident.clone(), *inner);
                            self.validate_block(body, inner_scope)?;
                        }
                        ScriptType::Range => {
                            let mut inner_scope = scope.clone();
                            inner_scope.set_local(ident.clone(), ScriptType::Int);
                            self.validate_block(body, inner_scope)?;
                        }
                        _ => {
                            let msg = format!("Expected iterable, found {iterable_typ}");
                            return Err(self.fail(msg, iterable.loc));
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
                        return Err(self.fail(format!("Expected boolean, found {typ}"), cond.loc));
                    }

                    self.validate_block(body, scope.clone())?;

                    if let Some(else_body) = else_body.as_ref() {
                        self.validate_block(else_body, scope.clone())?;
                    }
                }
                AstNode::IfIn {
                    assignee,
                    value,
                    body,
                    else_body,
                } => {
                    let opt_typ = self.validate_expr(value, &scope)?;
                    let ScriptType::Opt(typ) = opt_typ else {
                        return Err(
                            self.fail(format!("Expected option type, found {opt_typ}"), value.loc)
                        );
                    };

                    let mut inner_scope = scope.clone();
                    self.eval_assignment(assignee, &typ, &mut inner_scope)?;
                    self.validate_block(body, inner_scope)?;

                    if let Some(else_body) = else_body.as_ref() {
                        self.validate_block(else_body, scope.clone())?;
                    }
                }
                AstNode::Expression(expr) => {
                    self.validate_expr(expr, &scope)?;
                }
                AstNode::Return(expr) => {
                    if let Some(expr) = expr {
                        let typ = self.validate_expr(expr, &scope)?;
                        match &scope.ret {
                            None => {
                                return Err(self.fail("Unexpected return value".into(), expr.loc));
                            }
                            Some(r) => {
                                if !r.accepts(&typ) {
                                    return Err(
                                        self.fail(format!("Expected {r}, found {typ}"), expr.loc)
                                    );
                                }
                            }
                        }
                    } else {
                        match &scope.ret {
                            None => {}
                            Some(ScriptType::Opt(_)) => {}
                            Some(_) => {
                                return Err(self.fail("Expected return value".into(), Loc::start()));
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
                if let Some(expr) = expr {
                    let typ = self.validate_expr(expr, scope)?;
                    Some(ReturnType::explicit(Src::new(typ, expr.loc)))
                } else {
                    todo!("return without value");
                }
            }
            Some(AstNode::Expression(expr)) => {
                if ast.len() == 1 {
                    let typ = self.validate_expr(expr, scope)?;
                    Some(ReturnType::implicit(Src::new(typ, expr.loc)))
                } else {
                    None
                }
            }
            Some(AstNode::Condition {
                body, else_body, ..
            }) => {
                let body_ret = self.eval_return_type(body, scope)?;
                let else_ret = else_body
                    .as_ref()
                    .map(|else_body| self.eval_return_type(else_body, scope))
                    .transpose()?
                    .flatten();

                match (body_ret, else_ret) {
                    (None, None) => None,
                    (Some(l), Some(r)) => {
                        let types = vec![l.typ.clone(), r.typ.clone()];
                        let typ = self
                            .most_specific_type(&types)?
                            .unwrap_or(Src::new(ScriptType::identity(), Loc::new(0, 0)));
                        if l.is_explicit && r.is_explicit {
                            Some(ReturnType::explicit(typ))
                        } else if ast.len() == 1 {
                            Some(ReturnType::implicit(typ))
                        } else {
                            None
                        }
                    }
                    (Some(typ), None) | (None, Some(typ)) => Some(typ.into_opt()),
                }
            }
            Some(AstNode::IfIn {
                assignee,
                value,
                body,
                else_body,
            }) => {
                let opt_typ = self.validate_expr(value, &scope)?;
                let ScriptType::Opt(typ) = opt_typ else {
                    return Err(
                        self.fail(format!("Expected option type, found {opt_typ}"), value.loc)
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
                    (None, None) => None,
                    (Some(l), Some(r)) => {
                        let types = vec![l.typ.clone(), r.typ.clone()];
                        let typ = self
                            .most_specific_type(&types)?
                            .unwrap_or(Src::new(ScriptType::identity(), Loc::new(0, 0)));
                        if l.is_explicit && r.is_explicit {
                            Some(ReturnType::explicit(typ))
                        } else if ast.len() == 1 {
                            Some(ReturnType::implicit(typ))
                        } else {
                            None
                        }
                    }
                    (Some(typ), None) | (None, Some(typ)) => Some(typ.into_opt()),
                }
            }
            Some(_) => None,
        };

        Ok(typ)
    }

    fn eval_params(&self, params: &[Parameter], scope: &Scope) -> Result<TupleType> {
        let mut res = TupleType::identity();

        for param in params {
            let typ = self.eval_type_expr(&param.type_expr, scope)?;
            res.0.push(TupleItemType::new(param.name.clone(), typ))
        }

        Ok(res)
    }

    fn eval_expr(&self, expr: &Src<Expression>, scope: &Scope) -> Result<Src<ScriptType>> {
        let typ = self.validate_expr(expr, scope)?;
        Ok(Src::new(typ, expr.loc))
    }

    fn validate_expr(&self, expr: &Src<Expression>, scope: &Scope) -> Result<ScriptType> {
        match expr.as_ref() {
            Expression::Number(_) => Ok(ScriptType::Int),
            Expression::Str(_) => Ok(ScriptType::Str),
            Expression::True => Ok(ScriptType::Bool),
            Expression::False => Ok(ScriptType::Bool),
            Expression::Arguments => Ok(ScriptType::Tuple(scope.arguments.clone())),
            Expression::String(parts) => {
                for (part, offset) in parts {
                    let inner = Self::with_offset(self.src, *offset);
                    let typ = inner.validate_expr(part, scope)?;
                    match &typ {
                        ScriptType::Opt(t) => {
                            return Err(self.fail(format!("Expected {t} got {typ}"), part.loc));
                        }
                        ScriptType::State(t) => todo!("Error for printing state({t})"),
                        _ => (),
                    }
                }
                Ok(ScriptType::Str)
            }
            Expression::List(expressions) => {
                let types = expressions
                    .iter()
                    .map(|i| self.eval_expr(i, scope))
                    .collect::<Result<Vec<_>>>()?;

                if let Some(inner_type) = self.most_specific_type(&types)? {
                    Ok(ScriptType::List(inner_type.cloned().into()))
                } else {
                    Ok(ScriptType::EmptyList)
                }
            }
            Expression::Tuple(args) => {
                let applied = self.eval_args(args, scope)?;

                Ok(ScriptType::Tuple(applied.into_formal()))
            }
            Expression::Ref(ident) => {
                if let Some(value) = scope.get_local(ident) {
                    Ok(value.clone())
                } else if let Some(typ) = scope.types.get(ident) {
                    match typ {
                        TypeDefinition::RecDefinition { name, params } => {
                            // XXX Returning record as function up-front
                            // Should probably return a record type instead,
                            // and provide something like ScriptType::as_callable()
                            let rec_type = ScriptType::Rec {
                                name: name.clone(),
                                params: params.clone(),
                            };
                            Ok(ScriptType::Function {
                                params: params.clone(),
                                ret: rec_type.into(),
                            })
                        }
                        TypeDefinition::EnumDefinition(_) => Err(self.fail(
                            format!("Undefined expression, found enum {ident}"),
                            expr.loc,
                        )),
                    }
                } else {
                    Err(self.fail(format!("Undefined reference: {ident}"), expr.loc))
                }
            }
            Expression::PrefixedName(prefix, name) => match scope.types.get(prefix) {
                Some(TypeDefinition::RecDefinition {
                    name: rec_name,
                    params: rec_params,
                }) => {
                    // TODO Associated methods like Record::foo()
                    match name.as_str() {
                        "parse" => {
                            let ret = ScriptType::Rec {
                                params: rec_params.clone(),
                                name: rec_name.clone(),
                            };
                            let params = TupleType(vec![TupleItemType::unnamed(ScriptType::Str)]);
                            Ok(ScriptType::Function {
                                params,
                                ret: ret.into(),
                            })
                        }
                        _ => Err(self.fail(
                            format!("Method not found: '{name}' for {rec_name}"),
                            expr.loc,
                        )),
                    }
                }
                Some(TypeDefinition::EnumDefinition(def)) => {
                    if let Some(var) = def.variants.iter().find(|v| v.name == *name) {
                        if var.params.is_none() {
                            Ok(ScriptType::Enum(Arc::clone(def)))
                        } else {
                            Ok(ScriptType::EnumVariant(Arc::clone(def), var.name.clone()))
                        }
                    } else {
                        Err(self.fail(
                            format!("Variant not found: {name} in {}", def.name),
                            expr.loc,
                        ))
                    }
                }
                None => Err(self.fail(format!("Undefined type: {prefix}"), expr.loc)),
            },
            Expression::Access { subject, key } => {
                let subject_typ = self.validate_expr(subject, scope)?;
                match &subject_typ {
                    ScriptType::Tuple(tuple) => {
                        match tuple.0.iter().find(|a| a.name.as_ref() == Some(key)) {
                            Some(a) => Ok(a.typ.clone()),
                            None => Err(self.fail(
                                format!("Unknown attribute: {key} on {subject_typ}"),
                                expr.loc,
                            )),
                        }
                    }
                    ScriptType::Rec { params, .. } => {
                        match params.0.iter().find(|a| a.name.as_ref() == Some(key)) {
                            Some(a) => Ok(a.typ.clone()),
                            None => Err(self.fail(
                                format!("Unknown attribute: {key} on {subject_typ}"),
                                expr.loc,
                            )),
                        }
                    }
                    _ => Err(self.fail(
                        format!("Unknown attribute: {key} on {subject_typ}"),
                        expr.loc,
                    )),
                }
            }
            Expression::Not(expr) => {
                let typ = self.validate_expr(expr, scope)?;
                if !ScriptType::Bool.accepts(&typ) {
                    return Err(self.fail(format!("Expected boolean, found {typ}"), expr.loc));
                }
                Ok(ScriptType::Bool)
            }
            Expression::Equal(lhs, rhs) | Expression::NotEqual(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if !(l.accepts(&r) || r.accepts(&l)) {
                    return Err(self.fail(format!("Expected {l}, found {r}"), rhs.loc));
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
                    return Err(self.fail(format!("Expected number, found {l}"), lhs.loc));
                }
                if !ScriptType::Int.accepts(&r) {
                    return Err(self.fail(format!("Expected number, found {r}"), rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            Expression::Range(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if l != ScriptType::Int {
                    return Err(self.fail(format!("Expected number, found {l}"), lhs.loc));
                }
                if r != ScriptType::Int {
                    return Err(self.fail(format!("Expected number, found {r}"), rhs.loc));
                }

                Ok(ScriptType::Range)
            }
            Expression::Negate(expr) => {
                let typ = self.validate_expr(expr, scope)?;
                if !ScriptType::Int.accepts(&typ) {
                    return Err(self.fail(format!("Expected number, found {typ}"), expr.loc));
                }
                Ok(typ)
            }
            Expression::Addition(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Subtraction(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Multiplication(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Division(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Try(inner) => {
                let inner_typ = self.validate_expr(inner, scope)?;
                match inner_typ {
                    ScriptType::Opt(inner) => Ok(*inner),
                    _ => Err(self.fail(format!("Expected optional, found {inner_typ}"), inner.loc)),
                }
            }
            Expression::Call { subject, arguments } => match subject.as_ref().as_ref() {
                Expression::Ref(name) => match name.as_str() {
                    "state" => {
                        if let ArgumentsKind::Inline(arguments) = arguments.as_ref() {
                            if arguments.args.len() != 1 {
                                return Err(self.fail(
                                    format!("Expected 1 argument, got {}", arguments.args.len()),
                                    expr.loc,
                                ));
                            }
                            let arg = arguments.args.first().expect("state arg");
                            let typ = self.validate_expr(&arg.expr, scope)?;

                            Ok(ScriptType::State(typ.into()))
                        } else {
                            Err(self.fail(format!("Expected one inline argument"), expr.loc))
                        }
                    }
                    _ => match scope.get_local(name) {
                        Some(ScriptType::Function { params, ret }) => {
                            self.validate_arguments(params, arguments, scope)?;

                            Ok(*ret.clone())
                        }
                        Some(t) => {
                            Err(self.fail(format!("Expected a callable, found {t}"), subject.loc))
                        }
                        None => match scope.types.get(name) {
                            Some(TypeDefinition::RecDefinition { params, name }) => {
                                self.validate_arguments(params, arguments, scope)?;

                                Ok(ScriptType::Rec {
                                    params: params.clone(),
                                    name: name.clone(),
                                })
                            }
                            Some(TypeDefinition::EnumDefinition(_)) => Err(self.fail(
                                format!("Expected a callable, found enum {name}"),
                                subject.loc,
                            )),
                            None => {
                                Err(self.fail(format!("Undefined reference: {name}"), subject.loc))
                            }
                        },
                    },
                },
                Expression::PrefixedName(prefix, name) => {
                    match scope.types.get(prefix) {
                        Some(TypeDefinition::RecDefinition {
                            name: rec_name,
                            params,
                        }) => {
                            // TODO Associated methods like Record::foo()
                            match name.as_str() {
                                "parse" => {
                                    // TODO Fallible type
                                    Ok(ScriptType::Rec {
                                        params: params.clone(),
                                        name: rec_name.clone(),
                                    })
                                }
                                _ => Err(self.fail(
                                    format!("Method not found: '{name}' for {rec_name}"),
                                    expr.loc,
                                )),
                            }
                        }
                        Some(TypeDefinition::EnumDefinition(def)) => {
                            if let Some(var) = def.variants.iter().find(|v| v.name == *name) {
                                if let Some(params) = &var.params {
                                    self.validate_arguments(params, arguments, scope)?;
                                    Ok(ScriptType::Enum(Arc::clone(def)))
                                } else {
                                    Err(self.fail(
                                        format!("Variant not callable: {}::{}", def.name, name),
                                        expr.loc,
                                    ))
                                }
                            } else {
                                Err(self.fail(
                                    format!("Variant not found: {name} in {}", def.name),
                                    expr.loc,
                                ))
                            }
                        }
                        None => Err(self.fail(format!("Undefined type: {prefix}"), expr.loc)),
                    }
                }
                Expression::Access { subject, key } => {
                    let subject_typ = self.validate_expr(subject, scope)?;
                    match (&subject_typ, key.as_str()) {
                        (ScriptType::State(typ), "get") => {
                            self.validate_arguments(&TupleType::identity(), arguments, scope)?;
                            Ok(*typ.clone())
                        }
                        (ScriptType::State(typ), "set") => {
                            // Simulate normal function call
                            let formal = TupleType(vec![TupleItemType::unnamed(*typ.clone())]);
                            self.validate_arguments(&formal, arguments, scope)?;
                            Ok(ScriptType::identity())
                        }
                        (ScriptType::EmptyList, "push") => {
                            if let ArgumentsKind::Inline(inline_arguments) = arguments.as_ref() {
                                // "Promote" list based on the first argument type
                                if let Some(typ) = inline_arguments
                                    .args
                                    .first()
                                    .map(|arg| self.validate_expr(&arg.expr, scope))
                                    .transpose()?
                                {
                                    // XXX Variadic args not supported (but allowed in runtime)
                                    let formal =
                                        TupleType(vec![TupleItemType::unnamed(typ.clone())]);
                                    self.validate_arguments(&formal, arguments, scope)?;
                                    Ok(ScriptType::List(typ.into()))
                                } else {
                                    Ok(ScriptType::EmptyList)
                                }
                            } else {
                                Err(self.fail(format!("Expected one inline argument"), expr.loc))
                            }
                        }
                        (ScriptType::List(typ), "push") => {
                            // XXX Variadic args not supported (but allowed in runtime)
                            let formal = TupleType(vec![TupleItemType::unnamed(*typ.clone())]);
                            self.validate_arguments(&formal, arguments, scope)?;
                            Ok(ScriptType::List(typ.clone()))
                        }
                        (ScriptType::List(typ), "find") => {
                            let formal = TupleType(vec![TupleItemType::unnamed(*typ.clone())]);
                            self.validate_arguments(&formal, arguments, scope)?;
                            Ok(ScriptType::Opt(typ.clone()))
                        }
                        (ScriptType::List(typ), "sum") if ScriptType::Int.accepts(typ) => {
                            // No arguments allowed
                            self.validate_arguments(&TupleType::identity(), arguments, scope)?;
                            Ok(ScriptType::Int)
                        }
                        (ScriptType::List(typ), "sort") if ScriptType::Int.accepts(typ) => {
                            // No arguments allowed
                            self.validate_arguments(&TupleType::identity(), arguments, scope)?;
                            Ok(ScriptType::List(typ.clone()))
                        }
                        (ScriptType::EmptyList, "map") => {
                            // TODO This should be a warning, "function is never called"
                            Ok(ScriptType::EmptyList)
                        }
                        (ScriptType::List(typ), "map") => {
                            if let ArgumentsKind::Inline(inline_arguments) = arguments.as_ref() {
                                if let Some(first) = inline_arguments
                                    .args
                                    .first()
                                    .map(|arg| {
                                        Ok(Src::new(
                                            self.validate_expr(&arg.expr, scope)?,
                                            arg.expr.loc,
                                        ))
                                    })
                                    .transpose()?
                                {
                                    // XXX FIXME This should all just be handled in 'ScriptType::accepts'
                                    if let ScriptType::Function { ret, params } = first.as_ref() {
                                        if params.0.len() != 1 {
                                            return Err(self.fail(
                                                "Expected function to take one argument".into(),
                                                first.loc,
                                            ));
                                        }
                                        if let Some(par) = params.0.first() {
                                            if !par.typ.accepts(typ.as_ref()) {
                                                return Err(self.fail(
                                                    format!("Expected function to take {typ}"),
                                                    first.loc,
                                                ));
                                            }
                                            Ok(ScriptType::List(ret.clone()))
                                        } else {
                                            Err(self.fail(
                                                "Expected function to take one argument".into(),
                                                first.loc,
                                            ))
                                        }
                                    } else {
                                        Err(self.fail(
                                            format!("Expected function, found {}", first.as_ref()),
                                            expr.loc,
                                        ))
                                    }
                                } else {
                                    Err(self.fail("Expected one inline argument".into(), expr.loc))
                                }
                            } else {
                                Err(self.fail("Expected one inline argument".into(), expr.loc))
                            }
                        }
                        (ScriptType::List(typ), "map_to") => {
                            let Some(tuple_typ) = typ.as_tuple() else {
                                return Err(self.fail(
                                    format!("Expected a list of tuples, found {subject_typ}"),
                                    subject.loc,
                                ));
                            };

                            if let ArgumentsKind::Inline(inline_arguments) = arguments.as_ref() {
                                if let Some(first) = inline_arguments
                                    .args
                                    .first()
                                    .map(|arg| {
                                        Ok(Src::new(
                                            self.validate_expr(&arg.expr, scope)?,
                                            arg.expr.loc,
                                        ))
                                    })
                                    .transpose()?
                                {
                                    if let Some((params, ret)) = first.as_ref().as_callable() {
                                        if !params.accepts(tuple_typ) {
                                            return Err(self.fail(
                                                format!(
                                                    "Expected a function with arguments {tuple_typ}"
                                                ),
                                                first.loc,
                                            ));
                                        }

                                        Ok(ScriptType::List(Box::new(ScriptType::clone(
                                            ret.as_ref(),
                                        ))))
                                    } else {
                                        Err(self.fail(
                                            format!("Expected function, found {}", first.as_ref()),
                                            expr.loc,
                                        ))
                                    }
                                } else {
                                    Err(self.fail("Expected one inline argument".into(), expr.loc))
                                }
                            } else {
                                Err(self.fail("Expected one inline argument".into(), expr.loc))
                            }
                        }
                        (ScriptType::List(typ), "unzip") => {
                            // No arguments allowed
                            self.validate_arguments(&TupleType::identity(), arguments, scope)?;

                            // [(int, int)] -> ([int], [int])
                            if let Some(tuple) = typ.as_tuple() {
                                let types = tuple
                                    .0
                                    .iter()
                                    .map(|t| {
                                        TupleItemType::unnamed(ScriptType::List(Box::new(
                                            t.typ.clone(),
                                        )))
                                    })
                                    .collect();

                                Ok(ScriptType::Tuple(TupleType(types)))
                            } else {
                                Err(self.fail(
                                    format!("Expected list of tuples, found {typ}"),
                                    expr.loc,
                                ))
                            }
                        }

                        // XXX Using empty list [], as a placeholder for a "static method"
                        (ScriptType::EmptyList, "zip") => {
                            // XXX FIXME: Unnecessary restriction
                            if let ArgumentsKind::Inline(inline_arguments) = arguments.as_ref() {
                                let args = self.eval_args(inline_arguments, scope)?;
                                let items = args
                                    .into_inner()
                                    .into_iter()
                                    .map(|arg| match arg.typ.as_ref() {
                                        ScriptType::List(inner) => {
                                            Ok(TupleItemType::new(arg.name, inner.as_ref().clone()))
                                        }
                                        _ => Err(self.fail(
                                            format!("Expected list, found {}", arg.typ.as_ref()),
                                            arg.typ.loc,
                                        )),
                                    })
                                    .collect::<Result<Vec<_>>>()?;

                                let tuple = TupleType(items);
                                let inner = ScriptType::Tuple(tuple);
                                Ok(ScriptType::List(inner.into()))
                            } else {
                                Err(self.fail("Expected inline arguments".into(), expr.loc))
                            }
                        }
                        (ScriptType::Str, "lines") => {
                            Ok(ScriptType::List(Box::new(ScriptType::Str)))
                        }
                        (ScriptType::Rec { params, .. }, "with") => {
                            // let args = self.eval_args(arguments, scope)?;

                            // XXX FIXME Non-mandatory args:
                            // self.validate_arguments(&formal, arguments, scope)?;

                            Ok(subject_typ.clone())
                        }
                        _ => {
                            Err(self
                                .fail(format!("Unknown method: {key} on {subject_typ}"), expr.loc))
                        }
                    }
                }
                _ => Err(self.fail("Call not allowed here".into(), expr.loc)),
            },
        }
    }

    fn validate_arithmetic(
        &self,
        lhs: &Src<Expression>,
        rhs: &Src<Expression>,
        scope: &Scope,
    ) -> Result<ScriptType> {
        let l = self.validate_expr(lhs, scope)?;
        let r = self.validate_expr(rhs, scope)?;

        if l != ScriptType::Int {
            return Err(self.fail(format!("Expected number, found {l}"), lhs.loc));
        }
        if r != ScriptType::Int {
            return Err(self.fail(format!("Expected number, found {r}"), rhs.loc));
        }

        Ok(ScriptType::Int)
    }

    fn eval_args(&self, arguments: &Arguments, scope: &Scope) -> Result<Src<Vec<ArgumentType>>> {
        let args = arguments
            .args
            .iter()
            .map(|arg| {
                let typ = self.eval_expr(&arg.expr, scope)?;
                Ok(ArgumentType {
                    name: arg.name.clone(),
                    typ,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Src::new(args, arguments.loc))
    }

    fn validate_arguments(
        &self,
        params: &TupleType,
        arguments: &ArgumentsKind,
        scope: &Scope,
    ) -> Result<()> {
        match arguments {
            ArgumentsKind::Inline(arguments) => {
                let arguments = self.eval_args(arguments, scope)?;
                self.validate_args(params, &arguments)?;
            }
            ArgumentsKind::Destructure(expr) => {
                let typ = self.eval_expr(expr, scope)?;
                let arg = ArgumentType { name: None, typ };
                self.validate_single_arg(&ScriptType::Tuple(params.clone()), &arg)?;
            }
            ArgumentsKind::DestructureImplicit(loc) => {
                let typ = scope.arguments.clone();
                let arg = ArgumentType {
                    name: None,
                    typ: Src::new(ScriptType::Tuple(typ), *loc),
                };
                self.validate_single_arg(&ScriptType::Tuple(params.clone()), &arg)?;
            }
        }
        Ok(())
    }

    fn validate_args(&self, formal: &TupleType, other: &Src<Vec<ArgumentType>>) -> Result<()> {
        // For each formal: If args contain named, take it, else take next unnamed
        let mut positional = other.as_ref().iter().filter(|arg| arg.name.is_none());

        for par in formal.0.iter() {
            if let Some(name) = &par.name {
                if let Some(arg) = other
                    .as_ref()
                    .iter()
                    .find(|a| a.name.as_ref() == Some(name))
                {
                    self.validate_single_arg(&par.typ, arg)?;
                } else if let Some(arg) = positional.next() {
                    self.validate_single_arg(&par.typ, arg)?;
                } else {
                    return Err(self.fail(
                        format!("Missing argument {name}, expected {formal} got {other}"),
                        other.loc,
                    ));
                }
            } else if let Some(arg) = positional.next() {
                self.validate_single_arg(&par.typ, arg)?;
            } else {
                return Err(self.fail(
                    format!("Missing positional argument, expected {formal} got {other}"),
                    other.loc,
                ));
            }
        }

        if let Some(arg) = positional.next() {
            return Err(self.fail(
                format!("Expected no arguments here, found {}", arg.typ.as_ref()),
                arg.typ.loc,
            ));
        }

        Ok(())
    }

    fn validate_single_arg(&self, typ: &ScriptType, arg: &ArgumentType) -> Result<()> {
        if !typ.accepts(arg.typ.as_ref()) {
            return Err(self.fail(
                format!("Expected {} got {}", typ, arg.typ.as_ref()),
                arg.typ.loc,
            ));
        }
        Ok(())
    }

    fn eval_type_expr(&self, type_expr: &Src<TypeExpression>, scope: &Scope) -> Result<ScriptType> {
        match type_expr.as_ref() {
            TypeExpression::Scalar(ident) => match ident.as_str() {
                "str" => Ok(ScriptType::Str),
                "int" => Ok(ScriptType::Int),
                "bool" => Ok(ScriptType::Bool),
                _ => match scope.types.get(ident) {
                    Some(TypeDefinition::RecDefinition { name, params }) => Ok(ScriptType::Rec {
                        params: params.clone(),
                        name: name.clone(),
                    }),
                    Some(TypeDefinition::EnumDefinition(def)) => {
                        Ok(ScriptType::Enum(Arc::clone(def)))
                    }
                    None => Err(self.fail(format!("Unknown type: {ident}"), type_expr.loc)),
                },
            },
            TypeExpression::Tuple(params) => {
                let types = self.eval_params(params, scope)?;
                Ok(ScriptType::Tuple(types))
            }
            TypeExpression::List(inner) => {
                let inner = self.eval_type_expr(inner.as_ref(), scope)?;
                Ok(ScriptType::List(inner.into()))
            }
            TypeExpression::Opt(inner) => {
                let inner = self.eval_type_expr(inner.as_ref(), scope)?;
                Ok(ScriptType::Opt(inner.into()))
            }
        }
    }

    // Check that all the given types are "compatible", meaning that it's possible
    // to construct a [list] containing elements of these types. Return the type
    // the element type that such a list would have.
    // Locations are included for error formatting. The returned tuple contains
    // the location of the first found element with the returned type.
    fn most_specific_type(&self, types: &[Src<ScriptType>]) -> Result<Option<Src<ScriptType>>> {
        let mut iter = types.iter();
        if let Some(first) = iter.next() {
            let mut typ = first;
            for t in iter {
                if t.as_ref().accepts(typ.as_ref()) {
                    typ = t;
                } else if !typ.as_ref().accepts(t.as_ref()) {
                    return Err(self.fail(
                        format!(
                            "Found incompatible types: {} and {}",
                            t.as_ref(),
                            typ.as_ref()
                        ),
                        t.loc,
                    ));
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
    ) -> Result<()> {
        let assignee = lhs.as_ref();
        match (&assignee.name, &assignee.pattern) {
            (None, None) => {}
            (Some(name), None) => scope.set_local(name.clone(), other.clone()),
            (_, Some(pattern)) => match other {
                ScriptType::Tuple(tuple) => {
                    self.eval_destruction(pattern, lhs.loc, tuple, scope)?
                }
                ScriptType::Rec { params, .. } => {
                    self.eval_destruction(pattern, lhs.loc, params, scope)?
                }
                _ => {
                    return Err(self.fail(
                        format!("Unexpected tuple pattern, can't destructure {other}"),
                        lhs.loc,
                    ));
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
    ) -> Result<()> {
        let mut positional = other.0.iter().filter(|arg| arg.name.is_none());

        for assignee in lhs.iter() {
            if let Some(name) = &assignee.as_ref().name {
                if let Some(item) = other.0.iter().find(|a| a.name.as_ref() == Some(name)) {
                    self.eval_assignment(assignee, &item.typ, scope)?;
                } else if let Some(item) = positional.next() {
                    self.eval_assignment(assignee, &item.typ, scope)?;
                } else {
                    return Err(self.fail(
                        format!("Element '{name}' not found in {other}"),
                        assignee.loc,
                    ));
                }
            } else if let Some(item) = positional.next() {
                self.eval_assignment(assignee, &item.typ, scope)?;
            } else {
                return Err(self.fail(
                    format!("Missing positional argument in {other}"),
                    assignee.loc,
                ));
            }
        }

        if positional.next().is_some() {
            return Err(self.fail(
                format!("Pattern did not contain enough positional params {other}"),
                loc,
            ));
        }

        Ok(())
    }

    fn fail(&self, msg: String, loc: Loc) -> Error {
        Error::new(msg, self.src, loc.shift_right(self.offset))
    }
}
