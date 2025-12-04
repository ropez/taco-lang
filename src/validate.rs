use std::{collections::HashMap, fmt::Display, result, sync::Arc};

use crate::{
    error::TypeError,
    fmt::fmt_tuple,
    ident::{Ident, global},
    lexer::{Loc, Src},
    parser::{
        ArgumentExpression, Assignee, AstNode, CallExpression, Expression, Function,
        ParamExpression, TypeExpression,
    },
    stdlib::{NativeFunctionRef, NativeMethodRef},
};

type Result<T> = result::Result<T, Src<TypeError>>;

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
        params: TupleType, // XXX Remove this and look up definition like Enum
    },
    State(Box<ScriptType>),
    NativeFunction(NativeFunctionRef),
    NativeMethodBound(NativeMethodRef, Box<ScriptType>),

    // Special type used to represent a "generic", e.g. state(x: T): [T] is represented as
    // Function ( params: (Generic), ret: List<Generic> }
    Generic(u16),
}

impl ScriptType {
    // Use () to represent nothing, like Rust
    pub const fn identity() -> Self {
        Self::Tuple(TupleType::identity())
    }

    pub fn list_of(inner: ScriptType) -> Self {
        Self::List(inner.into())
    }

    fn accepts(&self, other: &ScriptType) -> bool {
        match (self, other) {
            (ScriptType::State(_), _) => false,
            (ScriptType::Generic(_), _) => true,
            (ScriptType::Bool, ScriptType::Bool) => true,
            (ScriptType::List(_), ScriptType::EmptyList) => true,
            (ScriptType::List(l), ScriptType::List(r)) => l.accepts(r),
            (ScriptType::Tuple(l), ScriptType::Tuple(r)) => l.accepts(r),
            (ScriptType::Tuple(l), ScriptType::Rec { params, .. }) => l.accepts(params),
            (ScriptType::Enum(l), ScriptType::Enum(r)) => Arc::ptr_eq(l, r),
            (ScriptType::Enum(_), ScriptType::EnumVariant(_, _)) => false,
            // (
            //     ScriptType::Function {
            //         params: lp,
            //         ret: lr,
            //     },
            //     r @ ScriptType::Rec { name, params: rp },
            // ) => rp.accepts(lp) && lr.accepts(r),
            (
                ScriptType::Function {
                    params: lp,
                    ret: lr,
                },
                ScriptType::EnumVariant(a, b),
            ) => {
                let variant = a.variants.iter().find(|v| v.name == *b);
                if let Some(variant) = variant {
                    if let Some(rp) = &variant.params {
                        rp.accepts(lp) && lr.accepts(&ScriptType::Enum(Arc::clone(a)))
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            (
                ScriptType::Function {
                    params: lp,
                    ret: lr,
                },
                rhs,
            ) => {
                if let Ok((rp, rr)) = rhs.as_callable() {
                    rp.accepts(lp) && lr.accepts(&rr)
                } else {
                    false
                }
            }

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

    pub fn as_callable(&self) -> result::Result<(TupleType, ScriptType), TypeError> {
        // XXX Too much cloning
        match &self {
            ScriptType::Function { params, ret } => Ok((params.clone(), ScriptType::clone(ret))),
            ScriptType::EnumVariant(def, name) => {
                if let Some(var) = def.variants.iter().find(|v| v.name == *name) {
                    let params = var.params.as_ref().unwrap();
                    Ok((params.clone(), ScriptType::Enum(Arc::clone(def))))
                } else {
                    unreachable!()
                }
            }
            ScriptType::NativeFunction(func) => {
                let params = func.arguments_type();
                let ret = func.return_type();
                Ok((params.clone(), ret))
            }
            ScriptType::NativeMethodBound(method, subject_typ) => {
                let params = method.arguments_type(subject_typ)?;
                let ret = method.return_type(subject_typ)?;
                Ok((params.clone(), ret))
            }
            _ => Err(TypeError::InvalidCallable(self.clone())),
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
            Self::Generic(n) => write!(f, "T{n}"),
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
    pub name: Option<Ident>,
    pub value: ScriptType,
}

impl TupleItemType {
    pub fn new(name: Option<Ident>, value: ScriptType) -> Self {
        Self { name, value }
    }

    pub fn named(name: Ident, value: ScriptType) -> Self {
        Self::new(Some(name), value)
    }

    pub fn unnamed(value: ScriptType) -> Self {
        Self::new(None, value)
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
    pub const fn identity() -> Self {
        Self(Vec::new())
    }

    pub fn from_single(item: ScriptType) -> Self {
        Self(vec![TupleItemType::unnamed(item)])
    }

    pub fn items(&self) -> &[TupleItemType] {
        &self.0
    }

    pub fn get_named_item(&self, name: &Ident) -> Option<&TupleItemType> {
        self.items().iter().find(|a| a.name.as_ref() == Some(name))
    }

    fn accepts(&self, other: &TupleType) -> bool {
        // Exact same algorithm as validate_args, but returning boolean
        // instead of Result with code reference.

        let mut positional = other.0.iter().filter(|arg| arg.name.is_none());
        for par in self.0.iter() {
            if let Some(name) = &par.name {
                if let Some(arg) = other.0.iter().find(|a| a.name.as_ref() == Some(name)) {
                    if !par.value.accepts(&arg.value) {
                        return false;
                    }
                } else if let Some(arg) = positional.next() {
                    if !par.value.accepts(&arg.value) {
                        return false;
                    }
                } else {
                    return false;
                }
            } else if let Some(arg) = positional.next() {
                if !par.value.accepts(&arg.value) {
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
        fmt_tuple(f, self.0.iter().map(|a| (a.name.clone(), &a.value)))
    }
}

#[derive(Debug, Clone)]
pub struct ArgumentExpressionType {
    pub name: Option<Ident>,
    pub value: Src<ScriptType>,
}

impl ArgumentExpressionType {
    fn into_tuple_item_type(self) -> TupleItemType {
        if let Some(name) = self.name {
            TupleItemType::named(name, self.value.cloned())
        } else {
            TupleItemType::unnamed(self.value.cloned())
        }
    }
}

impl Src<Vec<ArgumentExpressionType>> {
    fn into_tuple_type(self) -> TupleType {
        let args = self
            .into_inner()
            .into_iter()
            .map(|a| a.into_tuple_item_type())
            .collect();

        TupleType(args)
    }
}

// This is an "intermediate" struct, used while validating callable.
// It is not part of the scope, but only used to represent the evaluated
// call expression before validation.
#[derive(Debug)]
pub enum CallExpressionType {
    Inline(Src<Vec<ArgumentExpressionType>>),
    Destructure(Src<ScriptType>),
    DestructureImplicit(Loc),
}

impl Display for Src<Vec<ArgumentExpressionType>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_tuple(
            f,
            self.as_ref()
                .iter()
                .map(|a| (a.name.clone(), a.value.as_ref())),
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

#[derive(Default)]
pub struct Validator {
    globals: HashMap<Ident, ScriptType>,
    methods: HashMap<(Ident, Ident), NativeMethodRef>,
}

impl Validator {
    pub(crate) fn with_functions(self, functions: HashMap<Ident, NativeFunctionRef>) -> Self {
        let mut globals = self.globals;
        globals.extend(
            functions
                .into_iter()
                .map(|(name, f)| (name, ScriptType::NativeFunction(f))),
        );
        Self { globals, ..self }
    }

    pub(crate) fn with_methods(self, more: HashMap<(Ident, Ident), NativeMethodRef>) -> Self {
        let mut methods = self.methods;
        methods.extend(more);

        Self { methods, ..self }
    }

    fn get_method(&self, subject: &ScriptType, name: &Ident) -> Option<&NativeMethodRef> {
        let ns = match subject {
            ScriptType::Str => global::STRING,
            ScriptType::Range => global::RANGE,
            ScriptType::Rec { .. } => global::REC, // XXX Should also support user-defined methods on the rec's own name
            ScriptType::EmptyList | ScriptType::List(_) => global::LIST,
            ScriptType::State(_) => global::STATE,
            _ => todo!("NS for {subject}"),
        };
        self.methods.get(&(ns.into(), name.clone()))
    }

    pub fn validate(&self, ast: &[AstNode]) -> Result<()> {
        self.validate_block(ast, Scope::with_globals(&self.globals))?;
        Ok(())
    }

    fn validate_block(&self, ast: &[AstNode], mut scope: Scope) -> Result<Scope> {
        for node in ast {
            match node {
                AstNode::Assignment { assignee, value } => {
                    let typ = self.validate_expr(value, &scope)?;
                    self.eval_assignment(assignee, &typ, &mut scope)?;
                }
                AstNode::Function { name, fun } => {
                    let (params, ret) = self.eval_function(fun, &scope)?;

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
                            return Err(TypeError::EmptyList.at(iterable.loc));
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
                            return Err(TypeError::InvalidIterable(iterable_typ).at(iterable.loc));
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
                        return Err(TypeError::InvalidArgumentType {
                            expected: ScriptType::Bool,
                            actual: typ.clone(),
                        }
                        .at(cond.loc));
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
                        return Err(TypeError::InvalidOptional(opt_typ).at(value.loc));
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
                                return Err(TypeError::InvalidReturnType {
                                    expected: ScriptType::identity(),
                                    actual: typ,
                                }
                                .at(expr.loc));
                            }
                            Some(r) => {
                                if !r.accepts(&typ) {
                                    return Err(TypeError::InvalidReturnType {
                                        expected: r.clone(),
                                        actual: typ,
                                    }
                                    .at(expr.loc));
                                }
                            }
                        }
                    } else {
                        match &scope.ret {
                            None => {}
                            Some(ScriptType::Opt(_)) => {}
                            Some(_) => {
                                // XXX Location
                                return Err(TypeError::MissingReturnStatement.at(Loc::start()));
                            }
                        }
                    }
                }
            }
        }

        Ok(scope)
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
                    let typ = self.validate_expr(part, scope).map_err(|err| {
                        let loc = err.loc.shift_right(*offset);
                        Src::new(err.into_inner(), loc)
                    })?;
                    match &typ {
                        ScriptType::Opt(t) => {
                            return Err(TypeError::InvalidArgumentType {
                                expected: *t.clone(),
                                actual: typ,
                            }
                            .at(part.loc));
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
                    Ok(ScriptType::list_of(inner_type.cloned()))
                } else {
                    Ok(ScriptType::EmptyList)
                }
            }
            Expression::Tuple(args) => {
                let applied = self.eval_inline_args(args, scope)?;

                Ok(ScriptType::Tuple(applied.into_tuple_type()))
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
                        TypeDefinition::EnumDefinition(_) => {
                            Err(TypeError::InvalidExpression.at(expr.loc))
                        }
                    }
                } else {
                    Err(TypeError::UndefinedReference(ident.clone()).at(expr.loc))
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
                        _ => Err(TypeError::UndefinedMethod {
                            type_name: rec_name.clone(),
                            method_name: name.clone(),
                        }
                        .at(expr.loc)),
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
                        Err(TypeError::UndefinedVariant {
                            type_name: def.name.clone(),
                            variant_name: name.clone(),
                        }
                        .at(expr.loc))
                    }
                }
                None => Err(TypeError::UndefinedReference(prefix.clone()).at(expr.loc)),
            },
            Expression::Access { subject, key } => {
                let subject = self.validate_expr(subject, scope)?;
                if let Some(tuple) = subject.as_tuple()
                    && let Some(item) = tuple.get_named_item(key)
                {
                    return Ok(item.value.clone());
                }

                if let Some(method) = self.get_method(&subject, key) {
                    Ok(ScriptType::NativeMethodBound(
                        method.clone(),
                        subject.into(),
                    ))
                } else {
                    Err(TypeError::UndefinedAttribute {
                        subject,
                        attr_name: key.clone(),
                    }
                    .at(expr.loc))
                }
            }
            Expression::Function(fun) => {
                let (params, ret) = self.eval_function(fun, scope)?;
                Ok(ScriptType::Function {
                    params,
                    ret: ret.into(),
                })
            }
            Expression::Not(expr) => {
                let typ = self.validate_expr(expr, scope)?;
                if !ScriptType::Bool.accepts(&typ) {
                    return Err(TypeError::InvalidArgumentType {
                        expected: ScriptType::Bool,
                        actual: typ.clone(),
                    }
                    .at(expr.loc));
                }
                Ok(ScriptType::Bool)
            }
            Expression::Equal(lhs, rhs) | Expression::NotEqual(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if !(l.accepts(&r) || r.accepts(&l)) {
                    return Err(TypeError::InvalidArgumentType {
                        expected: l,
                        actual: r,
                    }
                    .at(rhs.loc));
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
                    return Err(TypeError::expected_number(l.clone()).at(lhs.loc));
                }
                if !ScriptType::Int.accepts(&r) {
                    return Err(TypeError::expected_number(r.clone()).at(rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            Expression::Range(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if !ScriptType::Int.accepts(&l) {
                    return Err(TypeError::expected_number(l.clone()).at(lhs.loc));
                }
                if !ScriptType::Int.accepts(&r) {
                    return Err(TypeError::expected_number(r.clone()).at(rhs.loc));
                }

                Ok(ScriptType::Range)
            }
            Expression::Negate(expr) => {
                let typ = self.validate_expr(expr, scope)?;
                if !ScriptType::Int.accepts(&typ) {
                    return Err(TypeError::expected_number(typ.clone()).at(expr.loc));
                }
                Ok(typ)
            }
            Expression::Addition(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Subtraction(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Multiplication(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Division(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Modulo(lhs, rhs) => self.validate_arithmetic(lhs, rhs, scope),
            Expression::Try(inner) => {
                let inner_typ = self.validate_expr(inner, scope)?;
                match inner_typ {
                    ScriptType::Opt(inner) => Ok(*inner),
                    _ => Err(TypeError::InvalidOptional(inner_typ).at(inner.loc)),
                }
            }
            Expression::Call { subject, arguments } => {
                let arguments = self.eval_call_expr(arguments, scope)?;

                let subject = self.eval_expr(subject, scope)?;
                let (params, ret) = subject
                    .as_ref()
                    .as_callable()
                    .map_err(|err| err.at(expr.loc))?;
                let found_types = self.validate_arguments(&params, &arguments, scope)?;
                if let Ok(generic) = resolve_generic(ret, &found_types) {
                    Ok(generic)
                } else {
                    todo!("No generic found in: {arguments:?}")
                }
            }
        }
    }

    fn eval_function(&self, fun: &Arc<Function>, scope: &Scope) -> Result<(TupleType, ScriptType)> {
        let params = self.eval_params(&fun.params, scope)?;
        let declared_type = fun
            .type_expr
            .as_ref()
            .map(|expr| self.eval_type_expr(expr, scope))
            .transpose()?;
        let mut inner = scope.clone();
        for arg in &params.0 {
            if let Some(name) = &arg.name {
                inner.set_local(name.clone(), arg.value.clone());
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
                return Err(TypeError::InvalidReturnType {
                    expected: typ.clone(),
                    actual: found_type,
                }
                .at(loc));
            } else {
                let loc = fun
                    .type_expr
                    .as_ref()
                    .map(|t| t.loc)
                    .unwrap_or(Loc::start());
                return Err(TypeError::MissingReturnStatement.at(loc));
            }
        }
        let ret = match (declared_type, found_type) {
            (Some(r), _) => r,
            (None, r) => r,
        };
        Ok((params, ret))
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
                    return Err(TypeError::InvalidOptional(opt_typ).at(value.loc));
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

    fn eval_params(&self, params: &[ParamExpression], scope: &Scope) -> Result<TupleType> {
        let mut res = TupleType::identity();

        for param in params {
            let typ = self.eval_type_expr(&param.type_expr, scope)?;
            res.0.push(TupleItemType::new(param.name.clone(), typ))
        }

        Ok(res)
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
            return Err(TypeError::expected_number(l).at(lhs.loc));
        }
        if r != ScriptType::Int {
            return Err(TypeError::expected_number(r).at(rhs.loc));
        }

        Ok(ScriptType::Int)
    }

    fn eval_call_expr(
        &self,
        arguments: &CallExpression,
        scope: &Scope,
    ) -> Result<CallExpressionType> {
        let res = match arguments {
            CallExpression::Inline(arguments) => {
                let inline = self.eval_inline_args(arguments, scope)?;
                CallExpressionType::Inline(inline)
            }
            CallExpression::Destructure(expr) => {
                let typ = self.eval_expr(expr, scope)?;
                CallExpressionType::Destructure(typ)
            }
            CallExpression::DestructureImplicit(loc) => {
                CallExpressionType::DestructureImplicit(*loc)
            }
        };
        Ok(res)
    }

    fn eval_inline_args(
        &self,
        arguments: &Src<Vec<ArgumentExpression>>,
        scope: &Scope,
    ) -> Result<Src<Vec<ArgumentExpressionType>>> {
        let args = arguments
            .as_ref()
            .iter()
            .map(|arg| {
                Ok(ArgumentExpressionType {
                    name: arg.name.clone(),
                    value: self.eval_expr(&arg.expr, scope)?,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Src::new(args, arguments.loc))
    }

    fn validate_arguments(
        &self,
        params: &TupleType,
        arguments: &CallExpressionType,
        scope: &Scope,
    ) -> Result<HashMap<u16, ScriptType>> {
        let found_type = match arguments {
            CallExpressionType::Inline(arguments) => {
                self.validate_inline_args(params, arguments)?
            }
            CallExpressionType::Destructure(t) => {
                let arg = ArgumentExpressionType {
                    name: None,
                    value: t.clone(),
                };
                self.validate_single_arg(&ScriptType::Tuple(params.clone()), &arg)?;
                HashMap::new() // XXX
            }
            CallExpressionType::DestructureImplicit(loc) => {
                let typ = scope.arguments.clone();
                let arg = ArgumentExpressionType {
                    name: None,
                    value: Src::new(ScriptType::Tuple(typ), *loc),
                };
                self.validate_single_arg(&ScriptType::Tuple(params.clone()), &arg)?;
                HashMap::new() // XXX
            }
        };
        Ok(found_type)
    }

    fn validate_inline_args(
        &self,
        formal: &TupleType,
        arguments: &Src<Vec<ArgumentExpressionType>>,
    ) -> Result<HashMap<u16, ScriptType>> {
        // For each formal: If args contain named, take it, else take next unnamed
        let mut positional = arguments.as_ref().iter().filter(|arg| arg.name.is_none());
        let mut found_types = HashMap::new();

        for par in formal.0.iter() {
            if let Some(name) = &par.name {
                if let Some(arg) = arguments
                    .as_ref()
                    .iter()
                    .find(|a| a.name.as_ref() == Some(name))
                {
                    self.validate_single_arg(&par.value, arg)?;
                } else if let Some(arg) = positional.next() {
                    self.validate_single_arg(&par.value, arg)?;
                } else {
                    return Err(TypeError::MissingArgument {
                        name: Some(name.clone()),
                        expected: formal.clone(),
                        actual: arguments.clone().into_tuple_type(),
                    }
                    .at(arguments.loc));
                }
            } else if let Some(arg) = positional.next() {
                self.validate_single_arg(&par.value, arg)?;
                if let ScriptType::Generic(n) = par.value {
                    found_types.insert(n, arg.value.cloned());
                }
                if let ScriptType::List(l) = &par.value {
                    if let ScriptType::Generic(n) = l.as_ref() {
                        if let ScriptType::List(a) = &arg.value.as_ref() {
                            found_types.insert(*n, a.as_ref().clone());
                        }
                    }
                }
                if let ScriptType::Function { ret, params: _ } = &par.value {
                    if let ScriptType::Generic(n) = ret.as_ref() {
                        if let ScriptType::Function { ret: a, params: _ } = arg.value.as_ref() {
                            found_types.insert(*n, *a.clone());
                        } else if let ScriptType::NativeFunction(f) = arg.value.as_ref() {
                            found_types.insert(*n, f.return_type());
                        } else if let ScriptType::EnumVariant(a, b) = arg.value.as_ref() {
                            found_types.insert(*n, ScriptType::Enum(Arc::clone(a)));
                        }
                    }
                }
            } else {
                return Err(TypeError::MissingArgument {
                    name: None,
                    expected: formal.clone(),
                    actual: arguments.clone().into_tuple_type(),
                }
                .at(arguments.loc));
            }
        }

        if let Some(_) = positional.next() {
            return Err(TypeError::UnexpectedArgument {
                expected: formal.clone(),
                actual: arguments.clone().into_tuple_type(),
            }
            .at(arguments.loc));
        }

        Ok(found_types)
    }

    fn validate_single_arg(&self, formal: &ScriptType, arg: &ArgumentExpressionType) -> Result<()> {
        if !formal.accepts(arg.value.as_ref()) {
            return Err(Src::new(
                TypeError::InvalidArgumentType {
                    expected: formal.clone(),
                    actual: arg.value.cloned(),
                },
                arg.value.loc,
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
                "range" => Ok(ScriptType::Range),
                _ => match scope.types.get(ident) {
                    Some(TypeDefinition::RecDefinition { name, params }) => Ok(ScriptType::Rec {
                        params: params.clone(),
                        name: name.clone(),
                    }),
                    Some(TypeDefinition::EnumDefinition(def)) => {
                        Ok(ScriptType::Enum(Arc::clone(def)))
                    }
                    None => Err(TypeError::UndefinedReference(ident.clone()).at(type_expr.loc)),
                },
            },
            TypeExpression::Tuple(params) => {
                let types = self.eval_params(params, scope)?;
                Ok(ScriptType::Tuple(types))
            }
            TypeExpression::List(inner) => {
                let inner = self.eval_type_expr(inner.as_ref(), scope)?;
                Ok(ScriptType::list_of(inner))
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
                    return Err(TypeError::InvalidArgumentType {
                        expected: typ.cloned(),
                        actual: t.cloned(),
                    }
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
                    return Err(TypeError::InvalidDestructure(other.clone()).at(lhs.loc));
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
                    self.eval_assignment(assignee, &item.value, scope)?;
                } else if let Some(item) = positional.next() {
                    self.eval_assignment(assignee, &item.value, scope)?;
                } else {
                    return Err(TypeError::MissingDestructureArgument {
                        name: Some(name.clone()),
                        actual: other.clone(),
                    }
                    .at(assignee.loc));
                }
            } else if let Some(item) = positional.next() {
                self.eval_assignment(assignee, &item.value, scope)?;
            } else {
                return Err(TypeError::MissingDestructureArgument {
                    name: None,
                    actual: other.clone(),
                }
                .at(assignee.loc));
            }
        }

        if positional.next().is_some() {
            return Err(TypeError::UnexpectedDestructureArgument {
                actual: other.clone(),
            }
            .at(loc));
        }

        Ok(())
    }
}

fn resolve_generic(
    t: ScriptType,
    found_types: &HashMap<u16, ScriptType>,
) -> result::Result<ScriptType, ()> {
    match t {
        ScriptType::Generic(n) => {
            let ret = found_types.get(&n).ok_or(())?;
            Ok(ret.clone())
        }
        ScriptType::State(inner) => Ok(ScriptType::State(
            resolve_generic(*inner, found_types)?.into(),
        )),
        ScriptType::Tuple(tuple) => {
            let items: Vec<_> = tuple
                .items()
                .iter()
                .map(|it| {
                    TupleItemType::new(
                        it.name.clone(),
                        resolve_generic(it.value.clone(), found_types).unwrap(),
                    )
                })
                .collect();
            Ok(ScriptType::Tuple(TupleType::from(items)))
        }
        ScriptType::List(inner) => Ok(ScriptType::list_of(resolve_generic(*inner, found_types)?)),
        ScriptType::Function { params: _, ret } => resolve_generic(*ret, found_types),
        ScriptType::NativeFunction(f) => resolve_generic(f.return_type(), found_types),
        other => Ok(other),
    }
}
