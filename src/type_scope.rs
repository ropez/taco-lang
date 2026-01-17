use std::{collections::HashMap, fmt::Write, sync::Arc};

use crate::{
    error::{TypeError, TypeErrorKind, TypeResult},
    ident::Ident,
    lexer::Src,
    parser::{
        AttributeExpression, UnionExpression, Expression, Function, Literal, ParamExpression,
        Record, TypeExpression,
    },
    script_type::{
        UnionType, UnionVariantType, FunctionType, RecType, ScriptType, TupleItemType, TupleType,
        TypeAttribute,
    },
    script_value::{ScriptValue, Tuple, TupleItem},
};

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    RecDefinition(Arc<RecType>),
    UnionDefinition(Arc<UnionType>),
}

#[derive(Clone)]
pub(crate) struct TypeScope {
    types: HashMap<Ident, TypeDefinition>,
    expected_type: Option<ScriptType>,
}

impl TypeScope {
    pub(crate) fn new(types: HashMap<Ident, TypeDefinition>) -> Self {
        Self {
            types,
            expected_type: None,
        }
    }

    pub(crate) fn with_expected(&self, exp: impl Into<Option<ScriptType>>) -> Self {
        Self {
            types: self.types.clone(),
            expected_type: exp.into(),
        }
    }

    pub(crate) fn get(&self, name: &Ident) -> Option<&TypeDefinition> {
        self.types.get(name)
    }

    pub(crate) fn add_union(&mut self, name: impl Into<Ident>, def: Arc<UnionType>) {
        self.types
            .insert(name.into(), TypeDefinition::UnionDefinition(def));
    }

    pub(crate) fn eval_rec(&mut self, rec: &Record) -> TypeResult<()> {
        let rec_type = RecType::new(&rec.name, eval_params(&rec.params, self)?);

        self.types
            .insert(rec.name.clone(), TypeDefinition::RecDefinition(rec_type));

        Ok(())
    }

    pub(crate) fn eval_union(&mut self, def: &UnionExpression) -> TypeResult<()> {
        let union_type = UnionType::new(
            &def.name,
            def.variants
                .iter()
                .map(|v| {
                    let params = v
                        .params
                        .as_ref()
                        .map(|p| eval_params(p, self))
                        .transpose()?;
                    Ok(UnionVariantType::new(&v.name, params))
                })
                .collect::<TypeResult<Vec<UnionVariantType>>>()?,
        );

        self.add_union(&def.name, union_type);

        Ok(())
    }
}

pub(crate) fn eval_function(source: &Function, scope: &TypeScope) -> TypeResult<Arc<FunctionType>> {
    let params = eval_params(&source.params, scope)?;
    let ret = source
        .type_expr
        .as_ref()
        .map(|expr| eval_type_expr(expr, scope))
        .transpose()?
        .unwrap_or_else(ScriptType::identity);

    let function = FunctionType::new(params, ret);

    Ok(function)
}

pub(crate) fn eval_params(params: &[ParamExpression], scope: &TypeScope) -> TypeResult<TupleType> {
    let mut items = Vec::new();

    let expected_params = if let Some(ScriptType::Function(fun)) = &scope.expected_type {
        Some(fun.params.items())
    } else {
        None
    };

    if let Some(expected) = expected_params {
        let mut expected_positional = expected.iter().filter(|arg| arg.name.is_none());
        for param in params {
            let exp = if let Some(name) = &param.name {
                expected
                    .iter()
                    .find(|e| e.name.as_ref() == Some(name))
                    .or_else(|| expected_positional.next())
            } else {
                expected_positional.next()
            };
            let typ = eval_type_expr(
                &param.type_expr,
                &scope.with_expected(exp.map(|it| it.value.clone())),
            )?;

            // XXX Should evaluate _type_ of expression, not only value
            // E.g. This should fail validation, but fails in runtime:
            // @json(name: 2)

            let attrs = eval_type_attrs(&param.attrs)?;

            items.push(TupleItemType::new_with_attrs(
                param.name.clone(),
                typ,
                attrs,
            ))
        }
    } else {
        for param in params {
            let typ = eval_type_expr(&param.type_expr, scope)?;
            let attrs = eval_type_attrs(&param.attrs)?;

            items.push(TupleItemType::new_with_attrs(
                param.name.clone(),
                typ,
                attrs,
            ))
        }
    }

    Ok(TupleType::new(items))
}

pub(crate) fn eval_type_expr(
    type_expr: &Src<TypeExpression>,
    scope: &TypeScope,
) -> TypeResult<ScriptType> {
    match type_expr.as_ref() {
        TypeExpression::Int => Ok(ScriptType::Int),
        TypeExpression::Str => Ok(ScriptType::Str),
        TypeExpression::Bool => Ok(ScriptType::Bool),
        TypeExpression::Range => Ok(ScriptType::Range),
        TypeExpression::Tuple(params) => {
            let types = eval_params(params, scope)?;
            Ok(ScriptType::Tuple(types))
        }
        TypeExpression::List(inner) => {
            let inner = eval_type_expr(inner.as_ref(), scope)?;
            Ok(ScriptType::list_of(inner))
        }
        TypeExpression::Opt(inner) => {
            let inner = eval_type_expr(inner.as_ref(), scope)?;
            Ok(ScriptType::opt_of(inner))
        }
        TypeExpression::Fallible(val, err) => {
            let inner_value = eval_type_expr(val.as_ref(), scope)?;
            let inner_error = eval_type_expr(err.as_ref(), scope)?;
            Ok(ScriptType::fallible_of(inner_value, inner_error))
        }
        TypeExpression::TypeName(ident) => match scope.get(ident) {
            Some(TypeDefinition::RecDefinition(rec)) => {
                Ok(ScriptType::RecInstance(Arc::clone(rec)))
            }
            Some(TypeDefinition::UnionDefinition(def)) => {
                Ok(ScriptType::UnionInstance(Arc::clone(def)))
            }
            None => Err(
                TypeError::new(TypeErrorKind::UndefinedReference(ident.clone())).at(type_expr.loc),
            ),
        },
        TypeExpression::Infer => {
            if let Some(t) = &scope.expected_type {
                Ok(t.clone())
            } else {
                Err(TypeError::new(TypeErrorKind::TypeNotInferred).at(type_expr.loc))
            }
        }
    }
}

fn eval_type_attrs(attrs: &[Src<AttributeExpression>]) -> TypeResult<Vec<TypeAttribute>> {
    attrs
        .iter()
        .map(|expr| {
            let args = expr
                .args
                .as_ref()
                .map(|args| {
                    let items = args
                        .as_ref()
                        .iter()
                        .map(|a| Ok(TupleItem::new(a.name.clone(), try_static_eval(&a.expr)?)))
                        .collect::<TypeResult<Vec<_>>>()?;
                    Ok(Tuple::new(items))
                })
                .transpose()?;

            Ok(TypeAttribute {
                name: expr.name.clone(),
                args,
            })
        })
        .collect::<TypeResult<Vec<_>>>()
}

fn try_static_eval(expr: &Src<Expression>) -> TypeResult<ScriptValue> {
    let err_mapper = |_| TypeError::new(TypeErrorKind::InvalidStaticExpression).at(expr.loc);

    let opt = match expr.as_ref() {
        Expression::Literal(literal) => match literal {
            Literal::Str(s) => ScriptValue::string(Arc::clone(s)),
            Literal::True => ScriptValue::Boolean(true),
            Literal::False => ScriptValue::Boolean(false),
            Literal::Int(n) => ScriptValue::Int(*n),
            Literal::Char(c) => ScriptValue::Char(*c),
        },
        Expression::String(parts) => {
            let mut builder = String::new();
            for (expr, offset) in parts {
                let val = try_static_eval(expr).map_err(|err| err.at_offset(*offset))?;
                write!(builder, "{}", val).unwrap();
            }
            ScriptValue::string(builder)
        }
        Expression::Addition(lhs, rhs) => {
            let lhs = try_static_eval(lhs)?.as_int().map_err(err_mapper)?;
            let rhs = try_static_eval(rhs)?.as_int().map_err(err_mapper)?;
            ScriptValue::Int(lhs + rhs)
        }
        Expression::Subtraction(lhs, rhs) => {
            let lhs = try_static_eval(lhs)?.as_int().map_err(err_mapper)?;
            let rhs = try_static_eval(rhs)?.as_int().map_err(err_mapper)?;
            ScriptValue::Int(lhs - rhs)
        }
        _ => Err(TypeError::new(TypeErrorKind::InvalidStaticExpression).at(expr.loc))?,
    };

    Ok(opt)
}
