use std::{collections::HashMap, result, sync::Arc};

use crate::{
    error::{TypeError, TypeErrorKind},
    ident::Ident,
    lexer::Src,
    parser::{
        AttributeExpression, EnumExpression, Expression, Function, Literal, ParamExpression,
        Record, TypeExpression,
    },
    script_type::{
        EnumType, EnumVariantType, FunctionType, RecType, ScriptType, TupleItemType, TupleType,
        TypeAttribute,
    },
    script_value::{ScriptValue, Tuple, TupleItem},
};

type Result<T> = result::Result<T, TypeError>;

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    RecDefinition(Arc<RecType>),
    EnumDefinition(Arc<EnumType>),
}

#[derive(Clone, Default)]
pub(crate) struct TypeScope {
    types: HashMap<Ident, TypeDefinition>,
    expected_type: Option<ScriptType>,
}

impl TypeScope {
    pub(crate) fn with_expected(&self, exp: impl Into<Option<ScriptType>>) -> Self {
        Self {
            types: self.types.clone(),
            expected_type: exp.into(),
        }
    }

    pub(crate) fn get(&self, name: &Ident) -> Option<&TypeDefinition> {
        self.types.get(name)
    }

    pub(crate) fn add_enum(&mut self, name: impl Into<Ident>, def: Arc<EnumType>) {
        self.types
            .insert(name.into(), TypeDefinition::EnumDefinition(def));
    }

    pub(crate) fn eval_rec(&mut self, rec: &Record) -> Result<()> {
        let rec_type = RecType::new(&rec.name, eval_params(&rec.params, self)?);

        self.types
            .insert(rec.name.clone(), TypeDefinition::RecDefinition(rec_type));

        Ok(())
    }

    pub(crate) fn eval_enum(&mut self, def: &EnumExpression) -> Result<()> {
        let enum_type = EnumType::new(
            &def.name,
            def.variants
                .iter()
                .map(|v| {
                    let params = v
                        .params
                        .as_ref()
                        .map(|p| eval_params(p, self))
                        .transpose()?;
                    Ok(EnumVariantType::new(&v.name, params))
                })
                .collect::<Result<Vec<EnumVariantType>>>()?,
        );

        self.add_enum(&def.name, enum_type);

        Ok(())
    }
}

pub(crate) fn eval_function(source: &Function, scope: &TypeScope) -> Result<Arc<FunctionType>> {
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

pub(crate) fn eval_params(params: &[ParamExpression], scope: &TypeScope) -> Result<TupleType> {
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
) -> Result<ScriptType> {
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
            Ok(ScriptType::Opt(inner.into()))
        }
        TypeExpression::TypeName(ident) => match scope.get(ident) {
            Some(TypeDefinition::RecDefinition(rec)) => {
                Ok(ScriptType::RecInstance(Arc::clone(rec)))
            }
            Some(TypeDefinition::EnumDefinition(def)) => {
                Ok(ScriptType::EnumInstance(Arc::clone(def)))
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

fn eval_type_attrs(attrs: &[Src<AttributeExpression>]) -> Result<Vec<TypeAttribute>> {
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
                        .map(|a| {
                            Ok(TupleItem::new(
                                a.name.clone(),
                                try_static_eval(a.expr.as_ref())?,
                            ))
                        })
                        .collect::<Result<Vec<_>>>()?;
                    Ok(Tuple::new(items))
                })
                .transpose()?;

            Ok(TypeAttribute {
                name: expr.name.clone(),
                args,
            })
        })
        .collect::<Result<Vec<_>>>()
}

fn try_static_eval(expr: &Expression) -> Result<ScriptValue> {
    let opt = match expr {
        Expression::Literal(literal) => match literal {
            Literal::Str(s) => ScriptValue::string(Arc::clone(s)),
            _ => todo!(),
        },
        Expression::String(s) => {
            if s.len() == 1
                && let Some(Expression::Literal(Literal::Str(f))) = s.first().map(|k| &*k.0)
            {
                ScriptValue::string(Arc::clone(f))
            } else {
                panic!()
            }
        }
        _ => panic!(),
    };

    Ok(opt)
}
