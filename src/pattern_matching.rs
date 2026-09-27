use std::{
    collections::{HashMap, HashSet},
    iter,
    sync::Arc,
};

use crate::{
    error::{TypeError, TypeErrorKind, TypeResult},
    ident::Ident,
    lexer::{Loc, Src},
    parser::{Literal, MatchPattern, MatchPatternItem},
    script_type::{ScriptType, TupleType, UnionType},
};

// Validate patterns for a given expression type.
//
// Raise an error of patterns are invalid or duplicated. Return true if all possible patterns are
// exhausted, or false if patterns are valid, but incomplete for all possible values of the type.
pub(crate) fn validate_patterns(
    expr_type: &ScriptType,
    patterns: &[&Src<MatchPattern>],
) -> TypeResult<bool> {
    // Start with the entire space for the type
    let mut remaining = vec![TypeSpace::from_type(expr_type)];

    // Go through each pattern, subtracting from the uncovered type space
    for pattern in patterns {
        let pattern_space = TypeSpace::from_pattern(pattern, expr_type)?;

        let mut next_remaining = Vec::new();

        for space in &remaining {
            next_remaining.extend(subtract_space(space, &pattern_space));
        }

        next_remaining = normalize_spaces(next_remaining);

        // XXX spaces might be equivalent but not equal by direct comparison
        if next_remaining == remaining {
            let err = TypeError::new(TypeErrorKind::PatternAlreadyExhausted)
                .at(pattern.loc);
            return Err(match pattern.as_ref() {
                MatchPattern::Assignee(n) => {
                    err.with_hint(format!("'{n}' is treated as a variable name here"))
                }
                _ => err,
            });
        }

        remaining = next_remaining;
    }

    Ok(remaining.is_empty())
}

// Given a tuple type and a tuple-like sequence of match patterns, resolve the patterns
// so that they are the same length an order as the tuple items.
pub(crate) fn resolve_tuple_patterns(
    tuple_type: &TupleType,
    patterns: &[MatchPatternItem],
) -> TypeResult<Vec<Src<MatchPattern>>> {
    let mut resolved = Vec::new();
    let mut positional = patterns.iter().filter(|arg| arg.name.is_none());

    for item in tuple_type.items() {
        let opt_item = if let Some(name) = &item.name {
            patterns
                .iter()
                .find(|a| a.name.as_ref().map(|n| n.as_ref()) == Some(name))
                .or_else(|| positional.next())
        } else {
            positional.next()
        };

        if let Some(arg) = opt_item {
            resolved.push(arg.pattern.clone());
        } else {
            // Insert placeholder pattern for alignment.
            // Maybe this is an error in some cases!
            resolved.push(Src::new(MatchPattern::Discard, Loc::void()));
        }
    }

    // Check for extra named patterns
    for arg in patterns.iter().filter_map(|arg| arg.name.as_ref()) {
        if !tuple_type
            .items()
            .iter()
            .filter_map(|f| f.name.as_ref())
            .any(|n| n == arg.as_ref())
        {
            return Err(
                TypeError::invalid_pattern(ScriptType::Tuple(tuple_type.clone())).at(arg.loc),
            );
        }
    }

    // Check for extra positional patterns
    if let Some(arg) = positional.next() {
        return Err(
            TypeError::invalid_pattern(ScriptType::Tuple(tuple_type.clone())).at(arg
                .name
                .as_ref()
                .map(|n| n.loc)
                .unwrap_or(arg.pattern.loc)),
        );
    }

    Ok(resolved)
}

#[derive(Debug, Clone, PartialEq)]
enum TypeSpace {
    // Special space used for discard, not implemented as LHS of subtract
    Anything,
    Empty,
    Bool(BoolSpace),
    Int(CommonSpace<i64>),
    Char(CommonSpace<char>),
    String(CommonSpace<Arc<str>>),
    Fallible(FallibleSpace),
    Tuple(Vec<TypeSpace>),
    Union(HashMap<Ident, Vec<TypeSpace>>),

    None,
    Opt(Box<TypeSpace>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BoolSpace {
    Any,
    OnlyTrue,
    OnlyFalse,
}

#[derive(Debug, Clone, PartialEq)]
struct FallibleSpace(Box<TypeSpace>, Box<TypeSpace>);

#[derive(Debug, Clone, PartialEq)]
enum CommonSpace<T>
where
    T: Clone + Eq + std::hash::Hash,
{
    Any,
    Only(T),
    Except(HashSet<T>),
}

impl TypeSpace {
    fn is_empty(&self) -> bool {
        match self {
            Self::Anything => false,
            Self::Empty => true,

            Self::Fallible(FallibleSpace(val, err)) => val.is_empty() && err.is_empty(),

            Self::Tuple(fields) => tuple_space_is_empty(fields),

            Self::Union(variants) => {
                variants.is_empty() || variants.values().all(|s| tuple_space_is_empty(s))
            }

            Self::None => false,
            Self::Bool(_) => false,
            Self::Int(_) => false,
            Self::Char(_) => false,
            Self::String(_) => false,
            Self::Opt(_) => false,
        }
    }

    fn from_type(script_type: &ScriptType) -> Self {
        match script_type {
            ScriptType::Bool => Self::Bool(BoolSpace::Any),
            ScriptType::Int => Self::Int(CommonSpace::Any),
            ScriptType::Char => Self::Char(CommonSpace::Any),
            ScriptType::Str => Self::String(CommonSpace::Any),
            ScriptType::Fallible(val_type, err_type) => Self::Fallible(FallibleSpace(
                Self::from_type(val_type).into(),
                Self::from_type(err_type).into(),
            )),
            ScriptType::Tuple(tuple) => Self::Tuple(full_tuple_space(tuple)),
            ScriptType::RecInstance(def) => Self::Tuple(full_tuple_space(&def.params)),
            ScriptType::UnionInstance(def) => Self::Union(full_union_variant_space(def)),
            ScriptType::Opt(inner) => Self::Opt(Self::from_type(inner).into()),
            _ => unimplemented!("from_type: {script_type:?}"),
        }
    }

    fn from_pattern(pattern: &Src<MatchPattern>, script_type: &ScriptType) -> TypeResult<Self> {
        // XXX Any pattern must allow Opt

        match (pattern.as_ref(), script_type) {
            (MatchPattern::Discard, _) => Ok(TypeSpace::Anything),

            (_, ScriptType::Opt(t)) => Self::from_pattern(pattern, t),

            (MatchPattern::Assignee(_), _) => Ok(TypeSpace::from_type(script_type)),

            (MatchPattern::Literal(lit), _) => match (lit, script_type) {
                (Literal::True, ScriptType::Bool) => Ok(Self::Bool(BoolSpace::OnlyTrue)),
                (Literal::False, ScriptType::Bool) => Ok(Self::Bool(BoolSpace::OnlyFalse)),
                (Literal::Int(n), ScriptType::Int) => Ok(Self::Int(CommonSpace::Only(*n))),
                (Literal::Char(ch), ScriptType::Char) => Ok(Self::Char(CommonSpace::Only(*ch))),
                (Literal::Str(s), ScriptType::Str) => {
                    Ok(Self::String(CommonSpace::Only(s.clone())))
                }
                _ => Err(TypeError::invalid_pattern(script_type.clone())),
            },

            (MatchPattern::FallibleOk(val_pat), ScriptType::Fallible(val_type, _)) => {
                Ok(Self::Fallible(FallibleSpace(
                    Self::from_pattern(val_pat, val_type)?.into(),
                    Self::Empty.into(),
                )))
            }

            (MatchPattern::FallibleErr(err_pat), ScriptType::Fallible(_, err_type)) => {
                Ok(Self::Fallible(FallibleSpace(
                    Self::Empty.into(),
                    Self::from_pattern(err_pat, err_type)?.into(),
                )))
            }

            (MatchPattern::Variant(_, name, patterns), ScriptType::UnionInstance(def)) => {
                let var = def
                    .variants
                    .iter()
                    .find(|v| v.name == *name)
                    .ok_or_else(|| TypeError::invalid_pattern(script_type.clone()))?;

                let fields = if let (Some(params), Some(patterns)) = (&var.params, patterns) {
                    tuple_pattern_space(params, patterns)?
                } else {
                    vec![]
                };
                let mut variants = HashMap::new();
                variants.insert(name.clone(), fields);

                Ok(Self::Union(variants))
            }

            (MatchPattern::Tuple(patterns), ScriptType::Tuple(tuple_type)) => {
                let fields = tuple_pattern_space(tuple_type, patterns)?;

                Ok(Self::Tuple(fields))
            }

            _ => Err(TypeError::invalid_pattern(script_type.clone())),
        }
        .map_err(|err| err.at(pattern.loc))
    }
}

fn tuple_space_is_empty(fields: &[TypeSpace]) -> bool {
    fields.iter().any(TypeSpace::is_empty)
}

fn full_union_variant_space(def: &UnionType) -> HashMap<Ident, Vec<TypeSpace>> {
    let mut variants = HashMap::new();

    for variant in &def.variants {
        if let Some(params) = &variant.params {
            variants.insert(variant.name.clone(), full_tuple_space(params));
        } else {
            variants.insert(variant.name.clone(), vec![]);
        }
    }

    variants
}

fn full_tuple_space(tuple_type: &TupleType) -> Vec<TypeSpace> {
    tuple_type
        .items()
        .iter()
        .map(|i| TypeSpace::from_type(&i.value))
        .collect()
}

fn tuple_pattern_space(
    tuple_type: &TupleType,
    pattern_fields: &[MatchPatternItem],
) -> TypeResult<Vec<TypeSpace>> {
    let mut fields = Vec::with_capacity(pattern_fields.len());
    let resolved = resolve_tuple_patterns(tuple_type, pattern_fields)?;
    for (pattern, item) in resolved.iter().zip(tuple_type.items().iter()) {
        fields.push(TypeSpace::from_pattern(pattern, &item.value)?);
    }

    Ok(fields)
}

fn intersect_space(lhs: &TypeSpace, rhs: &TypeSpace) -> TypeSpace {
    use TypeSpace::*;

    match (lhs, rhs) {
        (Empty, _) | (_, Empty) => Empty,
        (Anything, s) | (s, Anything) => s.clone(),
        (Bool(lhs), Bool(rhs)) => intersect_bool(*lhs, *rhs),
        (Int(lhs), Int(rhs)) => intersect_other(lhs, rhs).map(Int).unwrap_or(Empty),
        (Char(lhs), Char(rhs)) => intersect_other(lhs, rhs).map(Char).unwrap_or(Empty),
        (String(lhs), String(rhs)) => intersect_other(lhs, rhs).map(String).unwrap_or(Empty),
        (Fallible(lhs), Fallible(rhs)) => intersect_fallible(lhs, rhs),
        (Tuple(lhs), Tuple(rhs)) => intersect_tuple(lhs, rhs),
        (Union(lhs), Union(rhs)) => intersect_union(lhs, rhs),

        (Opt(lhs), Opt(rhs)) => Opt(intersect_space(lhs, rhs).into()),
        (Opt(s), None) | (None, Opt(s)) => Opt(s.clone()),

        // Mismatched spaces cannot intersect.
        _ => Empty,
    }
}

fn intersect_bool(lhs: BoolSpace, rhs: BoolSpace) -> TypeSpace {
    use BoolSpace::*;

    match (lhs, rhs) {
        (Any, Any) => TypeSpace::Bool(Any),
        (Any, other) | (other, Any) => TypeSpace::Bool(other),
        (OnlyTrue, OnlyTrue) => TypeSpace::Bool(OnlyTrue),
        (OnlyFalse, OnlyFalse) => TypeSpace::Bool(OnlyFalse),
        _ => TypeSpace::Empty,
    }
}

fn intersect_fallible(lhs: &FallibleSpace, rhs: &FallibleSpace) -> TypeSpace {
    let val = intersect_space(&lhs.0, &rhs.0);
    let err = intersect_space(&lhs.1, &rhs.1);

    if val.is_empty() && err.is_empty() {
        TypeSpace::Empty
    } else {
        TypeSpace::Fallible(FallibleSpace(val.into(), err.into()))
    }
}

fn intersect_other<T>(lhs: &CommonSpace<T>, rhs: &CommonSpace<T>) -> Option<CommonSpace<T>>
where
    T: Clone + Eq + std::hash::Hash,
{
    use CommonSpace::*;

    match (lhs, rhs) {
        (Any, Any) => Some(Any),
        (Any, other) | (other, Any) => Some(other.clone()),

        (Only(a), Only(b)) => {
            if a == b {
                Some(Only(a.clone()))
            } else {
                None
            }
        }

        (Only(v), Except(e)) | (Except(e), Only(v)) => {
            if e.contains(v) {
                None
            } else {
                Some(Only(v.clone()))
            }
        }

        (Except(a), Except(b)) => {
            let mut excluded = a.clone();
            excluded.extend(b.iter().cloned());
            Some(Except(excluded))
        }
    }
}

fn intersect_union(
    lhs: &HashMap<Ident, Vec<TypeSpace>>,
    rhs: &HashMap<Ident, Vec<TypeSpace>>,
) -> TypeSpace {
    let mut result = HashMap::new();

    for (name, lhs_fields) in lhs {
        let Some(rhs_fields) = rhs.get(name) else {
            continue;
        };

        if let Some(fields) = intersect_tuple_fields(lhs_fields, rhs_fields) {
            result.insert(name.clone(), fields);
        }
    }

    if result.is_empty() {
        TypeSpace::Empty
    } else {
        TypeSpace::Union(result)
    }
}

fn intersect_tuple(lhs: &[TypeSpace], rhs: &[TypeSpace]) -> TypeSpace {
    if let Some(result) = intersect_tuple_fields(lhs, rhs) {
        TypeSpace::Tuple(result)
    } else {
        TypeSpace::Empty
    }
}

fn intersect_tuple_fields(lhs: &[TypeSpace], rhs: &[TypeSpace]) -> Option<Vec<TypeSpace>> {
    if lhs.len() != rhs.len() {
        return None;
    }

    let mut result = Vec::with_capacity(lhs.len());

    for (a, b) in lhs.iter().zip(rhs.iter()) {
        let field = intersect_space(a, b);

        if field.is_empty() {
            return None;
        }

        result.push(field);
    }

    Some(result)
}

fn subtract_space(lhs: &TypeSpace, rhs: &TypeSpace) -> Vec<TypeSpace> {
    use TypeSpace::*;

    match (lhs, rhs) {
        (_, Anything) => vec![],
        (Empty, _) => vec![],
        (_, Empty) => vec![lhs.clone()],

        (Bool(lhs), Bool(rhs)) => subtract_bool(*lhs, *rhs),
        (Int(lhs), Int(rhs)) => subtract_other(lhs, rhs).into_iter().map(Int).collect(),
        (Char(lhs), Char(rhs)) => subtract_other(lhs, rhs).into_iter().map(Char).collect(),
        (String(lhs), String(rhs)) => subtract_other(lhs, rhs).into_iter().map(String).collect(),
        (Fallible(lhs), Fallible(rhs)) => subtract_fallible(lhs, rhs),
        (Tuple(lhs), Tuple(rhs)) => subtract_tuple(lhs, rhs),
        (Union(lhs), Union(rhs)) => subtract_union(lhs, rhs),

        (Opt(t), None) => vec![*t.clone()],
        (Opt(lhs), Opt(rhs)) => subtract_space(lhs, rhs).into_iter().collect(),
        (Opt(lhs), rhs) => subtract_space(lhs, rhs)
            .into_iter()
            .chain(iter::once(None))
            .collect(),
        (None, None) => vec![],
        (None, Opt(_)) => vec![],

        // Different shapes cannot overlap, so subtracting removes nothing.
        _ => vec![lhs.clone()],
    }
}

fn subtract_bool(lhs: BoolSpace, rhs: BoolSpace) -> Vec<TypeSpace> {
    use BoolSpace::*;

    match (lhs, rhs) {
        (_, Any) => vec![],
        (OnlyTrue, OnlyTrue) => vec![],
        (OnlyFalse, OnlyFalse) => vec![],
        (_, OnlyTrue) => vec![TypeSpace::Bool(OnlyFalse)],
        (_, OnlyFalse) => vec![TypeSpace::Bool(OnlyTrue)],
    }
}

fn subtract_fallible(lhs: &FallibleSpace, rhs: &FallibleSpace) -> Vec<TypeSpace> {
    let val = subtract_space(&lhs.0, &rhs.0);
    let err = subtract_space(&lhs.1, &rhs.1);

    let mut result = vec![];

    for v in &val {
        result.push(TypeSpace::Fallible(FallibleSpace(
            v.clone().into(),
            TypeSpace::Empty.into(),
        )));
    }

    for e in &err {
        result.push(TypeSpace::Fallible(FallibleSpace(
            TypeSpace::Empty.into(),
            e.clone().into(),
        )));
    }

    normalize_spaces(result)
}

fn subtract_other<T>(lhs: &CommonSpace<T>, rhs: &CommonSpace<T>) -> Vec<CommonSpace<T>>
where
    T: Clone + Eq + std::hash::Hash,
{
    use CommonSpace::*;

    match (lhs, rhs) {
        (_, Any) => vec![],
        (Only(a), Only(b)) => {
            if a == b {
                vec![]
            } else {
                vec![Only(a.clone())]
            }
        }
        (Any, Only(v)) => {
            let mut excluded = HashSet::new();
            excluded.insert(v.clone());
            vec![Except(excluded)]
        }
        (Any, Except(v)) => v.iter().cloned().map(Only).collect(),

        (Only(v), Except(e)) => {
            if e.contains(v) {
                vec![Only(v.clone())]
            } else {
                vec![]
            }
        }
        (Except(e), Only(v)) => {
            let mut excluded = e.clone();
            excluded.insert(v.clone());
            vec![Except(excluded)]
        }
        (Except(a), Except(b)) => b
            .iter()
            .filter(|value| !a.contains(value))
            .cloned()
            .map(Only)
            .collect(),
    }
}

fn subtract_tuple(lhs: &[TypeSpace], rhs: &[TypeSpace]) -> Vec<TypeSpace> {
    let fields = subtract_tuple_fields(lhs, rhs);
    normalize_spaces(fields.into_iter().map(TypeSpace::Tuple).collect())
}

fn subtract_union(
    lhs: &HashMap<Ident, Vec<TypeSpace>>,
    rhs: &HashMap<Ident, Vec<TypeSpace>>,
) -> Vec<TypeSpace> {
    let mut result = Vec::new();

    for (name, lhs_fields) in lhs {
        if let Some(rhs_fields) = rhs.get(name) {
            debug_assert_eq!(lhs_fields.len(), rhs_fields.len());

            for fields in subtract_tuple_fields(lhs_fields, rhs_fields) {
                let mut variant_space = HashMap::new();
                variant_space.insert(name.clone(), fields);
                result.push(TypeSpace::Union(variant_space));
            }
        } else {
            // rhs does not cover this variant at all, so the whole variant remains.
            let mut union = HashMap::new();
            union.insert(name.clone(), lhs_fields.clone());
            result.push(TypeSpace::Union(union));
        }
    }

    normalize_spaces(result)
}

fn subtract_tuple_fields(lhs: &[TypeSpace], rhs: &[TypeSpace]) -> Vec<Vec<TypeSpace>> {
    debug_assert_eq!(lhs.len(), rhs.len());

    let mut result = Vec::new();
    let mut prefix = Vec::new();

    for i in 0..lhs.len() {
        let lhs_field = &lhs[i];
        let rhs_field = &rhs[i];

        // Branches where this field does not match rhs_field.
        for outside in subtract_space(lhs_field, rhs_field) {
            let mut tuple_fields = Vec::new();

            tuple_fields.extend(prefix.iter().cloned());
            tuple_fields.push(outside);
            tuple_fields.extend(lhs[i + 1..].iter().cloned());

            result.push(tuple_fields);
        }

        // Continue with the branch where this field does match rhs_field.
        let inside = intersect_space(lhs_field, rhs_field);

        if inside.is_empty() {
            // rhs variant payload cannot match lhs payload at all.
            // Therefore the whole lhs payload remains.
            return vec![lhs.to_vec()];
        }

        prefix.push(inside);
    }

    result
}

fn normalize_spaces(spaces: Vec<TypeSpace>) -> Vec<TypeSpace> {
    let mut result = Vec::new();

    for space in spaces {
        if space.is_empty() {
            continue;
        }

        if !result.contains(&space) {
            result.push(space);
        }
    }

    result
}
