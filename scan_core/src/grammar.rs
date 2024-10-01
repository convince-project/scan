//! The language used by PGs and CSs.
//!
//! The type [`Expression<V>`] encodes the used language,
//! where `V` is the type parameter of variables.
//! The language features base types and product types,
//! Boolean logic and basic arithmetic expressions.

use ordered_float::OrderedFloat;
use std::hash::Hash;

/// The types supported by the language internally used by PGs and CSs.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    /// Boolean type.
    Boolean,
    /// Integer numerical type.
    Integer,
    /// Floating-point numerical type.
    Float,
    /// Product of a list of types (including other products).
    Product(Vec<Type>),
    /// List type
    List(Box<Type>),
}

impl Type {
    /// The default value for a given type.
    /// Used to initialize variables.
    pub fn default_value(&self) -> Val {
        match self {
            Type::Boolean => Val::Boolean(false),
            Type::Integer => Val::Integer(0),
            Type::Float => Val::Float(OrderedFloat(0.0)),
            Type::Product(tuple) => {
                Val::Tuple(Vec::from_iter(tuple.iter().map(Self::default_value)))
            }
            Type::List(t) => Val::List((**t).to_owned(), Vec::new()),
        }
    }
}

/// Integer values.
pub type Integer = i32;

/// Floating-point values.
pub type Float = f64;

/// Possible values for each [`Type`].
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Val {
    /// Boolean values.
    Boolean(bool),
    /// Integer values.
    Integer(Integer),
    /// Floating-point values.
    Float(OrderedFloat<Float>),
    /// Values for product types, i.e., tuples of suitable values.
    Tuple(Vec<Val>),
    /// Values for list types
    List(Type, Vec<Val>),
}

impl Val {
    pub(crate) fn r#type(&self) -> Type {
        match self {
            Val::Boolean(_) => Type::Boolean,
            Val::Integer(_) => Type::Integer,
            Val::Tuple(comps) => Type::Product(comps.iter().map(Val::r#type).collect()),
            Val::List(t, _) => Type::List(Box::new(t.to_owned())),
            Val::Float(_) => Type::Float,
        }
    }
}

impl From<Float> for Val {
    fn from(value: Float) -> Self {
        Val::Float(OrderedFloat(value))
    }
}

/// Expressions for the language internally used by PGs and CSs.
///
/// [`Expression<V>`] encodes the language in which `V` is the type of variables.
///
/// Note that not all expressions that can be formed are well-typed.
#[derive(Debug, Clone)]
pub enum Expression<V>
where
    V: Clone,
{
    // -------------------
    // General expressions
    // -------------------
    /// A constant value.
    Const(Val),
    /// A variable.
    Var(V),
    /// A tuple of expressions.
    Tuple(Vec<Expression<V>>),
    /// The component of a tuple.
    Component(usize, Box<Expression<V>>),
    // -----------------
    // Logical operators
    // -----------------
    /// n-uary logical conjunction.
    And(Vec<Expression<V>>),
    /// n-uary logical disjunction.
    Or(Vec<Expression<V>>),
    /// Logical implication.
    Implies(Box<(Expression<V>, Expression<V>)>),
    /// Logical negation.
    Not(Box<Expression<V>>),
    // --------------------
    // Arithmetic operators
    // --------------------
    /// Opposite of a numerical expression.
    Opposite(Box<Expression<V>>),
    /// Arithmetic n-ary sum.
    Sum(Vec<Expression<V>>),
    /// Arithmetic n-ary multiplication.
    Mult(Vec<Expression<V>>),
    // ------------
    // (In)Equality
    // ------------
    /// Equality of numerical expressions.
    Equal(Box<(Expression<V>, Expression<V>)>),
    /// Disequality of numerical expressions: LHS greater than RHS.
    Greater(Box<(Expression<V>, Expression<V>)>),
    /// Disequality of numerical expressions: LHS greater than, or equal to,  RHS.
    GreaterEq(Box<(Expression<V>, Expression<V>)>),
    /// Disequality of numerical expressions: LHS less than RHS.
    Less(Box<(Expression<V>, Expression<V>)>),
    /// Disequality of numerical expressions: LHS less than, or equal to, RHS.
    LessEq(Box<(Expression<V>, Expression<V>)>),
    // -----
    // Lists
    // -----
    /// Append element to the end of a list.
    Append(Box<(Expression<V>, Expression<V>)>),
    /// Truncate last element from a list.
    Truncate(Box<Expression<V>>),
    /// Take length of a list.
    Len(Box<Expression<V>>),
    // /// The component of a tuple.
    // Entry(Box<(Expression<V>, Expression<V>)>),
}

impl<V> From<bool> for Expression<V>
where
    V: Clone + Copy,
{
    fn from(value: bool) -> Self {
        Expression::Const(Val::Boolean(value))
    }
}

impl<V> From<Integer> for Expression<V>
where
    V: Clone + Copy,
{
    fn from(value: Integer) -> Self {
        Expression::Const(Val::Integer(value))
    }
}

impl<V> From<Float> for Expression<V>
where
    V: Clone + Copy,
{
    fn from(value: Float) -> Self {
        Expression::Const(Val::Float(OrderedFloat(value)))
    }
}

type DynFnExpr<C> = dyn Fn(&C) -> Option<Val> + Send + Sync;

pub(crate) struct FnExpression<C>(Box<DynFnExpr<C>>);

impl<C> std::fmt::Debug for FnExpression<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Expression")
    }
}

impl<C> FnExpression<C> {
    #[inline(always)]
    pub fn eval(&self, vars: &C) -> Option<Val> {
        self.0(vars)
    }
}

pub(crate) trait ValsContainer<V> {
    fn value(&self, var: V) -> Option<Val>;
}

impl<K: Clone + Send + Sync + 'static, V: ValsContainer<K> + 'static> TryFrom<Expression<K>>
    for FnExpression<V>
{
    // TODO FIXME: Use more significative error type
    type Error = ();

    fn try_from(value: Expression<K>) -> Result<Self, Self::Error> {
        Ok(FnExpression(match value {
            Expression::Const(val) => Box::new(move |_| Some(val.to_owned())),
            Expression::Var(var) => Box::new(move |vars| vars.value(var.clone())),
            Expression::Tuple(exprs) => {
                let exprs: Vec<FnExpression<_>> = exprs
                    .into_iter()
                    .map(FnExpression::try_from)
                    .collect::<Result<_, _>>()?;
                Box::new(move |vars| {
                    Some(Val::Tuple(
                        exprs
                            .iter()
                            .map(|expr| expr.eval(vars))
                            .collect::<Option<Vec<_>>>()?,
                    ))
                })
            }
            Expression::Component(index, expr) => {
                let expr = Self::try_from(*expr)?;
                Box::new(move |vars| {
                    if let Val::Tuple(vals) = expr.eval(vars)? {
                        vals.get(index).cloned()
                    } else {
                        None
                    }
                })
            }
            Expression::And(exprs) => {
                let exprs: Vec<FnExpression<_>> = exprs
                    .into_iter()
                    .map(Self::try_from)
                    .collect::<Result<_, _>>()?;
                Box::new(move |vars| {
                    for expr in exprs.iter() {
                        if let Val::Boolean(b) = expr.eval(vars)? {
                            if b {
                                continue;
                            } else {
                                return Some(Val::Boolean(false));
                            }
                        } else {
                            return None;
                        }
                    }
                    Some(Val::Boolean(true))
                })
            }
            Expression::Or(exprs) => {
                let exprs: Vec<FnExpression<_>> = exprs
                    .into_iter()
                    .map(Self::try_from)
                    .collect::<Result<_, _>>()?;
                Box::new(move |vars| {
                    for expr in exprs.iter() {
                        if let Val::Boolean(b) = expr.eval(vars)? {
                            if b {
                                return Some(Val::Boolean(true));
                            } else {
                                continue;
                            }
                        } else {
                            return None;
                        }
                    }
                    Some(Val::Boolean(false))
                })
            }
            Expression::Implies(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::try_from(lhs)?;
                let rhs = FnExpression::try_from(rhs)?;
                Box::new(move |vars| {
                    if let (Val::Boolean(lhs), Val::Boolean(rhs)) =
                        (lhs.eval(vars)?, rhs.eval(vars)?)
                    {
                        Some(Val::Boolean(rhs || !lhs))
                    } else {
                        None
                    }
                })
            }
            Expression::Not(expr) => {
                let expr = FnExpression::try_from(*expr)?;
                Box::new(move |vars| {
                    if let Val::Boolean(b) = expr.eval(vars)? {
                        Some(Val::Boolean(!b))
                    } else {
                        None
                    }
                })
            }
            Expression::Opposite(expr) => {
                let expr = FnExpression::try_from(*expr)?;
                Box::new(move |vars| match expr.eval(vars)? {
                    Val::Integer(i) => Some(Val::Integer(-i)),
                    Val::Float(f) => Some(Val::Float(-f)),
                    _ => None,
                })
            }
            Expression::Sum(exprs) => {
                let exprs: Vec<FnExpression<_>> = exprs
                    .into_iter()
                    .map(Self::try_from)
                    .collect::<Result<_, _>>()?;
                Box::new(move |vars| {
                    exprs
                        .iter()
                        .try_fold(Val::Integer(0), |val, expr| match val {
                            Val::Integer(acc) => match expr.eval(vars)? {
                                Val::Integer(i) => Some(Val::Integer(acc + i)),
                                Val::Float(f) => Some(Val::Float(OrderedFloat::from(acc) + f)),
                                _ => None,
                            },
                            Val::Float(acc) => match expr.eval(vars)? {
                                Val::Integer(i) => Some(Val::Float(acc + OrderedFloat::from(i))),
                                Val::Float(f) => Some(Val::Float(acc + f)),
                                _ => None,
                            },
                            _ => None,
                        })
                })
            }
            Expression::Mult(exprs) => {
                let exprs: Vec<FnExpression<_>> = exprs
                    .into_iter()
                    .map(Self::try_from)
                    .collect::<Result<_, _>>()?;
                Box::new(move |vars| {
                    exprs
                        .iter()
                        .try_fold(Val::Integer(0), |val, expr| match val {
                            Val::Integer(acc) => match expr.eval(vars)? {
                                Val::Integer(i) => Some(Val::Integer(acc * i)),
                                Val::Float(f) => Some(Val::Float(OrderedFloat::from(acc) * f)),
                                _ => None,
                            },
                            Val::Float(acc) => match expr.eval(vars)? {
                                Val::Integer(i) => Some(Val::Float(acc * OrderedFloat::from(i))),
                                Val::Float(f) => Some(Val::Float(acc * f)),
                                _ => None,
                            },
                            _ => None,
                        })
                })
            }
            Expression::Equal(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::try_from(lhs)?;
                let rhs = FnExpression::try_from(rhs)?;
                Box::new(move |vars| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) =
                        (lhs.eval(vars)?, rhs.eval(vars)?)
                    {
                        Some(Val::Boolean(lhs == rhs))
                    } else {
                        None
                    }
                })
            }
            Expression::Greater(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::try_from(lhs)?;
                let rhs = FnExpression::try_from(rhs)?;
                Box::new(move |vars| match lhs.eval(vars)? {
                    Val::Integer(lhs) => match rhs.eval(vars)? {
                        Val::Integer(rhs) => Some(Val::Boolean(lhs > rhs)),
                        Val::Float(rhs) => Some(Val::Boolean(OrderedFloat::from(lhs) > rhs)),
                        _ => None,
                    },
                    Val::Float(lhs) => match rhs.eval(vars)? {
                        Val::Integer(rhs) => Some(Val::Boolean(lhs > OrderedFloat::from(rhs))),
                        Val::Float(rhs) => Some(Val::Boolean(lhs > rhs)),
                        _ => None,
                    },
                    _ => None,
                })
            }
            Expression::GreaterEq(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::try_from(lhs)?;
                let rhs = FnExpression::try_from(rhs)?;
                Box::new(move |vars| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) =
                        (lhs.eval(vars)?, rhs.eval(vars)?)
                    {
                        Some(Val::Boolean(lhs >= rhs))
                    } else {
                        None
                    }
                })
            }
            Expression::Less(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::try_from(lhs)?;
                let rhs = FnExpression::try_from(rhs)?;
                Box::new(move |vars| match lhs.eval(vars)? {
                    Val::Integer(lhs) => match rhs.eval(vars)? {
                        Val::Integer(rhs) => Some(Val::Boolean(lhs < rhs)),
                        Val::Float(rhs) => Some(Val::Boolean(OrderedFloat::from(lhs) < rhs)),
                        _ => None,
                    },
                    Val::Float(lhs) => match rhs.eval(vars)? {
                        Val::Integer(rhs) => Some(Val::Boolean(lhs < OrderedFloat::from(rhs))),
                        Val::Float(rhs) => Some(Val::Boolean(lhs < rhs)),
                        _ => None,
                    },
                    _ => None,
                })
            }
            Expression::LessEq(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::try_from(lhs)?;
                let rhs = FnExpression::try_from(rhs)?;
                Box::new(move |vars| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) =
                        (lhs.eval(vars)?, rhs.eval(vars)?)
                    {
                        Some(Val::Boolean(lhs <= rhs))
                    } else {
                        None
                    }
                })
            }
            Expression::Append(exprs) => {
                let (list, element) = *exprs;
                let list = FnExpression::try_from(list)?;
                let element = FnExpression::try_from(element)?;
                Box::new(move |vars| {
                    if let Val::List(t, l) = list.eval(vars)? {
                        let element = element.eval(vars)?;
                        if element.r#type() == t {
                            l.to_owned().extend_from_slice(&[element]);
                            Some(Val::List(t, l))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
            }
            Expression::Truncate(list) => {
                let list = FnExpression::try_from(*list)?;
                Box::new(move |vars| {
                    if let Val::List(t, l) = list.eval(vars)? {
                        if !l.is_empty() {
                            Some(Val::List(t, l[..l.len() - 1].to_owned()))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
            }
            Expression::Len(list) => {
                let list = FnExpression::try_from(*list)?;
                Box::new(move |vars| {
                    if let Val::List(_t, l) = list.eval(vars)? {
                        Some(Val::Integer(l.len() as Integer))
                    } else {
                        None
                    }
                })
            }
        }))
    }
}
