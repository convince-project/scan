//! The language used by PGs and CSs.
//!
//! The type [`Expression<V>`] encodes the used language,
//! where `V` is the type parameter of variables.
//! The language features base types and product types,
//! Boolean logic and basic arithmetic expressions.

use std::{collections::HashMap, hash::Hash};

/// The types supported by the language internally used by PGs and CSs.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    /// Boolean type.
    Boolean,
    /// Integer numerical type.
    Integer,
    /// Product of a list of types (including other products).
    Product(Vec<Type>),
}

impl Type {
    /// The default value for a given type.
    /// Used to initialize variables.
    pub fn default_value(&self) -> Val {
        match self {
            Type::Boolean => Val::Boolean(false),
            Type::Integer => Val::Integer(0),
            Type::Product(tuple) => {
                Val::Tuple(Vec::from_iter(tuple.iter().map(Self::default_value)))
            }
        }
    }
}

/// Integer values.
pub type Integer = i32;

/// Possible values for each [`Type`].
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Val {
    /// Boolean values.
    Boolean(bool),
    /// Integer values.
    Integer(Integer),
    /// Values for product types, i.e., tuples of suitable values.
    Tuple(Vec<Val>),
}

impl Val {
    pub(crate) fn r#type(&self) -> Type {
        match self {
            Val::Boolean(_) => Type::Boolean,
            Val::Integer(_) => Type::Integer,
            Val::Tuple(comps) => Type::Product(comps.iter().map(Val::r#type).collect()),
        }
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

pub(crate) struct FnExpression<C>(Box<dyn Fn(&C) -> Option<Val> + Send + Sync>);

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

impl<K> TryFrom<Expression<K>> for FnExpression<HashMap<K, Val>>
where
    K: Copy + Clone + Eq + Hash + Send + Sync + 'static,
{
    type Error = ();

    fn try_from(value: Expression<K>) -> Result<Self, Self::Error> {
        Ok(FnExpression(match value {
            Expression::Const(val) => Box::new(move |_| Some(val.to_owned())),
            Expression::Var(var) => Box::new(move |vars| vars.get(&var).cloned()),
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
                Box::new(move |vars| {
                    if let Val::Integer(i) = expr.eval(vars)? {
                        Some(Val::Integer(-i))
                    } else {
                        None
                    }
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
                        .map(|expr| {
                            if let Val::Integer(i) = expr.eval(vars)? {
                                Some(i)
                            } else {
                                None
                            }
                        })
                        .sum::<Option<Integer>>()
                        .map(Val::Integer)
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
                        .map(|expr| {
                            if let Val::Integer(i) = expr.eval(vars)? {
                                Some(i)
                            } else {
                                None
                            }
                        })
                        .product::<Option<Integer>>()
                        .map(Val::Integer)
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
                Box::new(move |vars| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) =
                        (lhs.eval(vars)?, rhs.eval(vars)?)
                    {
                        Some(Val::Boolean(lhs > rhs))
                    } else {
                        None
                    }
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
                Box::new(move |vars| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) =
                        (lhs.eval(vars)?, rhs.eval(vars)?)
                    {
                        Some(Val::Boolean(lhs < rhs))
                    } else {
                        None
                    }
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
        }))
    }
}
