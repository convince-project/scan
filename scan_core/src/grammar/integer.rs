use std::ops::{Add, Div, Mul, Neg};

use rand::{Rng, RngExt};

use crate::{
    Expression, Type, TypeError, Val,
    grammar::{BooleanExpr, FloatExpr, NaturalExpr},
};

/// Integer values.
pub type Integer = i64;

/// Integer expressions.
#[derive(Debug, Clone)]
pub enum IntegerExpr<V>
where
    V: Clone,
{
    // -------------------
    // General expressions
    // -------------------
    /// A constant value.
    Const(Integer),
    /// A typed variable.
    Var(V),
    /// Conversion from Natural
    Nat(NaturalExpr<V>),
    // -------------
    // Random values
    // -------------
    /// A random integer between a lower bound (included) and an upper bound (excluded).
    Rand(Box<(IntegerExpr<V>, IntegerExpr<V>)>),
    // --------------------
    // Arithmetic operators
    // --------------------
    /// Opposite of a numerical expression.
    Opposite(Box<IntegerExpr<V>>),
    /// Arithmetic n-ary sum.
    Sum(Vec<IntegerExpr<V>>),
    /// Arithmetic n-ary multiplication.
    Product(Vec<IntegerExpr<V>>),
    /// Div operation
    Div(Box<(IntegerExpr<V>, IntegerExpr<V>)>),
    /// Rem operation
    Rem(Box<(IntegerExpr<V>, IntegerExpr<V>)>),
    /// Floor
    Floor(Box<FloatExpr<V>>),
    // -----
    // Flow
    // -----
    /// If-Then-Else construct, where If must be a boolean expression,
    /// Then and Else must have the same type,
    /// and this is also the type of the whole expression.
    Ite(Box<(BooleanExpr<V>, IntegerExpr<V>, IntegerExpr<V>)>),
}

impl<V> IntegerExpr<V>
where
    V: Clone,
{
    /// Returns `true` if the expression is constant, i.e., it contains no variables, and `false` otherwise.
    pub fn is_constant(&self) -> bool {
        match self {
            IntegerExpr::Const(_) => true,
            IntegerExpr::Var(_) | IntegerExpr::Rand(_) => false,
            IntegerExpr::Nat(natural_expr) => natural_expr.is_constant(),
            IntegerExpr::Opposite(integer_expr) => integer_expr.is_constant(),
            IntegerExpr::Sum(integer_exprs) | IntegerExpr::Product(integer_exprs) => {
                integer_exprs.iter().all(IntegerExpr::is_constant)
            }
            IntegerExpr::Div(args) | IntegerExpr::Rem(args) => {
                let (lhs, rhs) = args.as_ref();
                lhs.is_constant() && rhs.is_constant()
            }
            IntegerExpr::Floor(float_expr) => float_expr.is_constant(),
            IntegerExpr::Ite(args) => {
                let (ite, lhs, rhs) = args.as_ref();
                ite.is_constant() && lhs.is_constant() && rhs.is_constant()
            }
        }
    }

    /// Returns the [`Integer`] value computed from the expression,
    /// given the variable evaluation.
    /// It panics if the evaluation is not possible, including:
    ///
    /// - If a variable is not included in the evaluation;
    /// - If a variable included in the evaluation is not of [`Integer`] type;
    /// - Division by 0;
    /// - Overflow.
    pub fn eval<R: Rng>(&self, vars: &dyn Fn(&V) -> Val, rng: &mut R) -> Integer {
        match self {
            IntegerExpr::Const(int) => *int,
            IntegerExpr::Var(var) => {
                if let Val::Integer(int) = vars(var) {
                    int
                } else {
                    panic!("type mismatch: expected natural variable")
                }
            }
            IntegerExpr::Nat(natural_expr) => natural_expr.eval(vars, rng) as Integer,
            IntegerExpr::Rand(bounds) => {
                let (lower_bound_expr, upper_bound_expr) = bounds.as_ref();
                let lower_bound = lower_bound_expr.eval(vars, rng);
                let upper_bound = upper_bound_expr.eval(vars, rng);
                rng.random_range(lower_bound..upper_bound)
            }
            IntegerExpr::Opposite(integer_expr) => integer_expr.eval(vars, rng).strict_neg(),
            IntegerExpr::Sum(integer_exprs) => integer_exprs
                .iter()
                .fold(0, |acc, expr| acc.strict_add(expr.eval(vars, rng))),
            IntegerExpr::Product(integer_exprs) => integer_exprs
                .iter()
                .fold(1, |acc, expr| acc.strict_mul(expr.eval(vars, rng))),
            IntegerExpr::Div(args) => {
                let (lhs_expr, rhs_expr) = args.as_ref();
                let lhs = lhs_expr.eval(vars, rng);
                let rhs = rhs_expr.eval(vars, rng);
                lhs.strict_div(rhs)
            }
            IntegerExpr::Rem(args) => {
                let (lhs_expr, rhs_expr) = args.as_ref();
                let lhs = lhs_expr.eval(vars, rng);
                let rhs = rhs_expr.eval(vars, rng);
                lhs.strict_rem_euclid(rhs)
            }
            // NOTE WARN: is float-to-int floor operation sound?
            IntegerExpr::Floor(float_expr) => float_expr.eval(vars, rng).floor() as Integer,
            IntegerExpr::Ite(args) => {
                let (ite, lhs, rhs) = args.as_ref();
                if ite.eval(vars, rng) {
                    lhs.eval(vars, rng)
                } else {
                    rhs.eval(vars, rng)
                }
            }
        }
    }

    pub(crate) fn map<W: Clone>(self, map: &dyn Fn(V) -> W) -> IntegerExpr<W> {
        match self {
            IntegerExpr::Const(i) => IntegerExpr::Const(i),
            IntegerExpr::Var(var) => IntegerExpr::Var(map(var)),
            IntegerExpr::Nat(nat_expr) => IntegerExpr::Nat(nat_expr.map(map)),
            IntegerExpr::Rand(bounds) => {
                let (lower_bound, upper_bound) = *bounds;
                IntegerExpr::Rand(Box::new((lower_bound.map(map), upper_bound.map(map))))
            }
            IntegerExpr::Opposite(integer_expr) => {
                IntegerExpr::Opposite(Box::new(integer_expr.map(map)))
            }
            IntegerExpr::Sum(integer_exprs) => IntegerExpr::Sum(
                integer_exprs
                    .into_iter()
                    .map(|expr| expr.map(map))
                    .collect(),
            ),
            IntegerExpr::Product(integer_exprs) => IntegerExpr::Product(
                integer_exprs
                    .into_iter()
                    .map(|expr| expr.map(map))
                    .collect(),
            ),
            IntegerExpr::Div(args) => {
                let (lhs, rhs) = *args;
                IntegerExpr::Div(Box::new((lhs.map(map), rhs.map(map))))
            }
            IntegerExpr::Rem(args) => {
                let (lhs, rhs) = *args;
                IntegerExpr::Rem(Box::new((lhs.map(map), rhs.map(map))))
            }
            IntegerExpr::Floor(float_expr) => IntegerExpr::Floor(Box::new(float_expr.map(map))),
            IntegerExpr::Ite(args) => {
                let (r#if, then, r#else) = *args;
                IntegerExpr::Ite(Box::new((r#if.map(map), then.map(map), r#else.map(map))))
            }
        }
    }

    pub(crate) fn context(&self, vars: &dyn Fn(V) -> Option<Type>) -> Result<(), TypeError> {
        match self {
            IntegerExpr::Const(_) => Ok(()),
            IntegerExpr::Var(v) => matches!(vars(v.clone()), Some(Type::Integer))
                .then_some(())
                .ok_or(TypeError::TypeMismatch),
            IntegerExpr::Nat(natural_expr) => natural_expr.context(vars),
            IntegerExpr::Rand(exprs) | IntegerExpr::Div(exprs) | IntegerExpr::Rem(exprs) => {
                exprs.0.context(vars).and_then(|()| exprs.1.context(vars))
            }
            IntegerExpr::Opposite(integer_expr) => integer_expr.context(vars),
            IntegerExpr::Sum(integer_exprs) | IntegerExpr::Product(integer_exprs) => {
                integer_exprs.iter().try_for_each(|expr| expr.context(vars))
            }
            IntegerExpr::Floor(float_expr) => float_expr.context(vars),
            IntegerExpr::Ite(exprs) => exprs
                .0
                .context(vars)
                .and_then(|()| exprs.1.context(vars))
                .and_then(|()| exprs.2.context(vars)),
        }
    }
}

impl<V> From<Integer> for IntegerExpr<V>
where
    V: Clone,
{
    fn from(value: Integer) -> Self {
        Self::Const(value)
    }
}

impl<V> TryFrom<Expression<V>> for IntegerExpr<V>
where
    V: Clone,
{
    type Error = TypeError;

    fn try_from(value: Expression<V>) -> Result<Self, Self::Error> {
        match value {
            Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => Ok(IntegerExpr::Nat(natural_expr)),
            Expression::Integer(integer_expr) => Ok(integer_expr),
        }
    }
}

impl<V> From<NaturalExpr<V>> for IntegerExpr<V>
where
    V: Clone,
{
    fn from(value: NaturalExpr<V>) -> Self {
        Self::Nat(value)
    }
}

impl<V> Add for IntegerExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        if let IntegerExpr::Sum(ref mut exprs) = self {
            if let IntegerExpr::Sum(rhs_exprs) = rhs {
                exprs.extend(rhs_exprs);
            } else {
                exprs.push(rhs);
            }
            self
        } else if let IntegerExpr::Sum(ref mut rhs_exprs) = rhs {
            rhs_exprs.push(self);
            rhs
        } else {
            IntegerExpr::Sum(vec![self, rhs])
        }
    }
}

impl<V> Mul for IntegerExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn mul(mut self, mut rhs: Self) -> Self::Output {
        if let IntegerExpr::Product(ref mut exprs) = self {
            if let IntegerExpr::Product(rhs_exprs) = rhs {
                exprs.extend(rhs_exprs);
            } else {
                exprs.push(rhs);
            }
            self
        } else if let IntegerExpr::Product(ref mut rhs_exprs) = rhs {
            rhs_exprs.push(self);
            rhs
        } else {
            IntegerExpr::Product(vec![self, rhs])
        }
    }
}

impl<V> Div for IntegerExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        IntegerExpr::Div(Box::new((self, rhs)))
    }
}

impl<V> Neg for IntegerExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn neg(self) -> Self::Output {
        if let IntegerExpr::Opposite(expr) = self {
            *expr
        } else {
            IntegerExpr::Opposite(Box::new(self))
        }
    }
}
