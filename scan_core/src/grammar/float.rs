use std::ops::{Add, Div, Mul, Neg};

use rand::{Rng, RngExt};

use crate::{
    Expression, Type, TypeError, Val,
    grammar::{BooleanExpr, IntegerExpr, NaturalExpr},
};

/// Floating-point values.
pub type Float = f64;

/// Floating-point numerical expression.
#[derive(Debug, Clone)]
pub enum FloatExpr<V>
where
    V: Clone,
{
    // -------------------
    // General expressions
    // -------------------
    /// A constant value.
    Const(Float),
    /// A typed variable.
    Var(V),
    /// Conversion from Natural
    Nat(NaturalExpr<V>),
    /// Conversion from Integer
    Int(IntegerExpr<V>),
    // -------------
    // Random values
    // -------------
    /// A random integer between a lower bound (included) and an upper bound (excluded).
    Rand(Box<(FloatExpr<V>, FloatExpr<V>)>),
    // --------------------
    // Arithmetic operators
    // --------------------
    /// Opposite of a numerical expression.
    Opposite(Box<FloatExpr<V>>),
    /// Arithmetic n-ary sum.
    Sum(Vec<FloatExpr<V>>),
    /// Arithmetic n-ary multiplication.
    Product(Vec<FloatExpr<V>>),
    /// Div operation
    Div(Box<(FloatExpr<V>, FloatExpr<V>)>),
    // -----
    // Flow
    // -----
    /// If-Then-Else construct, where If must be a boolean expression,
    /// Then and Else must have the same type,
    /// and this is also the type of the whole expression.
    Ite(Box<(BooleanExpr<V>, FloatExpr<V>, FloatExpr<V>)>),
}

impl<V> FloatExpr<V>
where
    V: Clone,
{
    /// Returns `true` if the expression is constant, i.e., it contains no variables, and `false` otherwise.
    pub fn is_constant(&self) -> bool {
        match self {
            FloatExpr::Const(_) => true,
            FloatExpr::Var(_) | FloatExpr::Rand(_) => false,
            FloatExpr::Nat(natural_expr) => natural_expr.is_constant(),
            FloatExpr::Int(integer_expr) => integer_expr.is_constant(),

            FloatExpr::Opposite(float_expr) => float_expr.is_constant(),
            FloatExpr::Sum(float_exprs) | FloatExpr::Product(float_exprs) => {
                float_exprs.iter().all(FloatExpr::is_constant)
            }
            FloatExpr::Div(args) => {
                let (lhs, rhs) = args.as_ref();
                lhs.is_constant() && rhs.is_constant()
            }
            FloatExpr::Ite(args) => {
                let (ite, lhs, rhs) = args.as_ref();
                ite.is_constant() && lhs.is_constant() && rhs.is_constant()
            }
        }
    }

    /// Returns the [`Float`] value computed from the expression,
    /// given the variable evaluation.
    /// It panics if the evaluation is not possible, including:
    ///
    /// - If a variable is not included in the evaluation;
    /// - If a variable included in the evaluation is not of [`Float`] type;
    /// - Division by 0;
    /// - Overflow.
    pub fn eval<R: Rng>(&self, vars: &dyn Fn(&V) -> Val, rng: &mut R) -> Float {
        match self {
            FloatExpr::Const(float) => *float,
            FloatExpr::Var(var) => {
                if let Val::Float(float) = vars(var) {
                    float
                } else {
                    panic!("type mismatch: expected float variable")
                }
            }
            // NOTE WARN: the u64 as f64 is lossy!
            FloatExpr::Nat(natural_expr) => natural_expr.eval(vars, rng) as f64,
            // NOTE WARN: the i64 as f64 is lossy!
            FloatExpr::Int(integer_expr) => integer_expr.eval(vars, rng) as f64,
            FloatExpr::Rand(bounds) => {
                let (lower_bound_expr, upper_bound_expr) = bounds.as_ref();
                let lower_bound = lower_bound_expr.eval(vars, rng);
                let upper_bound = upper_bound_expr.eval(vars, rng);
                rng.random_range(lower_bound..upper_bound)
            }
            FloatExpr::Opposite(float_expr) => -float_expr.eval(vars, rng),
            FloatExpr::Sum(float_exprs) => {
                float_exprs.iter().map(|expr| expr.eval(vars, rng)).sum()
            }
            FloatExpr::Product(float_exprs) => float_exprs
                .iter()
                .map(|expr| expr.eval(vars, rng))
                .product(),
            FloatExpr::Div(args) => {
                let (lhs_expr, rhs_expr) = args.as_ref();
                let lhs = lhs_expr.eval(vars, rng);
                let rhs = rhs_expr.eval(vars, rng);
                lhs / rhs
            }
            FloatExpr::Ite(args) => {
                let (ite, lhs, rhs) = args.as_ref();
                if ite.eval(vars, rng) {
                    lhs.eval(vars, rng)
                } else {
                    rhs.eval(vars, rng)
                }
            }
        }
    }

    pub(crate) fn map<W: Clone>(self, map: &dyn Fn(V) -> W) -> FloatExpr<W> {
        match self {
            FloatExpr::Const(float) => FloatExpr::Const(float),
            FloatExpr::Var(var) => FloatExpr::Var(map(var)),
            FloatExpr::Nat(natural_expr) => FloatExpr::Nat(natural_expr.map(map)),
            FloatExpr::Int(integer_expr) => FloatExpr::Int(integer_expr.map(map)),
            FloatExpr::Rand(bounds) => {
                let (lower_bound, upper_bound) = *bounds;
                FloatExpr::Rand(Box::new((lower_bound.map(map), upper_bound.map(map))))
            }
            FloatExpr::Opposite(float_expr) => FloatExpr::Opposite(Box::new(float_expr.map(map))),
            FloatExpr::Sum(float_exprs) => {
                FloatExpr::Sum(float_exprs.into_iter().map(|expr| expr.map(map)).collect())
            }
            FloatExpr::Product(float_exprs) => {
                FloatExpr::Product(float_exprs.into_iter().map(|expr| expr.map(map)).collect())
            }
            FloatExpr::Div(args) => {
                let (lhs, rhs) = *args;
                FloatExpr::Div(Box::new((lhs.map(map), rhs.map(map))))
            }
            FloatExpr::Ite(args) => {
                let (r#if, then, r#else) = *args;
                FloatExpr::Ite(Box::new((r#if.map(map), then.map(map), r#else.map(map))))
            }
        }
    }

    pub(crate) fn context(&self, vars: &dyn Fn(V) -> Option<Type>) -> Result<(), TypeError> {
        match self {
            FloatExpr::Const(_) => Ok(()),
            FloatExpr::Var(v) => matches!(vars(v.clone()), Some(Type::Float))
                .then_some(())
                .ok_or(TypeError::TypeMismatch),
            FloatExpr::Nat(natural_expr) => natural_expr.context(vars),
            FloatExpr::Int(integer_expr) => integer_expr.context(vars),
            FloatExpr::Rand(exprs) | FloatExpr::Div(exprs) => {
                exprs.0.context(vars).and_then(|()| exprs.1.context(vars))
            }
            FloatExpr::Opposite(integer_expr) => integer_expr.context(vars),
            FloatExpr::Sum(integer_exprs) | FloatExpr::Product(integer_exprs) => {
                integer_exprs.iter().try_for_each(|expr| expr.context(vars))
            }
            FloatExpr::Ite(exprs) => exprs
                .0
                .context(vars)
                .and_then(|()| exprs.1.context(vars))
                .and_then(|()| exprs.2.context(vars)),
        }
    }
}

impl<V> From<Float> for FloatExpr<V>
where
    V: Clone,
{
    fn from(value: Float) -> Self {
        Self::Const(value)
    }
}

impl<V> TryFrom<Expression<V>> for FloatExpr<V>
where
    V: Clone,
{
    type Error = TypeError;

    fn try_from(value: Expression<V>) -> Result<Self, Self::Error> {
        match value {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => Ok(FloatExpr::Nat(natural_expr)),
            Expression::Integer(integer_expr) => Ok(FloatExpr::Int(integer_expr)),
            Expression::Float(float_expr) => Ok(float_expr),
        }
    }
}

impl<V> From<NaturalExpr<V>> for FloatExpr<V>
where
    V: Clone,
{
    fn from(value: NaturalExpr<V>) -> Self {
        Self::Nat(value)
    }
}

impl<V> From<IntegerExpr<V>> for FloatExpr<V>
where
    V: Clone,
{
    fn from(value: IntegerExpr<V>) -> Self {
        if let IntegerExpr::Nat(nat_expr) = value {
            Self::Nat(nat_expr)
        } else {
            Self::Int(value)
        }
    }
}

impl<V> Add for FloatExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        if let FloatExpr::Sum(ref mut exprs) = self {
            if let FloatExpr::Sum(rhs_exprs) = rhs {
                exprs.extend(rhs_exprs);
            } else {
                exprs.push(rhs);
            }
            self
        } else if let FloatExpr::Sum(ref mut rhs_exprs) = rhs {
            rhs_exprs.push(self);
            rhs
        } else {
            FloatExpr::Sum(vec![self, rhs])
        }
    }
}

impl<V> Mul for FloatExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn mul(mut self, mut rhs: Self) -> Self::Output {
        if let FloatExpr::Product(ref mut exprs) = self {
            if let FloatExpr::Product(rhs_exprs) = rhs {
                exprs.extend(rhs_exprs);
            } else {
                exprs.push(rhs);
            }
            self
        } else if let FloatExpr::Product(ref mut rhs_exprs) = rhs {
            rhs_exprs.push(self);
            rhs
        } else {
            FloatExpr::Product(vec![self, rhs])
        }
    }
}

impl<V> Neg for FloatExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn neg(self) -> Self::Output {
        if let FloatExpr::Opposite(expr) = self {
            *expr
        } else {
            FloatExpr::Opposite(Box::new(self))
        }
    }
}

impl<V> Div for FloatExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        FloatExpr::Div(Box::new((self, rhs)))
    }
}
