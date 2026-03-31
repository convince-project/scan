use std::ops::{Add, Div, Mul, Rem};

use rand::{Rng, RngExt};

use crate::{
    Expression, Type, TypeError, Val,
    grammar::{BooleanExpr, IntegerExpr},
};

/// Natural (unsigned) values.
pub type Natural = u64;

/// A [`Natural`] number expression
#[derive(Debug, Clone)]
pub enum NaturalExpr<V>
where
    V: Clone,
{
    // -------------------
    // General expressions
    // -------------------
    /// A constant value.
    Const(Natural),
    /// A typed variable.
    Var(V),
    // -------------
    // Random values
    // -------------
    /// A random integer between a lower bound (included) and an upper bound (excluded).
    Rand(Box<(NaturalExpr<V>, NaturalExpr<V>)>),
    // --------------------
    // Arithmetic operators
    // --------------------
    /// Arithmetic n-ary sum.
    Sum(Vec<NaturalExpr<V>>),
    /// Arithmetic n-ary multiplication.
    Product(Vec<NaturalExpr<V>>),
    /// Mod operation
    Rem(Box<(NaturalExpr<V>, NaturalExpr<V>)>),
    /// Div operation
    Div(Box<(NaturalExpr<V>, NaturalExpr<V>)>),
    /// Abs
    Abs(Box<IntegerExpr<V>>),
    // -----
    // Flow
    // -----
    /// If-Then-Else construct, where If must be a boolean expression,
    /// Then and Else must have the same type,
    /// and this is also the type of the whole expression.
    Ite(Box<(BooleanExpr<V>, NaturalExpr<V>, NaturalExpr<V>)>),
}

impl<V> NaturalExpr<V>
where
    V: Clone,
{
    /// Returns `true` if the expression is constant, i.e., it contains no variables, and `false` otherwise.
    pub fn is_constant(&self) -> bool {
        match self {
            NaturalExpr::Const(_) => true,
            NaturalExpr::Var(_) | NaturalExpr::Rand(_) => false,
            NaturalExpr::Sum(natural_exprs) | NaturalExpr::Product(natural_exprs) => {
                natural_exprs.iter().all(NaturalExpr::is_constant)
            }
            NaturalExpr::Rem(args) | NaturalExpr::Div(args) => {
                let (lhs, rhs) = args.as_ref();
                lhs.is_constant() && rhs.is_constant()
            }
            NaturalExpr::Abs(integer_expr) => integer_expr.is_constant(),
            NaturalExpr::Ite(args) => {
                let (ite, lhs, rhs) = args.as_ref();
                ite.is_constant() && lhs.is_constant() && rhs.is_constant()
            }
        }
    }

    /// Returns the [`Natural`] value computed from the expression,
    /// given the variable evaluation.
    /// It panics if the evaluation is not possible, including:
    ///
    /// - If a variable is not included in the evaluation;
    /// - If a variable included in the evaluation is not of [`Natural`] type;
    /// - Division by 0;
    /// - Overflow.
    pub fn eval<R: Rng>(&self, vars: &dyn Fn(&V) -> Val, rng: &mut R) -> Natural {
        match self {
            NaturalExpr::Const(nat) => *nat,
            NaturalExpr::Var(var) => {
                if let Val::Natural(nat) = vars(var) {
                    nat
                } else {
                    panic!("type mismatch: expected natural variable")
                }
            }
            NaturalExpr::Rand(bounds) => {
                let (lower_bound_expr, upper_bound_expr) = bounds.as_ref();
                let lower_bound = lower_bound_expr.eval(vars, rng);
                let upper_bound = upper_bound_expr.eval(vars, rng);
                rng.random_range(lower_bound..upper_bound)
            }
            NaturalExpr::Sum(natural_exprs) => natural_exprs
                .iter()
                .fold(0, |acc, expr| acc.strict_add(expr.eval(vars, rng))),
            NaturalExpr::Product(natural_exprs) => natural_exprs
                .iter()
                .fold(1, |acc, expr| acc.strict_mul(expr.eval(vars, rng))),
            NaturalExpr::Rem(args) => {
                let (lhs_expr, rhs_expr) = args.as_ref();
                let lhs = lhs_expr.eval(vars, rng);
                let rhs = rhs_expr.eval(vars, rng);
                lhs.strict_rem_euclid(rhs)
            }
            NaturalExpr::Div(_) => todo!(),
            NaturalExpr::Abs(integer_expr) => integer_expr.eval(vars, rng).unsigned_abs(),
            NaturalExpr::Ite(args) => {
                let (ite, lhs, rhs) = args.as_ref();
                if ite.eval(vars, rng) {
                    lhs.eval(vars, rng)
                } else {
                    rhs.eval(vars, rng)
                }
            }
        }
    }

    pub(crate) fn map<W: Clone>(self, map: &dyn Fn(V) -> W) -> NaturalExpr<W> {
        match self {
            NaturalExpr::Const(n) => NaturalExpr::Const(n),
            NaturalExpr::Var(var) => NaturalExpr::Var(map(var)),
            NaturalExpr::Rand(bounds) => {
                let (lower_bound, upper_bound) = *bounds;
                NaturalExpr::Rand(Box::new((lower_bound.map(map), upper_bound.map(map))))
            }
            NaturalExpr::Sum(natural_exprs) => NaturalExpr::Sum(
                natural_exprs
                    .into_iter()
                    .map(|expr| expr.map(map))
                    .collect(),
            ),
            NaturalExpr::Product(natural_exprs) => NaturalExpr::Product(
                natural_exprs
                    .into_iter()
                    .map(|expr| expr.map(map))
                    .collect(),
            ),
            NaturalExpr::Rem(args) => {
                let (lhs, rhs) = *args;
                NaturalExpr::Rem(Box::new((lhs.map(map), rhs.map(map))))
            }
            NaturalExpr::Div(args) => {
                let (lhs, rhs) = *args;
                NaturalExpr::Div(Box::new((lhs.map(map), rhs.map(map))))
            }
            NaturalExpr::Abs(integer_expr) => NaturalExpr::Abs(Box::new(integer_expr.map(map))),
            NaturalExpr::Ite(args) => {
                let (r#if, then, r#else) = *args;
                NaturalExpr::Ite(Box::new((r#if.map(map), then.map(map), r#else.map(map))))
            }
        }
    }

    pub(crate) fn context(&self, vars: &dyn Fn(V) -> Option<Type>) -> Result<(), TypeError> {
        match self {
            NaturalExpr::Const(_) => Ok(()),
            NaturalExpr::Var(v) => matches!(vars(v.clone()), Some(Type::Natural))
                .then_some(())
                .ok_or(TypeError::TypeMismatch),
            NaturalExpr::Rand(exprs) | NaturalExpr::Div(exprs) | NaturalExpr::Rem(exprs) => {
                exprs.0.context(vars).and_then(|()| exprs.1.context(vars))
            }
            NaturalExpr::Sum(integer_exprs) | NaturalExpr::Product(integer_exprs) => {
                integer_exprs.iter().try_for_each(|expr| expr.context(vars))
            }
            NaturalExpr::Ite(exprs) => exprs
                .0
                .context(vars)
                .and_then(|()| exprs.1.context(vars))
                .and_then(|()| exprs.2.context(vars)),
            NaturalExpr::Abs(integer_expr) => integer_expr.context(vars),
        }
    }
}

impl<V: Clone> From<Natural> for NaturalExpr<V> {
    fn from(value: Natural) -> Self {
        NaturalExpr::Const(value)
    }
}

impl<V> TryFrom<Expression<V>> for NaturalExpr<V>
where
    V: Clone,
{
    type Error = TypeError;

    fn try_from(value: Expression<V>) -> Result<Self, Self::Error> {
        if let Expression::Natural(nat_expr) = value {
            Ok(nat_expr)
        } else {
            Err(TypeError::TypeMismatch)
        }
    }
}

impl<V> Add for NaturalExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        if let NaturalExpr::Sum(ref mut exprs) = self {
            if let NaturalExpr::Sum(rhs_exprs) = rhs {
                exprs.extend(rhs_exprs);
            } else {
                exprs.push(rhs);
            }
            self
        } else if let NaturalExpr::Sum(ref mut rhs_exprs) = rhs {
            rhs_exprs.push(self);
            rhs
        } else {
            NaturalExpr::Sum(vec![self, rhs])
        }
    }
}

impl<V> Mul for NaturalExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn mul(mut self, mut rhs: Self) -> Self::Output {
        if let NaturalExpr::Product(ref mut exprs) = self {
            if let NaturalExpr::Product(rhs_exprs) = rhs {
                exprs.extend(rhs_exprs);
            } else {
                exprs.push(rhs);
            }
            self
        } else if let NaturalExpr::Product(ref mut rhs_exprs) = rhs {
            rhs_exprs.push(self);
            rhs
        } else {
            NaturalExpr::Product(vec![self, rhs])
        }
    }
}

impl<V> Div for NaturalExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        NaturalExpr::Div(Box::new((self, rhs)))
    }
}

impl<V> Rem for NaturalExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        NaturalExpr::Rem(Box::new((self, rhs)))
    }
}
