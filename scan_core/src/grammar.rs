//! The language used by PGs and CSs.
//!
//! The type [`Expression<V>`] encodes the used language,
//! where `V` is the type parameter of variables.
//! The language features base types and product types,
//! Boolean logic and basic arithmetic expressions.

mod boolean;
mod float;
mod integer;
mod natural;

use rand::Rng;
use std::{
    hash::Hash,
    ops::{Add, BitAnd, BitOr, Div, Mul, Neg, Not, Rem},
};
use thiserror::Error;

use crate::dummy_rng::DummyRng;

pub use boolean::*;
pub use float::*;
pub use integer::*;
pub use natural::*;

/// The error type for operations with [`Type`].
#[derive(Debug, Clone, Copy, Error)]
pub enum TypeError {
    /// Types that should be matching are not,
    /// or are not compatible with each other.
    #[error("type mismatch")]
    TypeMismatch,
    /// The variable's type is unknown.
    #[error("the type of variable is unknown")]
    UnknownVar,
    /// Bounds violate some constraint.
    #[error("the bounds violate some constraint")]
    BadBounds,
    /// Probability violates some constraint.
    #[error("the probability violates some constraint")]
    BadProbability,
}

/// The types supported by the language internally used by PGs and CSs.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Type {
    /// Boolean type.
    Boolean,
    /// Natural (unsigned) numerical type.
    Natural,
    /// Integer numerical type.
    Integer,
    /// Floating-point numerical type.
    Float,
}

impl Type {
    /// The default value for a given type.
    /// Used to initialize variables.
    pub fn default_value(self) -> Val {
        match self {
            Type::Boolean => Val::Boolean(false),
            Type::Natural => Val::Natural(0),
            Type::Integer => Val::Integer(0),
            Type::Float => Val::Float(0.0),
        }
    }
}

/// Possible values for each [`Type`].
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Val {
    /// Boolean values.
    Boolean(bool),
    /// Natural (unsigned) values.
    Natural(Natural),
    /// Integer values.
    Integer(Integer),
    /// Floating-point values.
    Float(Float),
}

impl Val {
    /// Returns the [`Type`] of the value.
    pub fn r#type(self) -> Type {
        match self {
            Val::Boolean(_) => Type::Boolean,
            Val::Natural(_) => Type::Natural,
            Val::Integer(_) => Type::Integer,
            Val::Float(_) => Type::Float,
        }
    }
}

impl From<bool> for Val {
    fn from(value: bool) -> Self {
        Val::Boolean(value)
    }
}

impl From<Natural> for Val {
    fn from(value: Natural) -> Self {
        Val::Natural(value)
    }
}

impl From<Integer> for Val {
    fn from(value: Integer) -> Self {
        Val::Integer(value)
    }
}

impl From<Float> for Val {
    fn from(value: Float) -> Self {
        Val::Float(value)
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
    /// Expression of Boolean type.
    Boolean(BooleanExpr<V>),
    /// Expression of Natural type (unsigned integers).
    Natural(NaturalExpr<V>),
    /// Expression of Integer type.
    Integer(IntegerExpr<V>),
    /// Expression of Float type.
    Float(FloatExpr<V>),
}

impl<V> From<Val> for Expression<V>
where
    V: Clone,
{
    fn from(value: Val) -> Self {
        match value {
            Val::Boolean(b) => Expression::Boolean(BooleanExpr::Const(b)),
            Val::Natural(nat) => Expression::Natural(NaturalExpr::Const(nat)),
            Val::Integer(int) => Expression::Integer(IntegerExpr::Const(int)),
            Val::Float(float) => Expression::Float(FloatExpr::Const(float)),
        }
    }
}

impl<V> From<bool> for Expression<V>
where
    V: Clone,
{
    fn from(value: bool) -> Self {
        Expression::Boolean(BooleanExpr::from(value))
    }
}

impl<V> From<Natural> for Expression<V>
where
    V: Clone,
{
    fn from(value: Natural) -> Self {
        Expression::Natural(NaturalExpr::from(value))
    }
}

impl<V> From<Integer> for Expression<V>
where
    V: Clone,
{
    fn from(value: Integer) -> Self {
        Expression::Integer(IntegerExpr::from(value))
    }
}

impl<V> From<Float> for Expression<V>
where
    V: Clone,
{
    fn from(value: Float) -> Self {
        Expression::Float(FloatExpr::from(value))
    }
}

impl<V> Expression<V>
where
    V: Clone,
{
    /// Computes the type of an expression.
    ///
    /// Fails if the expression is badly typed,
    /// e.g., if variables in it have type incompatible with the expression.
    pub fn r#type(&self) -> Type {
        match self {
            Expression::Boolean(_) => Type::Boolean,
            Expression::Natural(_) => Type::Natural,
            Expression::Integer(_) => Type::Integer,
            Expression::Float(_) => Type::Float,
        }
    }

    /// Evaluates the expression with the given variable assignments and provided RNG.
    ///
    /// Will assume the expression (with the variable assignment) is well-typed,
    /// and may panic if producing an unexpected type.
    pub fn eval<R: Rng>(&self, vars: &dyn Fn(&V) -> Val, rng: &mut R) -> Val {
        match self {
            Expression::Boolean(boolean_expr) => Val::Boolean(boolean_expr.eval(vars, rng)),
            Expression::Natural(natural_expr) => Val::Natural(natural_expr.eval(vars, rng)),
            Expression::Integer(integer_expr) => Val::Integer(integer_expr.eval(vars, rng)),
            Expression::Float(float_expr) => Val::Float(float_expr.eval(vars, rng)),
        }
    }

    /// Evals a constant expression.
    /// Returns an error if expression contains variables.
    pub fn is_constant(&self) -> bool {
        match self {
            Expression::Boolean(boolean_expr) => boolean_expr.is_constant(),
            Expression::Natural(natural_expr) => natural_expr.is_constant(),
            Expression::Integer(integer_expr) => integer_expr.is_constant(),
            Expression::Float(float_expr) => float_expr.is_constant(),
        }
    }

    /// Evals a constant expression.
    /// Returns an error if expression contains variables.
    pub fn eval_constant(&self) -> Result<Val, TypeError> {
        if self.is_constant() {
            Ok(self.eval(&|_| panic!("no vars"), &mut DummyRng))
        } else {
            Err(TypeError::UnknownVar)
        }
    }

    pub(crate) fn map<W: Clone>(self, map: &dyn Fn(V) -> W) -> Expression<W> {
        match self {
            Expression::Boolean(boolean_expr) => Expression::Boolean(boolean_expr.map(map)),
            Expression::Natural(natural_expr) => Expression::Natural(natural_expr.map(map)),
            Expression::Integer(integer_expr) => Expression::Integer(integer_expr.map(map)),
            Expression::Float(float_expr) => Expression::Float(float_expr.map(map)),
        }
    }

    pub(crate) fn context(&self, vars: &dyn Fn(V) -> Option<Type>) -> Result<(), TypeError> {
        match self {
            Expression::Boolean(boolean_expr) => boolean_expr.context(vars),
            Expression::Natural(natural_expr) => natural_expr.context(vars),
            Expression::Integer(integer_expr) => integer_expr.context(vars),
            Expression::Float(float_expr) => float_expr.context(vars),
        }
    }

    /// Creates an `[Expression]` out of a variable and the type of such variable.
    pub fn from_var(var: V, r#type: Type) -> Self {
        match r#type {
            Type::Boolean => Expression::Boolean(BooleanExpr::Var(var)),
            Type::Natural => Expression::Natural(NaturalExpr::Var(var)),
            Type::Integer => Expression::Integer(IntegerExpr::Var(var)),
            Type::Float => Expression::Float(FloatExpr::Var(var)),
        }
    }

    /// Creates an [`Expression`] that predicates the equality of `self` and `rhs`,
    /// and returns error if comparison is not possible.
    ///
    /// Equality of Boolean is represented as "if and only if".
    ///
    /// Equality of numerical types automatically casts the one of most-restrictive type to the less restrictive type of the other one:
    /// for example, [`NaturalExpr`] can be cast to [`IntegerExpr`] or [`FloatExpr`];
    /// and `[IntegerExpr]` can be cast to `[FloatExpr]`.
    pub fn equal_to(self, rhs: Self) -> Result<BooleanExpr<V>, TypeError> {
        match self {
            Expression::Boolean(boolean_expr) => match rhs {
                Expression::Boolean(boolean_expr_rhs) => {
                    Ok(BooleanExpr::Implies(Box::new((
                        boolean_expr.clone(),
                        boolean_expr_rhs.clone(),
                    ))) & BooleanExpr::Implies(Box::new((boolean_expr_rhs, boolean_expr))))
                }
                Expression::Natural(_) | Expression::Integer(_) | Expression::Float(_) => {
                    Err(TypeError::TypeMismatch)
                }
            },
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => {
                    Ok(BooleanExpr::NatEqual(natural_expr, natural_expr_rhs))
                }
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::IntEqual(
                    IntegerExpr::from(natural_expr),
                    integer_expr_rhs,
                )),
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatEqual(
                    FloatExpr::Nat(natural_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::IntEqual(
                    integer_expr,
                    IntegerExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => {
                    Ok(BooleanExpr::IntEqual(integer_expr, integer_expr_rhs))
                }
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatEqual(
                    FloatExpr::Int(integer_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::FloatEqual(
                    float_expr,
                    FloatExpr::Nat(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::FloatEqual(
                    float_expr,
                    FloatExpr::Int(integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => {
                    Ok(BooleanExpr::FloatEqual(float_expr, float_expr_rhs))
                }
            },
        }
    }

    /// Creates a [`BooleanExpr`] that compares numerical expressions `self` and `rhs`,
    /// and returns error if comparison is not possible.
    ///
    /// Equality of numerical types automatically casts the one of most-restrictive type to the less restrictive type of the other one;
    /// see [`Self::equal_to`].
    pub fn greater_than_or_equal_to(self, rhs: Self) -> Result<BooleanExpr<V>, TypeError> {
        match self {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => {
                    Ok(BooleanExpr::NatGreaterEq(natural_expr, natural_expr_rhs))
                }
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::IntGreaterEq(
                    IntegerExpr::from(natural_expr),
                    integer_expr_rhs,
                )),
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatGreaterEq(
                    FloatExpr::Nat(natural_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::IntGreaterEq(
                    integer_expr,
                    IntegerExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => {
                    Ok(BooleanExpr::IntGreaterEq(integer_expr, integer_expr_rhs))
                }
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatGreaterEq(
                    FloatExpr::Int(integer_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::FloatGreaterEq(
                    float_expr,
                    FloatExpr::Nat(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::FloatGreaterEq(
                    float_expr,
                    FloatExpr::Int(integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => {
                    Ok(BooleanExpr::FloatGreaterEq(float_expr, float_expr_rhs))
                }
            },
        }
    }

    /// Creates a [`BooleanExpr`] that compares numerical expressions `self` and `rhs`,
    /// and returns error if comparison is not possible.
    ///
    /// Equality of numerical types automatically casts the one of most-restrictive type to the less restrictive type of the other one;
    /// see [`Self::equal_to`].
    pub fn greater_than(self, rhs: Self) -> Result<BooleanExpr<V>, TypeError> {
        match self {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => {
                    Ok(BooleanExpr::NatGreater(natural_expr, natural_expr_rhs))
                }
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::IntGreater(
                    IntegerExpr::from(natural_expr),
                    integer_expr_rhs,
                )),
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatGreater(
                    FloatExpr::from(natural_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::IntGreater(
                    integer_expr,
                    IntegerExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => {
                    Ok(BooleanExpr::IntGreater(integer_expr, integer_expr_rhs))
                }
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatGreater(
                    FloatExpr::from(integer_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::FloatGreater(
                    float_expr,
                    FloatExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::FloatGreater(
                    float_expr,
                    FloatExpr::from(integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => {
                    Ok(BooleanExpr::FloatGreater(float_expr, float_expr_rhs))
                }
            },
        }
    }

    /// Creates a [`BooleanExpr`] that compares numerical expressions `self` and `rhs`,
    /// and returns error if comparison is not possible.
    ///
    /// Equality of numerical types automatically casts the one of most-restrictive type to the less restrictive type of the other one;
    /// see [`Self::equal_to`].
    pub fn less_than(self, rhs: Self) -> Result<BooleanExpr<V>, TypeError> {
        match self {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => {
                    Ok(BooleanExpr::NatLess(natural_expr, natural_expr_rhs))
                }
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::IntLess(
                    IntegerExpr::from(natural_expr),
                    integer_expr_rhs,
                )),
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatLess(
                    FloatExpr::from(natural_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::IntLess(
                    integer_expr,
                    IntegerExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => {
                    Ok(BooleanExpr::IntLess(integer_expr, integer_expr_rhs))
                }
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatLess(
                    FloatExpr::from(integer_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::FloatLess(
                    float_expr,
                    FloatExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::FloatLess(
                    float_expr,
                    FloatExpr::from(integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => {
                    Ok(BooleanExpr::FloatLess(float_expr, float_expr_rhs))
                }
            },
        }
    }

    /// Creates a [`BooleanExpr`] that compares numerical expressions `self` and `rhs`,
    /// and returns error if comparison is not possible.
    ///
    /// Equality of numerical types automatically casts the one of most-restrictive type to the less restrictive type of the other one;
    /// see [`Self::equal_to`].
    pub fn less_than_or_equal_to(self, rhs: Self) -> Result<BooleanExpr<V>, TypeError> {
        match self {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => {
                    Ok(BooleanExpr::NatLessEq(natural_expr, natural_expr_rhs))
                }
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::IntLessEq(
                    IntegerExpr::from(natural_expr),
                    integer_expr_rhs,
                )),
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatLessEq(
                    FloatExpr::Nat(natural_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::IntLessEq(
                    integer_expr,
                    IntegerExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => {
                    Ok(BooleanExpr::IntLessEq(integer_expr, integer_expr_rhs))
                }
                Expression::Float(float_expr_rhs) => Ok(BooleanExpr::FloatLessEq(
                    FloatExpr::Int(integer_expr),
                    float_expr_rhs,
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(BooleanExpr::FloatLessEq(
                    float_expr,
                    FloatExpr::Nat(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(BooleanExpr::FloatLessEq(
                    float_expr,
                    FloatExpr::Int(integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => {
                    Ok(BooleanExpr::FloatLessEq(float_expr, float_expr_rhs))
                }
            },
        }
    }

    /// Creates a [`BooleanExpr`] that compares numerical expressions `self` and `rhs`,
    /// and returns error if comparison is not possible.
    ///
    /// Equality of numerical types automatically casts the one of most-restrictive type to the less restrictive type of the other one;
    /// see [`Self::equal_to`].
    pub fn ite(self, then: Self, r#else: Self) -> Result<Self, TypeError> {
        if let Expression::Boolean(r#if) = self {
            match then {
                Expression::Boolean(if_boolean_expr) => {
                    if let Expression::Boolean(else_boolean_expr) = r#else {
                        Ok(Expression::Boolean(BooleanExpr::Ite(Box::new((
                            r#if,
                            if_boolean_expr,
                            else_boolean_expr,
                        )))))
                    } else {
                        Err(TypeError::TypeMismatch)
                    }
                }
                Expression::Natural(if_natural_expr) => match r#else {
                    Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                    Expression::Natural(else_natural_expr) => Ok(Expression::Natural(
                        NaturalExpr::Ite(Box::new((r#if, if_natural_expr, else_natural_expr))),
                    )),
                    Expression::Integer(else_integer_expr) => {
                        Ok(Expression::Integer(IntegerExpr::Ite(Box::new((
                            r#if,
                            IntegerExpr::from(if_natural_expr),
                            else_integer_expr,
                        )))))
                    }
                    Expression::Float(else_float_expr) => Ok(Expression::Float(FloatExpr::Ite(
                        Box::new((r#if, FloatExpr::from(if_natural_expr), else_float_expr)),
                    ))),
                },
                Expression::Integer(if_integer_expr) => match r#else {
                    Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                    Expression::Natural(else_natural_expr) => {
                        Ok(Expression::Integer(IntegerExpr::Ite(Box::new((
                            r#if,
                            if_integer_expr,
                            IntegerExpr::from(else_natural_expr),
                        )))))
                    }
                    Expression::Integer(else_integer_expr) => Ok(Expression::Integer(
                        IntegerExpr::Ite(Box::new((r#if, if_integer_expr, else_integer_expr))),
                    )),
                    Expression::Float(else_float_expr) => Ok(Expression::Float(FloatExpr::Ite(
                        Box::new((r#if, FloatExpr::from(if_integer_expr), else_float_expr)),
                    ))),
                },
                Expression::Float(if_float_expr) => match r#else {
                    Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                    Expression::Natural(else_natural_expr) => {
                        Ok(Expression::Float(FloatExpr::Ite(Box::new((
                            r#if,
                            if_float_expr,
                            FloatExpr::from(else_natural_expr),
                        )))))
                    }
                    Expression::Integer(else_integer_expr) => {
                        Ok(Expression::Float(FloatExpr::Ite(Box::new((
                            r#if,
                            if_float_expr,
                            FloatExpr::from(else_integer_expr),
                        )))))
                    }
                    Expression::Float(else_float_expr) => Ok(Expression::Float(FloatExpr::Ite(
                        Box::new((r#if, if_float_expr, else_float_expr)),
                    ))),
                },
            }
        } else {
            Err(TypeError::TypeMismatch)
        }
    }
}

impl<V: Clone> Neg for Expression<V> {
    type Output = Result<Expression<V>, TypeError>;

    fn neg(self) -> Self::Output {
        match self {
            Expression::Boolean(_) | Expression::Natural(_) => Err(TypeError::TypeMismatch),
            Expression::Integer(integer_expr) => Ok(Expression::Integer(-integer_expr)),
            Expression::Float(float_expr) => Ok(Expression::Float(-float_expr)),
        }
    }
}

impl<V: Clone> Not for Expression<V> {
    type Output = Result<Expression<V>, TypeError>;

    fn not(self) -> Self::Output {
        match self {
            Expression::Boolean(boolean_expr) => Ok(Expression::Boolean(!boolean_expr)),
            Expression::Natural(_) | Expression::Integer(_) | Expression::Float(_) => {
                Err(TypeError::TypeMismatch)
            }
        }
    }
}

impl<V: Clone> Add for Expression<V> {
    type Output = Result<Self, TypeError>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => {
                    Ok(Expression::Natural(natural_expr + natural_expr_rhs))
                }
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Integer(
                    IntegerExpr::from(natural_expr) + integer_expr_rhs,
                )),
                Expression::Float(float_expr_rhs) => Ok(Expression::Float(
                    FloatExpr::from(natural_expr) + float_expr_rhs,
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Integer(
                    integer_expr + IntegerExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => {
                    Ok(Expression::Integer(integer_expr + integer_expr_rhs))
                }
                Expression::Float(float_expr_rhs) => Ok(Expression::Float(
                    FloatExpr::from(integer_expr) + float_expr_rhs,
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Float(
                    float_expr + FloatExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Float(
                    float_expr + FloatExpr::from(integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => {
                    Ok(Expression::Float(float_expr + float_expr_rhs))
                }
            },
        }
    }
}

impl<V: Clone> Mul for Expression<V> {
    type Output = Result<Self, TypeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => {
                    Ok(Expression::Natural(natural_expr * natural_expr_rhs))
                }
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Integer(
                    IntegerExpr::from(natural_expr) * integer_expr_rhs,
                )),
                Expression::Float(float_expr_rhs) => Ok(Expression::Float(
                    FloatExpr::from(natural_expr) * float_expr_rhs,
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Integer(
                    integer_expr * IntegerExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => {
                    Ok(Expression::Integer(integer_expr * integer_expr_rhs))
                }
                Expression::Float(float_expr_rhs) => Ok(Expression::Float(
                    FloatExpr::from(integer_expr) * float_expr_rhs,
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Float(
                    float_expr * FloatExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Float(
                    float_expr * FloatExpr::from(integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => {
                    Ok(Expression::Float(float_expr * float_expr_rhs))
                }
            },
        }
    }
}

impl<V: Clone> Div for Expression<V> {
    type Output = Result<Self, TypeError>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => {
                    Ok(Expression::Natural(natural_expr / natural_expr_rhs))
                }
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Integer(
                    IntegerExpr::from(natural_expr) / integer_expr_rhs,
                )),
                Expression::Float(float_expr_rhs) => Ok(Expression::Float(
                    FloatExpr::from(natural_expr) / float_expr_rhs,
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Integer(
                    integer_expr / IntegerExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => {
                    Ok(Expression::Integer(integer_expr / integer_expr_rhs))
                }
                Expression::Float(float_expr_rhs) => Ok(Expression::Float(
                    FloatExpr::from(integer_expr) / float_expr_rhs,
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Float(
                    float_expr / FloatExpr::from(natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Float(
                    float_expr / FloatExpr::from(integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => {
                    Ok(Expression::Float(float_expr / float_expr_rhs))
                }
            },
        }
    }
}

impl<V: Clone> Rem for Expression<V> {
    type Output = Result<Self, TypeError>;

    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            Expression::Natural(natural_expr) => match rhs {
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Natural(NaturalExpr::Rem(
                    Box::new((natural_expr, natural_expr_rhs)),
                ))),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Integer(IntegerExpr::Rem(
                    Box::new((IntegerExpr::from(natural_expr), integer_expr_rhs)),
                ))),
                Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Integer(IntegerExpr::Rem(
                    Box::new((integer_expr, IntegerExpr::from(natural_expr_rhs))),
                ))),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Integer(IntegerExpr::Rem(
                    Box::new((integer_expr, integer_expr_rhs)),
                ))),
                Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
            },
            Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
        }
    }
}

impl<V: Clone> BitAnd for Expression<V> {
    type Output = Result<Self, TypeError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        if let Expression::Boolean(lhs) = self {
            if let Expression::Boolean(rhs) = rhs {
                Ok(Expression::Boolean(lhs & rhs))
            } else {
                Err(TypeError::TypeMismatch)
            }
        } else {
            Err(TypeError::TypeMismatch)
        }
    }
}

impl<V: Clone> BitOr for Expression<V> {
    type Output = Result<Self, TypeError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        if let Expression::Boolean(lhs) = self {
            if let Expression::Boolean(rhs) = rhs {
                Ok(Expression::Boolean(lhs | rhs))
            } else {
                Err(TypeError::TypeMismatch)
            }
        } else {
            Err(TypeError::TypeMismatch)
        }
    }
}
