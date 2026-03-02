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
    /// The tuple has no component for such index.
    #[error("the tuple does not have the component")]
    MissingComponent,
    /// The variable's type is unknown.
    #[error("the type of variable is unknown")]
    UnknownVar,
    /// The index is out of bounds.
    #[error("the index is out of bounds")]
    IndexOutOfBounds,
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
    Boolean(BooleanExpr<V>),
    Natural(NaturalExpr<V>),
    Integer(IntegerExpr<V>),
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

    pub fn map<W: Clone>(self, map: &dyn Fn(V) -> W) -> Expression<W> {
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

    pub fn from_var(var: V, r#type: Type) -> Self {
        match r#type {
            Type::Boolean => Expression::Boolean(BooleanExpr::Var(var)),
            Type::Natural => Expression::Natural(NaturalExpr::Var(var)),
            Type::Integer => Expression::Integer(IntegerExpr::Var(var)),
            Type::Float => Expression::Float(FloatExpr::Var(var)),
        }
    }

    pub fn equal_to(self, rhs: Self) -> Result<Self, TypeError> {
        match self {
            Expression::Boolean(boolean_expr) => match rhs {
                Expression::Boolean(boolean_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::Implies(Box::new((
                        boolean_expr.clone(),
                        boolean_expr_rhs.clone(),
                    ))) & BooleanExpr::Implies(Box::new((boolean_expr_rhs, boolean_expr))),
                )),
                Expression::Natural(_) | Expression::Integer(_) | Expression::Float(_) => {
                    Err(TypeError::TypeMismatch)
                }
            },
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::NatEqual(natural_expr, natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntEqual(IntegerExpr::from(natural_expr), integer_expr_rhs),
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntEqual(integer_expr, IntegerExpr::from(natural_expr_rhs)),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntEqual(integer_expr, integer_expr_rhs),
                )),
            },
            Expression::Float(_) => Err(TypeError::TypeMismatch),
        }
    }

    pub fn greater_than_or_equal_to(self, rhs: Self) -> Result<Self, TypeError> {
        match self {
            Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::NatGreaterEq(natural_expr, natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntGreaterEq(IntegerExpr::from(natural_expr), integer_expr_rhs),
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntGreaterEq(integer_expr, IntegerExpr::from(natural_expr_rhs)),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntGreaterEq(integer_expr, integer_expr_rhs),
                )),
            },
        }
    }

    pub fn greater_than(self, rhs: Self) -> Result<Self, TypeError> {
        match self {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::NatGreater(natural_expr, natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntGreater(IntegerExpr::from(natural_expr), integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatGreater(FloatExpr::from(natural_expr), float_expr_rhs),
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntGreater(integer_expr, IntegerExpr::from(natural_expr_rhs)),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntGreater(integer_expr, integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatGreater(FloatExpr::from(integer_expr), float_expr_rhs),
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatGreater(float_expr, FloatExpr::from(natural_expr_rhs)),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatGreater(float_expr, FloatExpr::from(integer_expr_rhs)),
                )),
                Expression::Float(float_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatGreater(float_expr, float_expr_rhs),
                )),
            },
        }
    }

    pub fn less_than(self, rhs: Self) -> Result<Self, TypeError> {
        match self {
            Expression::Boolean(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::NatLess(natural_expr, natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntLess(IntegerExpr::from(natural_expr), integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatLess(FloatExpr::from(natural_expr), float_expr_rhs),
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntLess(integer_expr, IntegerExpr::from(natural_expr_rhs)),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntLess(integer_expr, integer_expr_rhs),
                )),
                Expression::Float(float_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatLess(FloatExpr::from(integer_expr), float_expr_rhs),
                )),
            },
            Expression::Float(float_expr) => match rhs {
                Expression::Boolean(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatLess(float_expr, FloatExpr::from(natural_expr_rhs)),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatLess(float_expr, FloatExpr::from(integer_expr_rhs)),
                )),
                Expression::Float(float_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::FloatLess(float_expr, float_expr_rhs),
                )),
            },
        }
    }

    pub fn less_than_or_equal_to(self, rhs: Self) -> Result<Self, TypeError> {
        match self {
            Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
            Expression::Natural(natural_expr) => match rhs {
                Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::NatLessEq(natural_expr, natural_expr_rhs),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntLessEq(IntegerExpr::from(natural_expr), integer_expr_rhs),
                )),
            },
            Expression::Integer(integer_expr) => match rhs {
                Expression::Boolean(_) | Expression::Float(_) => Err(TypeError::TypeMismatch),
                Expression::Natural(natural_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntLessEq(integer_expr, IntegerExpr::from(natural_expr_rhs)),
                )),
                Expression::Integer(integer_expr_rhs) => Ok(Expression::Boolean(
                    BooleanExpr::IntLessEq(integer_expr, integer_expr_rhs),
                )),
            },
        }
    }

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

// /// Expressions for the language internally used by PGs and CSs.
// ///
// /// [`Expression<V>`] encodes the language in which `V` is the type of variables.
// ///
// /// Note that not all expressions that can be formed are well-typed.
// #[derive(Debug, Clone)]
// pub enum Expression<V>
// where
//     V: Clone,
// {
//     // -------------------
//     // General expressions
//     // -------------------
//     /// A constant value.
//     Const(Val),
//     /// A typed variable.
//     Var(V, Type),
//     // -------------
//     // Random values
//     // -------------
//     /// A Bernoulli distribution with the given probability.
//     RandBool(f64),
//     /// A random integer between a lower bound (included) and an upper bound (excluded).
//     RandInt(Integer, Integer),
//     /// A random float between a lower bound (included) and an upper bound (excluded).
//     RandFloat(Float, Float),
//     // -----------------
//     // Logical operators
//     // -----------------
//     /// n-ary logical conjunction.
//     And(Vec<Expression<V>>),
//     /// n-ary logical disjunction.
//     Or(Vec<Expression<V>>),
//     /// Logical implication.
//     Implies(Box<(Expression<V>, Expression<V>)>),
//     /// Logical negation.
//     Not(Box<Expression<V>>),
//     // --------------------
//     // Arithmetic operators
//     // --------------------
//     /// Opposite of a numerical expression.
//     Opposite(Box<Expression<V>>),
//     /// Arithmetic n-ary sum.
//     Sum(Vec<Expression<V>>),
//     /// Arithmetic n-ary multiplication.
//     Mult(Vec<Expression<V>>),
//     /// Mod operation
//     Mod(Box<(Expression<V>, Expression<V>)>),
//     /// Div operation
//     Div(Box<(Expression<V>, Expression<V>)>),
//     /// Floor
//     Floor(Box<Expression<V>>),
//     // ------------
//     // (In)Equality
//     // ------------
//     /// Equality of numerical expressions.
//     Equal(Box<(Expression<V>, Expression<V>)>),
//     /// Inequality of numerical expressions: LHS greater than RHS.
//     Greater(Box<(Expression<V>, Expression<V>)>),
//     /// Inequality of numerical expressions: LHS greater than, or equal to,  RHS.
//     GreaterEq(Box<(Expression<V>, Expression<V>)>),
//     /// Inequality of numerical expressions: LHS less than RHS.
//     Less(Box<(Expression<V>, Expression<V>)>),
//     /// Inequality of numerical expressions: LHS less than, or equal to, RHS.
//     LessEq(Box<(Expression<V>, Expression<V>)>),
//     // -----
//     // Flow
//     // -----
//     /// If-Then-Else construct, where If must be a boolean expression,
//     /// Then and Else must have the same type,
//     /// and this is also the type of the whole expression.
//     Ite(Box<(Expression<V>, Expression<V>, Expression<V>)>),
// }

// impl<V> Expression<V>
// where
//     V: Clone,
// {
//     /// Computes the type of an expression.
//     ///
//     /// Fails if the expression is badly typed,
//     /// e.g., if variables in it have type incompatible with the expression.
//     pub fn r#type(&self) -> Result<Type, TypeError> {
//         match self {
//             Expression::Const(val) => Ok(val.r#type()),
//             Expression::Var(_var, t) => Ok(*t),
//             Expression::And(props) | Expression::Or(props) => {
//                 if props
//                     .iter()
//                     .map(|prop| prop.r#type())
//                     .collect::<Result<Vec<Type>, TypeError>>()?
//                     .iter()
//                     .all(|prop| matches!(prop, Type::Boolean))
//                 {
//                     Ok(Type::Boolean)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::Implies(props) => {
//                 if matches!(props.0.r#type()?, Type::Boolean)
//                     && matches!(props.1.r#type()?, Type::Boolean)
//                 {
//                     Ok(Type::Boolean)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::Not(prop) => {
//                 if matches!(prop.r#type()?, Type::Boolean) {
//                     Ok(Type::Boolean)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::Opposite(expr) => match expr.r#type()? {
//                 Type::Integer => Ok(Type::Integer),
//                 Type::Float => Ok(Type::Float),
//                 _ => Err(TypeError::TypeMismatch),
//             },
//             Expression::Sum(exprs) | Expression::Mult(exprs) => {
//                 let types = exprs
//                     .iter()
//                     .map(|expr| expr.r#type())
//                     .collect::<Result<Vec<Type>, TypeError>>()?;

//                 if types.iter().all(|expr| matches!(expr, Type::Integer)) {
//                     Ok(Type::Integer)
//                 } else if types
//                     .iter()
//                     .all(|expr| matches!(expr, Type::Integer | Type::Float))
//                 {
//                     Ok(Type::Float)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::Equal(exprs) => {
//                 let type_0 = exprs.0.r#type()?;
//                 let type_1 = exprs.1.r#type()?;
//                 if (matches!(type_0, Type::Boolean) && matches!(type_1, Type::Boolean))
//                     || (matches!(type_0, Type::Integer | Type::Float)
//                         && matches!(type_1, Type::Integer | Type::Float))
//                 {
//                     Ok(Type::Boolean)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::GreaterEq(exprs)
//             | Expression::LessEq(exprs)
//             | Expression::Greater(exprs)
//             | Expression::Less(exprs) => {
//                 if matches!(exprs.0.r#type()?, Type::Integer | Type::Float)
//                     && matches!(exprs.1.r#type()?, Type::Integer | Type::Float)
//                 {
//                     Ok(Type::Boolean)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::Mod(exprs) => {
//                 if matches!(exprs.0.r#type()?, Type::Integer)
//                     && matches!(exprs.1.r#type()?, Type::Integer)
//                 {
//                     Ok(Type::Integer)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::Div(exprs) => {
//                 if matches!(exprs.0.r#type()?, Type::Integer | Type::Float)
//                     && matches!(exprs.1.r#type()?, Type::Integer | Type::Float)
//                 {
//                     Ok(Type::Float)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::RandBool(p) if 0f64 <= *p && *p <= 1f64 => Ok(Type::Boolean),
//             Expression::RandBool(_) => Err(TypeError::BadProbability),
//             Expression::RandInt(l, u) if l < u => Ok(Type::Integer),
//             Expression::RandInt(_, _) => Err(TypeError::BadBounds),
//             Expression::RandFloat(l, u) if l < u => Ok(Type::Float),
//             Expression::RandFloat(_, _) => Err(TypeError::BadBounds),
//             Expression::Ite(exprs) => {
//                 let r#if = exprs.0.r#type()?;
//                 let then = exprs.1.r#type()?;
//                 let r#else = exprs.2.r#type()?;
//                 if matches!(r#if, Type::Boolean) && then == r#else {
//                     Ok(then)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::Floor(expression) => {
//                 if matches!(expression.r#type()?, Type::Float) {
//                     Ok(Type::Integer)
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//         }
//     }

//     /// Evaluates the expression with the given variable assignments and provided RNG.
//     ///
//     /// Will assume the expression (with the variable assignment) is well-typed,
//     /// and may panic if producing an unexpected type.
//     pub fn eval<R: Rng>(&self, vars: &dyn Fn(V) -> Val, rng: &mut R) -> Val {
//         match self {
//             Expression::Const(val) => *val,
//             Expression::Var(var, _t) => vars(var.clone()),
//             Expression::And(exprs) => Val::Boolean(exprs.iter().all(|expr| {
//                 if let Val::Boolean(b) = expr.eval(vars, rng) {
//                     b
//                 } else {
//                     panic!("type mismatch");
//                 }
//             })),
//             Expression::Or(exprs) => Val::Boolean(exprs.iter().any(|expr| {
//                 if let Val::Boolean(b) = expr.eval(vars, rng) {
//                     b
//                 } else {
//                     panic!("type mismatch");
//                 }
//             })),
//             Expression::Implies(exprs) => {
//                 let (lhs, rhs) = exprs.as_ref();
//                 if let (Val::Boolean(lhs), Val::Boolean(rhs)) =
//                     (lhs.eval(vars, rng), rhs.eval(vars, rng))
//                 {
//                     Val::Boolean(rhs || !lhs)
//                 } else {
//                     panic!("type mismatch");
//                 }
//             }
//             Expression::Not(expr) => {
//                 if let Val::Boolean(b) = expr.eval(vars, rng) {
//                     Val::Boolean(!b)
//                 } else {
//                     panic!("type mismatch");
//                 }
//             }
//             Expression::Opposite(expr) => match expr.eval(vars, rng) {
//                 Val::Integer(i) => Val::Integer(-i),
//                 Val::Float(f) => Val::Float(-f),
//                 _ => panic!("type mismatch"),
//             },
//             Expression::Sum(exprs) => exprs.iter().fold(Val::Integer(0), |val, expr| match val {
//                 Val::Integer(acc) => match expr.eval(vars, rng) {
//                     Val::Integer(i) => Val::Integer(acc + i),
//                     Val::Float(f) => Val::Float(f64::from(acc) + f),
//                     _ => panic!("type mismatch"),
//                 },
//                 Val::Float(acc) => match expr.eval(vars, rng) {
//                     Val::Integer(i) => Val::Float(acc + f64::from(i)),
//                     Val::Float(f) => Val::Float(acc + f),
//                     _ => panic!("type mismatch"),
//                 },
//                 _ => panic!("type mismatch"),
//             }),
//             Expression::Mult(exprs) => exprs.iter().fold(Val::Integer(1), |val, expr| match val {
//                 Val::Integer(acc) => match expr.eval(vars, rng) {
//                     Val::Integer(i) => Val::Integer(acc * i),
//                     Val::Float(f) => Val::Float(f64::from(acc) * f),
//                     _ => panic!("type mismatch"),
//                 },
//                 Val::Float(acc) => match expr.eval(vars, rng) {
//                     Val::Integer(i) => Val::Float(acc * f64::from(i)),
//                     Val::Float(f) => Val::Float(acc * f),
//                     _ => panic!("type mismatch"),
//                 },
//                 _ => panic!("type mismatch"),
//             }),
//             Expression::Equal(exprs) => {
//                 let (lhs, rhs) = exprs.as_ref();
//                 match (lhs.eval(vars, rng), rhs.eval(vars, rng)) {
//                     (Val::Integer(lhs), Val::Integer(rhs)) => Val::Boolean(lhs == rhs),
//                     (Val::Integer(lhs), Val::Float(rhs)) => Val::Boolean(lhs as Float == rhs),
//                     (Val::Float(lhs), Val::Integer(rhs)) => Val::Boolean(lhs == rhs as Float),
//                     (Val::Float(lhs), Val::Float(rhs)) => Val::Boolean(lhs == rhs),
//                     (Val::Boolean(lhs), Val::Boolean(rhs)) => Val::Boolean(lhs == rhs),
//                     _ => panic!("type mismatch"),
//                 }
//             }
//             Expression::Greater(exprs) => {
//                 let (lhs, rhs) = exprs.as_ref();
//                 match lhs.eval(vars, rng) {
//                     Val::Integer(lhs) => match rhs.eval(vars, rng) {
//                         Val::Integer(rhs) => Val::Boolean(lhs > rhs),
//                         Val::Float(rhs) => Val::Boolean(f64::from(lhs) > rhs),
//                         _ => panic!("type mismatch"),
//                     },
//                     Val::Float(lhs) => match rhs.eval(vars, rng) {
//                         Val::Integer(rhs) => Val::Boolean(lhs > f64::from(rhs)),
//                         Val::Float(rhs) => Val::Boolean(lhs > rhs),
//                         _ => panic!("type mismatch"),
//                     },
//                     _ => panic!("type mismatch"),
//                 }
//             }
//             Expression::GreaterEq(exprs) => {
//                 let (lhs, rhs) = exprs.as_ref();
//                 match lhs.eval(vars, rng) {
//                     Val::Integer(lhs) => match rhs.eval(vars, rng) {
//                         Val::Integer(rhs) => Val::Boolean(lhs >= rhs),
//                         Val::Float(rhs) => Val::Boolean(f64::from(lhs) >= rhs),
//                         _ => panic!("type mismatch"),
//                     },
//                     Val::Float(lhs) => match rhs.eval(vars, rng) {
//                         Val::Integer(rhs) => Val::Boolean(lhs >= f64::from(rhs)),
//                         Val::Float(rhs) => Val::Boolean(lhs >= rhs),
//                         _ => panic!("type mismatch"),
//                     },
//                     _ => panic!("type mismatch"),
//                 }
//             }
//             Expression::Less(exprs) => {
//                 let (lhs, rhs) = exprs.as_ref();
//                 match lhs.eval(vars, rng) {
//                     Val::Integer(lhs) => match rhs.eval(vars, rng) {
//                         Val::Integer(rhs) => Val::Boolean(lhs < rhs),
//                         Val::Float(rhs) => Val::Boolean(f64::from(lhs) < rhs),
//                         _ => panic!("type mismatch"),
//                     },
//                     Val::Float(lhs) => match rhs.eval(vars, rng) {
//                         Val::Integer(rhs) => Val::Boolean(lhs < f64::from(rhs)),
//                         Val::Float(rhs) => Val::Boolean(lhs < rhs),
//                         _ => panic!("type mismatch"),
//                     },
//                     _ => panic!("type mismatch"),
//                 }
//             }
//             Expression::LessEq(exprs) => {
//                 let (lhs, rhs) = exprs.as_ref();
//                 match lhs.eval(vars, rng) {
//                     Val::Integer(lhs) => match rhs.eval(vars, rng) {
//                         Val::Integer(rhs) => Val::Boolean(lhs <= rhs),
//                         Val::Float(rhs) => Val::Boolean(f64::from(lhs) <= rhs),
//                         _ => panic!("type mismatch"),
//                     },
//                     Val::Float(lhs) => match rhs.eval(vars, rng) {
//                         Val::Integer(rhs) => Val::Boolean(lhs <= f64::from(rhs)),
//                         Val::Float(rhs) => Val::Boolean(lhs <= rhs),
//                         _ => panic!("type mismatch"),
//                     },
//                     _ => panic!("type mismatch"),
//                 }
//             }
//             Expression::Mod(exprs) => {
//                 let (lhs, rhs) = exprs.as_ref();
//                 if let (Val::Integer(lhs), Val::Integer(rhs)) =
//                     (lhs.eval(vars, rng), rhs.eval(vars, rng))
//                 {
//                     Val::Integer(lhs % rhs)
//                 } else {
//                     panic!("type mismatch");
//                 }
//             }
//             Expression::Div(exprs) => {
//                 let (lhs, rhs) = exprs.as_ref();
//                 let num = lhs.eval(vars, rng);
//                 let den = rhs.eval(vars, rng);
//                 match (num, den) {
//                     (Val::Integer(num), Val::Integer(den)) if den != 0 => {
//                         Val::Float(num as f64 / den as f64)
//                     }
//                     (Val::Integer(num), Val::Float(den)) if den != 0. => {
//                         Val::Float(num as f64 / den)
//                     }
//                     (Val::Float(num), Val::Integer(den)) if den != 0 => {
//                         Val::Float(num / den as f64)
//                     }
//                     (Val::Float(num), Val::Float(den)) if den != 0. => Val::Float(num / den),
//                     _ => panic!("type mismatch"),
//                 }
//             }
//             Expression::RandBool(p) => Val::Boolean(rng.random_bool(*p)),
//             Expression::RandInt(l, u) => Val::Integer(rng.random_range(*l..*u)),
//             Expression::RandFloat(l, u) => Val::Float(rng.random_range(*l..*u)),
//             Expression::Ite(exprs) => {
//                 let (r#if, then, r#else) = exprs.as_ref();
//                 if let Val::Boolean(r#if) = r#if.eval(vars, rng) {
//                     if r#if {
//                         then.eval(vars, rng)
//                     } else {
//                         r#else.eval(vars, rng)
//                     }
//                 } else {
//                     panic!("type mismatch");
//                 }
//             }
//             Expression::Floor(expression) => {
//                 if let Val::Float(f) = expression.eval(vars, rng) {
//                     Val::Integer(f.floor() as Integer)
//                 } else {
//                     panic!("type mismatch");
//                 }
//             }
//         }
//     }

//     /// Evals a constant expression.
//     /// Returns an error if expression contains variables.
//     pub fn eval_constant(&self) -> Result<Val, TypeError> {
//         match self {
//             Expression::Const(val) => Ok(*val),
//             Expression::Var(_, _) => Err(TypeError::UnknownVar),
//             Expression::And(props) => props
//                 .iter()
//                 .try_fold(false, |acc, prop| {
//                     let val = prop.eval_constant()?;
//                     if let Val::Boolean(b) = val {
//                         Ok(acc && b)
//                     } else {
//                         Err(TypeError::TypeMismatch)
//                     }
//                 })
//                 .map(Val::Boolean),
//             Expression::Or(props) => props
//                 .iter()
//                 .try_fold(false, |acc, prop| {
//                     let val = prop.eval_constant()?;
//                     if let Val::Boolean(b) = val {
//                         Ok(acc || b)
//                     } else {
//                         Err(TypeError::TypeMismatch)
//                     }
//                 })
//                 .map(Val::Boolean),
//             Expression::Implies(props) => {
//                 if let (Val::Boolean(lhs), Val::Boolean(rhs)) =
//                     (props.0.eval_constant()?, props.1.eval_constant()?)
//                 {
//                     Ok(Val::Boolean(rhs || !lhs))
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::Not(prop) => {
//                 if let Val::Boolean(val) = prop.eval_constant()? {
//                     Ok(Val::Boolean(!val))
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//             Expression::Opposite(expr) => match expr.eval_constant()? {
//                 Val::Integer(i) => Ok(Val::Integer(-i)),
//                 Val::Float(i) => Ok(Val::Float(-i)),
//                 _ => Err(TypeError::TypeMismatch),
//             },
//             Expression::Sum(exprs) => exprs.iter().try_fold(Val::Integer(0), |acc, expr| {
//                 let val = expr.eval_constant()?;
//                 match (acc, val) {
//                     (Val::Integer(acc), Val::Integer(val)) => Ok(Val::Integer(acc + val)),
//                     (Val::Integer(acc), Val::Float(val)) => Ok(Val::Float(f64::from(acc) + val)),
//                     (Val::Float(acc), Val::Integer(val)) => Ok(Val::Float(acc + f64::from(val))),
//                     (Val::Float(acc), Val::Float(val)) => Ok(Val::Float(acc + val)),
//                     _ => Err(TypeError::TypeMismatch),
//                 }
//             }),
//             Expression::Mult(exprs) => exprs.iter().try_fold(Val::Integer(1), |acc, expr| {
//                 let val = expr.eval_constant()?;
//                 match (acc, val) {
//                     (Val::Integer(acc), Val::Integer(val)) => Ok(Val::Integer(acc * val)),
//                     (Val::Integer(acc), Val::Float(val)) => Ok(Val::Float(f64::from(acc) * val)),
//                     (Val::Float(acc), Val::Integer(val)) => Ok(Val::Float(acc * f64::from(val))),
//                     (Val::Float(acc), Val::Float(val)) => Ok(Val::Float(acc * val)),
//                     _ => Err(TypeError::TypeMismatch),
//                 }
//             }),
//             Expression::Div(exprs) => {
//                 let num = exprs.0.eval_constant()?;
//                 let den = exprs.1.eval_constant()?;
//                 match (num, den) {
//                     (Val::Integer(num), Val::Integer(den)) if den != 0 => {
//                         Ok(Val::Float(num as f64 / den as f64))
//                     }
//                     (Val::Integer(num), Val::Float(den)) if den != 0. => {
//                         Ok(Val::Float(num as f64 / den))
//                     }
//                     (Val::Float(num), Val::Integer(den)) if den != 0 => {
//                         Ok(Val::Float(num / den as f64))
//                     }
//                     (Val::Float(num), Val::Float(den)) if den != 0. => Ok(Val::Float(num / den)),
//                     _ => Err(TypeError::TypeMismatch),
//                 }
//             }
//             Expression::RandBool(_) => todo!(),
//             Expression::RandInt(_, _) => todo!(),
//             Expression::RandFloat(_, _) => todo!(),
//             Expression::Mod(_) => todo!(),
//             Expression::Equal(_) => todo!(),
//             Expression::Greater(_) => todo!(),
//             Expression::GreaterEq(_) => todo!(),
//             Expression::Less(_) => todo!(),
//             Expression::LessEq(_) => todo!(),
//             Expression::Ite(_) => todo!(),
//             Expression::Floor(expression) => {
//                 if let Val::Float(f) = expression.eval_constant()? {
//                     Ok(Val::Integer(f.floor() as Integer))
//                 } else {
//                     Err(TypeError::TypeMismatch)
//                 }
//             }
//         }
//     }

//     pub(crate) fn context(&self, vars: &dyn Fn(V) -> Option<Type>) -> Result<(), TypeError> {
//         match self {
//             Expression::Var(var, t) => {
//                 if let Some(var_t) = vars(var.clone()) {
//                     if &var_t == t {
//                         Ok(())
//                     } else {
//                         Err(TypeError::TypeMismatch)
//                     }
//                 } else {
//                     Err(TypeError::UnknownVar)
//                 }
//             }
//             Expression::Const(_)
//             | Expression::RandBool(_)
//             | Expression::RandInt(_, _)
//             | Expression::RandFloat(_, _) => Ok(()),
//             Expression::And(tuple)
//             | Expression::Or(tuple)
//             | Expression::Sum(tuple)
//             | Expression::Mult(tuple) => tuple.iter().try_for_each(|expr| expr.context(vars)),
//             Expression::Not(expr) | Expression::Opposite(expr) => expr.context(vars),
//             Expression::Implies(exprs)
//             | Expression::Equal(exprs)
//             | Expression::Greater(exprs)
//             | Expression::GreaterEq(exprs)
//             | Expression::Less(exprs)
//             | Expression::LessEq(exprs)
//             | Expression::Mod(exprs)
//             | Expression::Div(exprs) => exprs.0.context(vars).and_then(|_| exprs.1.context(vars)),
//             Expression::Ite(exprs) => exprs
//                 .0
//                 .context(vars)
//                 .and_then(|_| exprs.1.context(vars))
//                 .and_then(|_| exprs.2.context(vars)),
//             Expression::Floor(expression) => expression.context(vars),
//         }
//     }

//     /// Creates the disjunction of a list of expressions.
//     ///
//     /// Optimizes automatically nested disjunctions through associativity.
//     pub fn and(args: Vec<Self>) -> Result<Self, TypeError> {
//         args.iter().try_for_each(|arg| {
//             matches!(arg.r#type()?, Type::Boolean)
//                 .then_some(())
//                 .ok_or(TypeError::TypeMismatch)
//         })?;
//         match args.len() {
//             0 => Ok(Expression::Const(Val::Boolean(true))),
//             1 => Ok(args[0].clone()),
//             _ => {
//                 let mut subformulae = Vec::new();
//                 for subformula in args.into_iter() {
//                     if let Expression::And(subs) = subformula {
//                         subformulae.extend(subs);
//                     } else {
//                         subformulae.push(subformula);
//                     }
//                 }
//                 Ok(Expression::And(subformulae))
//             }
//         }
//     }

//     /// Creates the conjunction of a list of expressions.
//     ///
//     /// Optimizes automatically nested conjunctions through associativity.
//     pub fn or(args: Vec<Self>) -> Result<Self, TypeError> {
//         args.iter().try_for_each(|arg| {
//             matches!(arg.r#type()?, Type::Boolean)
//                 .then_some(())
//                 .ok_or(TypeError::TypeMismatch)
//         })?;
//         match args.len() {
//             0 => Ok(Expression::Const(Val::Boolean(false))),
//             1 => Ok(args[0].clone()),
//             _ => {
//                 let mut subformulae = Vec::new();
//                 for subformula in args.into_iter() {
//                     if let Expression::Or(subs) = subformula {
//                         subformulae.extend(subs);
//                     } else {
//                         subformulae.push(subformula);
//                     }
//                 }
//                 Ok(Expression::Or(subformulae))
//             }
//         }
//     }
// }

// impl<V> std::ops::Not for Expression<V>
// where
//     V: Clone,
// {
//     type Output = Result<Self, TypeError>;

//     fn not(self) -> Self::Output {
//         if let Type::Boolean = self.r#type()? {
//             if let Expression::Not(sub) = self {
//                 Ok(*sub)
//             } else {
//                 Ok(Expression::Not(Box::new(self)))
//             }
//         } else {
//             Err(TypeError::TypeMismatch)
//         }
//     }
// }

// impl<V> std::ops::Neg for Expression<V>
// where
//     V: Clone,
// {
//     type Output = Self;

//     fn neg(self) -> Self::Output {
//         if let Expression::Opposite(sub) = self {
//             *sub
//         } else {
//             Expression::Opposite(Box::new(self))
//         }
//     }
// }

// impl<V> std::ops::Add for Expression<V>
// where
//     V: Clone,
// {
//     type Output = Self;

//     fn add(self, rhs: Self) -> Self::Output {
//         let mut subformulae = Vec::new();
//         if let Expression::Sum(subs) = self {
//             subformulae.extend(subs);
//         } else {
//             subformulae.push(self);
//         }
//         if let Expression::Sum(subs) = rhs {
//             subformulae.extend(subs);
//         } else {
//             subformulae.push(rhs);
//         }
//         Expression::Sum(subformulae)
//     }
// }

// impl<V> std::ops::Mul for Expression<V>
// where
//     V: Clone,
// {
//     type Output = Self;

//     fn mul(self, rhs: Self) -> Self::Output {
//         let mut subformulae = Vec::new();
//         if let Expression::Mult(subs) = self {
//             subformulae.extend(subs);
//         } else {
//             subformulae.push(self);
//         }
//         if let Expression::Mult(subs) = rhs {
//             subformulae.extend(subs);
//         } else {
//             subformulae.push(rhs);
//         }
//         Expression::Mult(subformulae)
//     }
// }

// impl<V> std::iter::Sum for Expression<V>
// where
//     V: Clone,
// {
//     fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
//         iter.reduce(|acc, e| acc + e).unwrap_or(Self::from(0))
//     }
// }

// impl<V> std::iter::Product for Expression<V>
// where
//     V: Clone,
// {
//     fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
//         iter.reduce(|acc, e| acc * e).unwrap_or(Self::from(1))
//     }
// }

// impl<V> From<bool> for Expression<V>
// where
//     V: Clone,
// {
//     fn from(value: bool) -> Self {
//         Expression::Const(Val::Boolean(value))
//     }
// }

// impl<V> From<Integer> for Expression<V>
// where
//     V: Clone,
// {
//     fn from(value: Integer) -> Self {
//         Expression::Const(Val::Integer(value))
//     }
// }

// impl<V> From<Float> for Expression<V>
// where
//     V: Clone,
// {
//     fn from(value: Float) -> Self {
//         Expression::Const(Val::Float(value))
//     }
// }
