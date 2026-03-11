use std::ops::{BitAnd, BitOr, Not};

use rand::{Rng, RngExt};

use crate::{
    Expression, Type, TypeError, Val,
    grammar::{FloatExpr, IntegerExpr, NaturalExpr},
};

#[derive(Debug, Clone)]
pub enum BooleanExpr<V>
where
    V: Clone,
{
    Const(bool),
    Var(V),
    /// A Bernoulli distribution with the given probability.
    Rand(FloatExpr<V>),
    // -----------------
    // Logical operators
    // -----------------
    /// n-ary logical conjunction.
    And(Vec<BooleanExpr<V>>),
    /// n-ary logical disjunction.
    Or(Vec<BooleanExpr<V>>),
    /// Logical implication.
    Implies(Box<(BooleanExpr<V>, BooleanExpr<V>)>),
    /// Logical negation.
    Not(Box<BooleanExpr<V>>),
    // ------------
    // (In)Equality
    // ------------
    /// Equality of numerical expressions.
    NatEqual(NaturalExpr<V>, NaturalExpr<V>),
    IntEqual(IntegerExpr<V>, IntegerExpr<V>),
    FloatEqual(FloatExpr<V>, FloatExpr<V>),
    /// Inequality of numerical expressions: LHS greater than RHS.
    NatGreater(NaturalExpr<V>, NaturalExpr<V>),
    IntGreater(IntegerExpr<V>, IntegerExpr<V>),
    FloatGreater(FloatExpr<V>, FloatExpr<V>),
    /// Inequality of numerical expressions: LHS greater than, or equal to,  RHS.
    NatGreaterEq(NaturalExpr<V>, NaturalExpr<V>),
    IntGreaterEq(IntegerExpr<V>, IntegerExpr<V>),
    FloatGreaterEq(FloatExpr<V>, FloatExpr<V>),
    /// Inequality of numerical expressions: LHS less than RHS.
    NatLess(NaturalExpr<V>, NaturalExpr<V>),
    IntLess(IntegerExpr<V>, IntegerExpr<V>),
    FloatLess(FloatExpr<V>, FloatExpr<V>),
    /// Inequality of numerical expressions: LHS less than, or equal to, RHS.
    NatLessEq(NaturalExpr<V>, NaturalExpr<V>),
    IntLessEq(IntegerExpr<V>, IntegerExpr<V>),
    FloatLessEq(FloatExpr<V>, FloatExpr<V>),
    // -----
    // Flow
    // -----
    /// If-Then-Else construct, where If must be a boolean expression,
    /// Then and Else must have the same type,
    /// and this is also the type of the whole expression.
    Ite(Box<(BooleanExpr<V>, BooleanExpr<V>, BooleanExpr<V>)>),
}

impl<V> BooleanExpr<V>
where
    V: Clone,
{
    pub fn is_constant(&self) -> bool {
        match self {
            BooleanExpr::Const(_) => true,
            BooleanExpr::Var(_) => false,
            BooleanExpr::Rand(_float_expr) => false,
            BooleanExpr::And(boolean_exprs) | BooleanExpr::Or(boolean_exprs) => {
                boolean_exprs.iter().all(Self::is_constant)
            }
            BooleanExpr::Implies(args) => {
                let (lhs, rhs) = args.as_ref();
                lhs.is_constant() && rhs.is_constant()
            }
            BooleanExpr::Not(boolean_expr) => boolean_expr.is_constant(),
            BooleanExpr::NatEqual(natural_expr_lhs, natural_expr_rhs)
            | BooleanExpr::NatGreater(natural_expr_lhs, natural_expr_rhs)
            | BooleanExpr::NatGreaterEq(natural_expr_lhs, natural_expr_rhs)
            | BooleanExpr::NatLess(natural_expr_lhs, natural_expr_rhs)
            | BooleanExpr::NatLessEq(natural_expr_lhs, natural_expr_rhs) => {
                natural_expr_lhs.is_constant() && natural_expr_rhs.is_constant()
            }
            BooleanExpr::IntEqual(integer_expr, integer_expr1)
            | BooleanExpr::IntGreater(integer_expr, integer_expr1)
            | BooleanExpr::IntGreaterEq(integer_expr, integer_expr1)
            | BooleanExpr::IntLess(integer_expr, integer_expr1)
            | BooleanExpr::IntLessEq(integer_expr, integer_expr1) => {
                integer_expr.is_constant() && integer_expr1.is_constant()
            }
            BooleanExpr::FloatEqual(float_expr, float_expr1)
            | BooleanExpr::FloatLess(float_expr, float_expr1)
            | BooleanExpr::FloatLessEq(float_expr, float_expr1)
            | BooleanExpr::FloatGreater(float_expr, float_expr1)
            | BooleanExpr::FloatGreaterEq(float_expr, float_expr1) => {
                float_expr.is_constant() && float_expr1.is_constant()
            }
            BooleanExpr::Ite(args) => {
                let (ite, lhs, rhs) = args.as_ref();
                ite.is_constant() && lhs.is_constant() && rhs.is_constant()
            }
        }
    }

    pub fn eval<R: Rng>(&self, vars: &dyn Fn(&V) -> Val, rng: &mut R) -> bool {
        match self {
            BooleanExpr::Const(b) => *b,
            BooleanExpr::Var(var) => {
                if let Val::Boolean(b) = vars(var) {
                    b
                } else {
                    panic!("type mismatch: expected boolean variable")
                }
            }
            BooleanExpr::Rand(float_expr) => {
                let bernoulli = float_expr.eval(vars, rng);
                rng.random_bool(bernoulli)
            }
            BooleanExpr::And(boolean_exprs) => boolean_exprs
                .iter()
                .all(|boolean_expr| boolean_expr.eval(vars, rng)),
            BooleanExpr::Or(boolean_exprs) => boolean_exprs
                .iter()
                .any(|boolean_expr| boolean_expr.eval(vars, rng)),
            BooleanExpr::Implies(boolean_exprs) => {
                let (lhs, rhs) = boolean_exprs.as_ref();
                rhs.eval(vars, rng) || !lhs.eval(vars, rng)
            }
            BooleanExpr::Not(boolean_expr) => !&boolean_expr.eval(vars, rng),
            BooleanExpr::NatEqual(natural_expr_lhs, natural_expr_rhs) => {
                natural_expr_lhs.eval(vars, rng) == natural_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::IntEqual(integer_expr_lhs, integer_expr_rhs) => {
                integer_expr_lhs.eval(vars, rng) == integer_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::FloatEqual(float_expr_lhs, float_expr_rhs) => {
                float_expr_lhs.eval(vars, rng) == float_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::NatGreater(natural_expr_lhs, natural_expr_rhs) => {
                natural_expr_lhs.eval(vars, rng) > natural_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::IntGreater(integer_expr_lhs, integer_expr_rhs) => {
                integer_expr_lhs.eval(vars, rng) > integer_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::FloatGreater(float_expr_lhs, float_expr_rhs) => {
                float_expr_lhs.eval(vars, rng) > float_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::NatGreaterEq(natural_expr_lhs, natural_expr_rhs) => {
                natural_expr_lhs.eval(vars, rng) >= natural_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::IntGreaterEq(integer_expr_lhs, integer_expr_rhs) => {
                integer_expr_lhs.eval(vars, rng) >= integer_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::FloatGreaterEq(float_expr_lhs, float_expr_rhs) => {
                float_expr_lhs.eval(vars, rng) >= float_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::NatLess(natural_expr_lhs, natural_expr_rhs) => {
                natural_expr_lhs.eval(vars, rng) < natural_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::IntLess(integer_expr_lhs, integer_expr_rhs) => {
                integer_expr_lhs.eval(vars, rng) < integer_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::FloatLess(float_expr_lhs, float_expr_rhs) => {
                float_expr_lhs.eval(vars, rng) < float_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::NatLessEq(natural_expr_lhs, natural_expr_rhs) => {
                natural_expr_lhs.eval(vars, rng) <= natural_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::IntLessEq(integer_expr_lhs, integer_expr_rhs) => {
                integer_expr_lhs.eval(vars, rng) <= integer_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::FloatLessEq(float_expr_lhs, float_expr_rhs) => {
                float_expr_lhs.eval(vars, rng) <= float_expr_rhs.eval(vars, rng)
            }
            BooleanExpr::Ite(args) => {
                let (ite, lhs, rhs) = args.as_ref();
                if ite.eval(vars, rng) {
                    lhs.eval(vars, rng)
                } else {
                    rhs.eval(vars, rng)
                }
            }
        }
    }

    pub fn map<W: Clone>(self, map: &dyn Fn(V) -> W) -> BooleanExpr<W> {
        match self {
            BooleanExpr::Const(b) => BooleanExpr::Const(b),
            BooleanExpr::Var(var) => BooleanExpr::Var(map(var)),
            BooleanExpr::Rand(float_expr) => BooleanExpr::Rand(float_expr.map(map)),
            BooleanExpr::And(boolean_exprs) => BooleanExpr::And(
                boolean_exprs
                    .into_iter()
                    .map(|expr| expr.map(map))
                    .collect(),
            ),
            BooleanExpr::Or(boolean_exprs) => BooleanExpr::Or(
                boolean_exprs
                    .into_iter()
                    .map(|expr| expr.map(map))
                    .collect(),
            ),
            BooleanExpr::Implies(args) => {
                let (lhs, rhs) = *args;
                BooleanExpr::Implies(Box::new((lhs.map(map), rhs.map(map))))
            }
            BooleanExpr::Not(boolean_expr) => BooleanExpr::Not(Box::new(boolean_expr.map(map))),
            BooleanExpr::NatEqual(natural_expr_lhs, natural_expr_rhs) => {
                BooleanExpr::NatEqual(natural_expr_lhs.map(map), natural_expr_rhs.map(map))
            }
            BooleanExpr::IntEqual(integer_expr_lhs, integer_expr_rhs) => {
                BooleanExpr::IntEqual(integer_expr_lhs.map(map), integer_expr_rhs.map(map))
            }
            BooleanExpr::FloatEqual(float_expr_lhs, float_expr_rhs) => {
                BooleanExpr::FloatEqual(float_expr_lhs.map(map), float_expr_rhs.map(map))
            }
            BooleanExpr::NatGreater(natural_expr_lhs, natural_expr_rhs) => {
                BooleanExpr::NatEqual(natural_expr_lhs.map(map), natural_expr_rhs.map(map))
            }
            BooleanExpr::IntGreater(integer_expr_lhs, integer_expr_rhs) => {
                BooleanExpr::IntGreater(integer_expr_lhs.map(map), integer_expr_rhs.map(map))
            }
            BooleanExpr::FloatGreater(float_expr_lhs, float_expr_rhs) => {
                BooleanExpr::FloatGreater(float_expr_lhs.map(map), float_expr_rhs.map(map))
            }
            BooleanExpr::NatGreaterEq(natural_expr_lhs, natural_expr_rhs) => {
                BooleanExpr::NatGreaterEq(natural_expr_lhs.map(map), natural_expr_rhs.map(map))
            }
            BooleanExpr::IntGreaterEq(integer_expr_lhs, integer_expr_rhs) => {
                BooleanExpr::IntGreaterEq(integer_expr_lhs.map(map), integer_expr_rhs.map(map))
            }
            BooleanExpr::FloatGreaterEq(float_expr_lhs, float_expr_rhs) => {
                BooleanExpr::FloatGreaterEq(float_expr_lhs.map(map), float_expr_rhs.map(map))
            }
            BooleanExpr::NatLess(natural_expr_lhs, natural_expr_rhs) => {
                BooleanExpr::NatLess(natural_expr_lhs.map(map), natural_expr_rhs.map(map))
            }
            BooleanExpr::IntLess(integer_expr_lhs, integer_expr_rhs) => {
                BooleanExpr::IntLess(integer_expr_lhs.map(map), integer_expr_rhs.map(map))
            }
            BooleanExpr::FloatLess(float_expr_lhs, float_expr_rhs) => {
                BooleanExpr::FloatLess(float_expr_lhs.map(map), float_expr_rhs.map(map))
            }
            BooleanExpr::NatLessEq(natural_expr_lhs, natural_expr_rhs) => {
                BooleanExpr::NatLessEq(natural_expr_lhs.map(map), natural_expr_rhs.map(map))
            }
            BooleanExpr::IntLessEq(integer_expr_lhs, integer_expr_rhs) => {
                BooleanExpr::IntLessEq(integer_expr_lhs.map(map), integer_expr_rhs.map(map))
            }
            BooleanExpr::FloatLessEq(float_expr_lhs, float_expr_rhs) => {
                BooleanExpr::FloatLessEq(float_expr_lhs.map(map), float_expr_rhs.map(map))
            }
            BooleanExpr::Ite(args) => {
                let (r#if, then, r#else) = *args;
                BooleanExpr::Ite(Box::new((r#if.map(map), then.map(map), r#else.map(map))))
            }
        }
    }

    pub(crate) fn context(&self, vars: &dyn Fn(V) -> Option<Type>) -> Result<(), TypeError> {
        match self {
            BooleanExpr::Const(_) => todo!(),
            BooleanExpr::Var(_) => todo!(),
            BooleanExpr::Rand(float_expr) => todo!(),
            BooleanExpr::And(boolean_exprs) => todo!(),
            BooleanExpr::Or(boolean_exprs) => todo!(),
            BooleanExpr::Implies(_) => todo!(),
            BooleanExpr::Not(boolean_expr) => todo!(),
            BooleanExpr::NatEqual(natural_expr, natural_expr1) => todo!(),
            BooleanExpr::IntEqual(integer_expr, integer_expr1) => todo!(),
            BooleanExpr::NatGreater(natural_expr, natural_expr1) => todo!(),
            BooleanExpr::IntGreater(integer_expr, integer_expr1) => todo!(),
            BooleanExpr::FloatGreater(float_expr, float_expr1) => todo!(),
            BooleanExpr::NatGreaterEq(natural_expr, natural_expr1) => todo!(),
            BooleanExpr::IntGreaterEq(integer_expr, integer_expr1) => todo!(),
            BooleanExpr::NatLess(natural_expr, natural_expr1) => todo!(),
            BooleanExpr::IntLess(integer_expr, integer_expr1) => todo!(),
            BooleanExpr::FloatLess(float_expr, float_expr1) => todo!(),
            BooleanExpr::NatLessEq(natural_expr, natural_expr1) => todo!(),
            BooleanExpr::IntLessEq(integer_expr, integer_expr1) => todo!(),
            BooleanExpr::Ite(_) => todo!(),
            BooleanExpr::FloatEqual(float_expr, float_expr1) => todo!(),
            BooleanExpr::FloatGreaterEq(float_expr, float_expr1) => todo!(),
            BooleanExpr::FloatLessEq(float_expr, float_expr1) => todo!(),
        }
    }
}

impl<V> From<bool> for BooleanExpr<V>
where
    V: Clone,
{
    fn from(value: bool) -> Self {
        Self::Const(value)
    }
}

impl<V> TryFrom<Expression<V>> for BooleanExpr<V>
where
    V: Clone,
{
    type Error = TypeError;

    fn try_from(value: Expression<V>) -> Result<Self, Self::Error> {
        if let Expression::Boolean(bool_expr) = value {
            Ok(bool_expr)
        } else {
            Err(TypeError::TypeMismatch)
        }
    }
}

impl<V> Not for BooleanExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn not(self) -> Self::Output {
        if let Self::Not(expr) = self {
            *expr
        } else {
            Self::Not(Box::new(self))
        }
    }
}

impl<V> BitAnd for BooleanExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn bitand(mut self, mut rhs: Self) -> Self::Output {
        if let BooleanExpr::And(ref mut exprs) = self {
            if let BooleanExpr::And(rhs_exprs) = rhs {
                exprs.extend(rhs_exprs);
            } else {
                exprs.push(rhs);
            }
            self
        } else if let BooleanExpr::And(ref mut rhs_exprs) = rhs {
            rhs_exprs.push(self);
            rhs
        } else {
            BooleanExpr::And(vec![self, rhs])
        }
    }
}

impl<V> BitOr for BooleanExpr<V>
where
    V: Clone,
{
    type Output = Self;

    fn bitor(mut self, mut rhs: Self) -> Self::Output {
        if let BooleanExpr::And(ref mut exprs) = self {
            if let BooleanExpr::Or(rhs_exprs) = rhs {
                exprs.extend(rhs_exprs);
            } else {
                exprs.push(rhs);
            }
            self
        } else if let BooleanExpr::Or(ref mut rhs_exprs) = rhs {
            rhs_exprs.push(self);
            rhs
        } else {
            BooleanExpr::Or(vec![self, rhs])
        }
    }
}
