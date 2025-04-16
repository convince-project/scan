//! The language used by PGs and CSs.
//!
//! The type [`Expression<V>`] encodes the used language,
//! where `V` is the type parameter of variables.
//! The language features base types and product types,
//! Boolean logic and basic arithmetic expressions.

use rand::Rng;
use std::hash::Hash;
use thiserror::Error;

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
            Type::Float => Val::Float(0.0),
            Type::Product(tuple) => {
                Val::Tuple(Vec::from_iter(tuple.iter().map(Self::default_value)))
            }
            Type::List(t) => Val::List((**t).clone(), Vec::new()),
        }
    }
}

/// Integer values.
pub type Integer = i32;

/// Floating-point values.
pub type Float = f64;

/// Possible values for each [`Type`].
#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    /// Boolean values.
    Boolean(bool),
    /// Integer values.
    Integer(Integer),
    /// Floating-point values.
    Float(Float),
    /// Values for product types, i.e., tuples of suitable values.
    Tuple(Vec<Val>),
    /// Values for list types
    List(Type, Vec<Val>),
}

impl Val {
    /// Returns the [`Type`] of the value.
    pub fn r#type(&self) -> Type {
        match self {
            Val::Boolean(_) => Type::Boolean,
            Val::Integer(_) => Type::Integer,
            Val::Tuple(comps) => Type::Product(comps.iter().map(Val::r#type).collect()),
            Val::List(t, _) => Type::List(Box::new(t.clone())),
            Val::Float(_) => Type::Float,
        }
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
    // -------------------
    // General expressions
    // -------------------
    /// A constant value.
    Const(Val),
    /// A typed variable.
    Var(V, Type),
    /// A tuple of expressions.
    Tuple(Vec<Expression<V>>),
    /// The component of a tuple.
    Component(usize, Box<Expression<V>>),
    // -------------
    // Random values
    // -------------
    /// A Bernulli distribution with the given probability.
    RandBool(f64),
    /// A random integer between a lower bound (included) and an upper bound (excluded).
    RandInt(Integer, Integer),
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
    /// Mod operation
    Mod(Box<(Expression<V>, Expression<V>)>),
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

impl<V> Expression<V>
where
    V: Clone,
{
    /// Computes the type of an expression.
    ///
    /// Fails if the expression is badly typed,
    /// e.g., if variables in it have type incompatible with the expression.
    pub fn r#type(&self) -> Result<Type, TypeError> {
        match self {
            Expression::Const(val) => Ok(val.r#type()),
            Expression::Tuple(tuple) => tuple
                .iter()
                .map(|e| e.r#type())
                .collect::<Result<Vec<Type>, TypeError>>()
                .map(Type::Product),
            Expression::Var(_var, t) => Ok(t.clone()),
            Expression::And(props) | Expression::Or(props) => {
                if props
                    .iter()
                    .map(|prop| prop.r#type())
                    .collect::<Result<Vec<Type>, TypeError>>()?
                    .iter()
                    .all(|prop| matches!(prop, Type::Boolean))
                {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Implies(props) => {
                if matches!(props.0.r#type()?, Type::Boolean)
                    && matches!(props.1.r#type()?, Type::Boolean)
                {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Not(prop) => {
                if matches!(prop.r#type()?, Type::Boolean) {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Opposite(expr) => match expr.r#type()? {
                Type::Integer => Ok(Type::Integer),
                Type::Float => Ok(Type::Float),
                _ => Err(TypeError::TypeMismatch),
            },
            Expression::Sum(exprs) | Expression::Mult(exprs) => {
                let types = exprs
                    .iter()
                    .map(|expr| expr.r#type())
                    .collect::<Result<Vec<Type>, TypeError>>()?;

                if types.iter().all(|expr| matches!(expr, Type::Integer)) {
                    Ok(Type::Integer)
                } else if types
                    .iter()
                    .all(|expr| matches!(expr, Type::Integer | Type::Float))
                {
                    Ok(Type::Float)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Equal(exprs) => {
                let type_0 = exprs.0.r#type()?;
                let type_1 = exprs.1.r#type()?;
                if matches!(type_0, Type::Integer | Type::Boolean) && type_0 == type_1 {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::GreaterEq(exprs) | Expression::LessEq(exprs) => {
                let type_0 = exprs.0.r#type()?;
                let type_1 = exprs.1.r#type()?;
                if matches!(type_0, Type::Integer) && matches!(type_1, Type::Integer) {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Greater(exprs) | Expression::Less(exprs) => {
                if matches!(exprs.0.r#type()?, Type::Integer | Type::Float)
                    && matches!(exprs.1.r#type()?, Type::Integer | Type::Float)
                {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Component(index, expr) => {
                if let Type::Product(components) = expr.r#type()? {
                    components
                        .get(*index)
                        .cloned()
                        .ok_or(TypeError::MissingComponent)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Append(exprs) => {
                let list_type = exprs.0.r#type()?;
                let element_type = exprs.1.r#type()?;
                if let Type::List(ref elements_type) = list_type {
                    if &element_type == elements_type.as_ref() {
                        Ok(list_type)
                    } else {
                        Err(TypeError::TypeMismatch)
                    }
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Truncate(list) => {
                let list_type = list.r#type()?;
                if let Type::List(_) = list_type {
                    Ok(list_type)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Len(list) => {
                let list_type = list.r#type()?;
                if let Type::List(_) = list_type {
                    Ok(Type::Integer)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Mod(exprs) => {
                if matches!(exprs.0.r#type()?, Type::Integer)
                    && matches!(exprs.1.r#type()?, Type::Integer)
                {
                    Ok(Type::Integer)
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::RandBool(p) if 0f64 <= *p && *p <= 1f64 => Ok(Type::Boolean),
            Expression::RandBool(_) => Err(TypeError::BadProbability),
            Expression::RandInt(l, u) if l < u => Ok(Type::Integer),
            Expression::RandInt(_, _) => Err(TypeError::BadBounds),
        }
    }

    /// Evals a constant expression.
    /// Returns an error if expression contains variables.
    pub fn eval_constant(&self) -> Result<Val, TypeError> {
        match self {
            Expression::Const(val) => Ok(val.clone()),
            Expression::Tuple(tuple) => tuple
                .iter()
                .map(|e| e.eval_constant())
                .collect::<Result<Vec<Val>, TypeError>>()
                .map(Val::Tuple),
            Expression::Var(_, _) => Err(TypeError::UnknownVar),
            Expression::And(props) => props
                .iter()
                .try_fold(false, |acc, prop| {
                    let val = prop.eval_constant()?;
                    if let Val::Boolean(b) = val {
                        Ok(acc && b)
                    } else {
                        Err(TypeError::TypeMismatch)
                    }
                })
                .map(Val::Boolean),
            Expression::Or(props) => props
                .iter()
                .try_fold(false, |acc, prop| {
                    let val = prop.eval_constant()?;
                    if let Val::Boolean(b) = val {
                        Ok(acc || b)
                    } else {
                        Err(TypeError::TypeMismatch)
                    }
                })
                .map(Val::Boolean),
            Expression::Implies(props) => {
                if let (Val::Boolean(lhs), Val::Boolean(rhs)) =
                    (props.0.eval_constant()?, props.1.eval_constant()?)
                {
                    Ok(Val::Boolean(rhs || !lhs))
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Not(prop) => {
                if let Val::Boolean(val) = prop.eval_constant()? {
                    Ok(Val::Boolean(!val))
                } else {
                    Err(TypeError::TypeMismatch)
                }
            }
            Expression::Opposite(expr) => match expr.eval_constant()? {
                Val::Integer(i) => Ok(Val::Integer(-i)),
                Val::Float(i) => Ok(Val::Float(-i)),
                _ => Err(TypeError::TypeMismatch),
            },
            Expression::Sum(exprs) => exprs.iter().try_fold(Val::Integer(0), |acc, expr| {
                let val = expr.eval_constant()?;
                match (acc, val) {
                    (Val::Integer(acc), Val::Integer(val)) => Ok(Val::Integer(acc + val)),
                    (Val::Integer(acc), Val::Float(val)) => Ok(Val::Float(f64::from(acc) + val)),
                    (Val::Float(acc), Val::Integer(val)) => Ok(Val::Float(acc + f64::from(val))),
                    (Val::Float(acc), Val::Float(val)) => Ok(Val::Float(acc + val)),
                    _ => Err(TypeError::TypeMismatch),
                }
            }),
            Expression::Mult(exprs) => exprs.iter().try_fold(Val::Integer(1), |acc, expr| {
                let val = expr.eval_constant()?;
                match (acc, val) {
                    (Val::Integer(acc), Val::Integer(val)) => Ok(Val::Integer(acc * val)),
                    (Val::Integer(acc), Val::Float(val)) => Ok(Val::Float(f64::from(acc) * val)),
                    (Val::Float(acc), Val::Integer(val)) => Ok(Val::Float(acc * f64::from(val))),
                    (Val::Float(acc), Val::Float(val)) => Ok(Val::Float(acc * val)),
                    _ => Err(TypeError::TypeMismatch),
                }
            }),
            Expression::Component(_, expression) => todo!(),
            Expression::RandBool(_) => todo!(),
            Expression::RandInt(_, _) => todo!(),
            Expression::Mod(_) => todo!(),
            Expression::Equal(_) => todo!(),
            Expression::Greater(_) => todo!(),
            Expression::GreaterEq(_) => todo!(),
            Expression::Less(_) => todo!(),
            Expression::LessEq(_) => todo!(),
            Expression::Append(_) => todo!(),
            Expression::Truncate(expression) => todo!(),
            Expression::Len(expression) => todo!(),
        }
    }

    pub(crate) fn context(&self, vars: &dyn Fn(V) -> Option<Type>) -> Result<(), TypeError> {
        match self {
            Expression::Var(var, t) => {
                if let Some(var_t) = vars(var.clone()) {
                    if &var_t == t {
                        Ok(())
                    } else {
                        Err(TypeError::TypeMismatch)
                    }
                } else {
                    Err(TypeError::UnknownVar)
                }
            }
            Expression::Const(_) | Expression::RandBool(_) | Expression::RandInt(_, _) => Ok(()),
            Expression::Tuple(tuple)
            | Expression::And(tuple)
            | Expression::Or(tuple)
            | Expression::Sum(tuple)
            | Expression::Mult(tuple) => tuple.iter().try_for_each(|expr| expr.context(vars)),
            Expression::Component(_, expr)
            | Expression::Not(expr)
            | Expression::Opposite(expr)
            | Expression::Truncate(expr)
            | Expression::Len(expr) => expr.context(vars),
            Expression::Implies(exprs)
            | Expression::Equal(exprs)
            | Expression::Greater(exprs)
            | Expression::GreaterEq(exprs)
            | Expression::Less(exprs)
            | Expression::LessEq(exprs)
            | Expression::Mod(exprs)
            | Expression::Append(exprs) => {
                exprs.0.context(vars).and_then(|_| exprs.1.context(vars))
            }
        }
    }

    /// Creates the disjunction of a list of expressions.
    ///
    /// Optimizes automatically nested disjunctions through associativity.
    pub fn and(args: Vec<Self>) -> Result<Self, TypeError> {
        args.iter().try_for_each(|arg| {
            matches!(arg.r#type()?, Type::Boolean)
                .then_some(())
                .ok_or(TypeError::TypeMismatch)
        })?;
        match args.len() {
            0 => Ok(Expression::Const(Val::Boolean(true))),
            1 => Ok(args[0].clone()),
            _ => {
                let mut subformulae = Vec::new();
                for subformula in args.into_iter() {
                    if let Expression::And(subs) = subformula {
                        subformulae.extend(subs);
                    } else {
                        subformulae.push(subformula);
                    }
                }
                Ok(Expression::And(subformulae))
            }
        }
    }

    /// Creates the conjunction of a list of expressions.
    ///
    /// Optimizes automatically nested conjunctions through associativity.
    pub fn or(args: Vec<Self>) -> Result<Self, TypeError> {
        args.iter().try_for_each(|arg| {
            matches!(arg.r#type()?, Type::Boolean)
                .then_some(())
                .ok_or(TypeError::TypeMismatch)
        })?;
        match args.len() {
            0 => Ok(Expression::Const(Val::Boolean(false))),
            1 => Ok(args[0].clone()),
            _ => {
                let mut subformulae = Vec::new();
                for subformula in args.into_iter() {
                    if let Expression::Or(subs) = subformula {
                        subformulae.extend(subs);
                    } else {
                        subformulae.push(subformula);
                    }
                }
                Ok(Expression::Or(subformulae))
            }
        }
    }

    /// Creates the component of an expression.
    ///
    /// Optimizes automatically the component of a tuple.
    pub fn component(self, index: usize) -> Self {
        if let Expression::Tuple(args) = self {
            args[index].clone()
        } else {
            Expression::Component(index, Box::new(self))
        }
    }
}

impl<V> std::ops::Not for Expression<V>
where
    V: Clone,
{
    type Output = Result<Self, TypeError>;

    fn not(self) -> Self::Output {
        if let Type::Boolean = self.r#type()? {
            if let Expression::Not(sub) = self {
                Ok(*sub)
            } else {
                Ok(Expression::Not(Box::new(self)))
            }
        } else {
            Err(TypeError::TypeMismatch)
        }
    }
}

impl<V> std::ops::Neg for Expression<V>
where
    V: Clone,
{
    type Output = Self;

    fn neg(self) -> Self::Output {
        if let Expression::Opposite(sub) = self {
            *sub
        } else {
            Expression::Opposite(Box::new(self))
        }
    }
}

impl<V> std::ops::Add for Expression<V>
where
    V: Clone,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut subformulae = Vec::new();
        if let Expression::Sum(subs) = self {
            subformulae.extend(subs);
        } else {
            subformulae.push(self);
        }
        if let Expression::Sum(subs) = rhs {
            subformulae.extend(subs);
        } else {
            subformulae.push(rhs);
        }
        Expression::Sum(subformulae)
    }
}

impl<V> std::ops::Mul for Expression<V>
where
    V: Clone,
{
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut subformulae = Vec::new();
        if let Expression::Mult(subs) = self {
            subformulae.extend(subs);
        } else {
            subformulae.push(self);
        }
        if let Expression::Mult(subs) = rhs {
            subformulae.extend(subs);
        } else {
            subformulae.push(rhs);
        }
        Expression::Mult(subformulae)
    }
}

impl<V> std::iter::Sum for Expression<V>
where
    V: Clone,
{
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, e| acc + e).unwrap_or(Self::from(0))
    }
}

impl<V> std::iter::Product for Expression<V>
where
    V: Clone,
{
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, e| acc * e).unwrap_or(Self::from(1))
    }
}

impl<V> From<bool> for Expression<V>
where
    V: Clone,
{
    fn from(value: bool) -> Self {
        Expression::Const(Val::Boolean(value))
    }
}

impl<V> From<Integer> for Expression<V>
where
    V: Clone,
{
    fn from(value: Integer) -> Self {
        Expression::Const(Val::Integer(value))
    }
}

impl<V> From<Float> for Expression<V>
where
    V: Clone,
{
    fn from(value: Float) -> Self {
        Expression::Const(Val::Float(value))
    }
}

type DynFnExpr<V, R> = dyn Fn(&dyn Fn(V) -> Val, &mut R) -> Val + Send + Sync;

pub(crate) struct FnExpression<V, R: Rng>(Box<DynFnExpr<V, R>>);

impl<C, R: Rng> std::fmt::Debug for FnExpression<C, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Expression")
    }
}

impl<V, R: Rng> FnExpression<V, R> {
    #[inline(always)]
    pub fn eval(&self, vars: &dyn Fn(V) -> Val, rng: &mut R) -> Val {
        self.0(vars, rng)
    }
}

impl<V: Clone + Send + Sync + 'static, R: Rng + 'static> From<Expression<V>>
    for FnExpression<V, R>
{
    fn from(value: Expression<V>) -> Self {
        FnExpression(match value {
            Expression::Const(val) => Box::new(move |_, _| val.clone()),
            Expression::Var(var, t) => Box::new(move |vars, _| {
                // vars(var.clone())
                let val = vars(var.clone());
                if t == val.r#type() {
                    val
                } else {
                    panic!("value and variable type mismatch");
                }
            }),
            Expression::Tuple(exprs) => {
                let exprs: Vec<FnExpression<_, _>> =
                    exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars, rng| {
                    Val::Tuple(
                        exprs
                            .iter()
                            .map(|expr| expr.eval(vars, rng))
                            .collect::<Vec<_>>(),
                    )
                })
            }
            Expression::Component(index, expr) => {
                let expr = Self::from(*expr);
                Box::new(move |vars, rng| {
                    if let Val::Tuple(vals) = expr.eval(vars, rng) {
                        vals[index].clone()
                    } else {
                        panic!("index out of bounds");
                    }
                })
            }
            Expression::And(exprs) => {
                let exprs: Vec<FnExpression<_, _>> = exprs.into_iter().map(Self::from).collect();
                Box::new(move |vars, rng| {
                    Val::Boolean(exprs.iter().all(|expr| {
                        if let Val::Boolean(b) = expr.eval(vars, rng) {
                            b
                        } else {
                            panic!("type mismatch");
                        }
                    }))
                })
            }
            Expression::Or(exprs) => {
                let exprs: Vec<FnExpression<_, _>> = exprs.into_iter().map(Self::from).collect();
                Box::new(move |vars, rng| {
                    Val::Boolean(exprs.iter().any(|expr| {
                        if let Val::Boolean(b) = expr.eval(vars, rng) {
                            b
                        } else {
                            panic!("type mismatch");
                        }
                    }))
                })
            }
            Expression::Implies(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars, rng| {
                    if let (Val::Boolean(lhs), Val::Boolean(rhs)) =
                        (lhs.eval(vars, rng), rhs.eval(vars, rng))
                    {
                        Val::Boolean(rhs || !lhs)
                    } else {
                        panic!("type mismatch");
                    }
                })
            }
            Expression::Not(expr) => {
                let expr = FnExpression::from(*expr);
                Box::new(move |vars, rng| {
                    if let Val::Boolean(b) = expr.eval(vars, rng) {
                        Val::Boolean(!b)
                    } else {
                        panic!("type mismatch");
                    }
                })
            }
            Expression::Opposite(expr) => {
                let expr = FnExpression::from(*expr);
                Box::new(move |vars, rng| match expr.eval(vars, rng) {
                    Val::Integer(i) => Val::Integer(-i),
                    Val::Float(f) => Val::Float(-f),
                    _ => panic!("type mismatch"),
                })
            }
            Expression::Sum(exprs) => {
                let exprs: Vec<FnExpression<_, _>> = exprs.into_iter().map(Self::from).collect();
                Box::new(move |vars, rng| {
                    exprs.iter().fold(Val::Integer(0), |val, expr| match val {
                        Val::Integer(acc) => match expr.eval(vars, rng) {
                            Val::Integer(i) => Val::Integer(acc + i),
                            Val::Float(f) => Val::Float(f64::from(acc) + f),
                            _ => panic!("type mismatch"),
                        },
                        Val::Float(acc) => match expr.eval(vars, rng) {
                            Val::Integer(i) => Val::Float(acc + f64::from(i)),
                            Val::Float(f) => Val::Float(acc + f),
                            _ => panic!("type mismatch"),
                        },
                        _ => panic!("type mismatch"),
                    })
                })
            }
            Expression::Mult(exprs) => {
                let exprs: Vec<FnExpression<_, _>> = exprs.into_iter().map(Self::from).collect();
                Box::new(move |vars, rng| {
                    exprs.iter().fold(Val::Integer(0), |val, expr| match val {
                        Val::Integer(acc) => match expr.eval(vars, rng) {
                            Val::Integer(i) => Val::Integer(acc * i),
                            Val::Float(f) => Val::Float(f64::from(acc) * f),
                            _ => panic!("type mismatch"),
                        },
                        Val::Float(acc) => match expr.eval(vars, rng) {
                            Val::Integer(i) => Val::Float(acc * f64::from(i)),
                            Val::Float(f) => Val::Float(acc * f),
                            _ => panic!("type mismatch"),
                        },
                        _ => panic!("type mismatch"),
                    })
                })
            }
            Expression::Equal(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(
                    move |vars, rng| match (lhs.eval(vars, rng), rhs.eval(vars, rng)) {
                        (Val::Integer(lhs), Val::Integer(rhs)) => Val::Boolean(lhs == rhs),
                        (Val::Boolean(lhs), Val::Boolean(rhs)) => Val::Boolean(lhs == rhs),
                        _ => panic!("type mismatch"),
                    },
                )
            }
            Expression::Greater(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars, rng| match lhs.eval(vars, rng) {
                    Val::Integer(lhs) => match rhs.eval(vars, rng) {
                        Val::Integer(rhs) => Val::Boolean(lhs > rhs),
                        Val::Float(rhs) => Val::Boolean(f64::from(lhs) > rhs),
                        _ => panic!("type mismatch"),
                    },
                    Val::Float(lhs) => match rhs.eval(vars, rng) {
                        Val::Integer(rhs) => Val::Boolean(lhs > f64::from(rhs)),
                        Val::Float(rhs) => Val::Boolean(lhs > rhs),
                        _ => panic!("type mismatch"),
                    },
                    _ => panic!("type mismatch"),
                })
            }
            Expression::GreaterEq(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars, rng| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) =
                        (lhs.eval(vars, rng), rhs.eval(vars, rng))
                    {
                        Val::Boolean(lhs >= rhs)
                    } else {
                        panic!("type mismatch");
                    }
                })
            }
            Expression::Less(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars, rng| match lhs.eval(vars, rng) {
                    Val::Integer(lhs) => match rhs.eval(vars, rng) {
                        Val::Integer(rhs) => Val::Boolean(lhs < rhs),
                        Val::Float(rhs) => Val::Boolean(f64::from(lhs) < rhs),
                        _ => panic!("type mismatch"),
                    },
                    Val::Float(lhs) => match rhs.eval(vars, rng) {
                        Val::Integer(rhs) => Val::Boolean(lhs < f64::from(rhs)),
                        Val::Float(rhs) => Val::Boolean(lhs < rhs),
                        _ => panic!("type mismatch"),
                    },
                    _ => panic!("type mismatch"),
                })
            }
            Expression::LessEq(exprs) => {
                let (source_lhs, source_rhs) = *exprs;
                let lhs = FnExpression::from(source_lhs);
                let rhs = FnExpression::from(source_rhs);
                Box::new(move |vars, rng| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) =
                        (lhs.eval(vars, rng), rhs.eval(vars, rng))
                    {
                        Val::Boolean(lhs <= rhs)
                    } else {
                        panic!("type mismatch");
                    }
                })
            }
            Expression::Append(exprs) => {
                let (list, element) = *exprs;
                let list = FnExpression::from(list);
                let element = FnExpression::from(element);
                Box::new(move |vars, rng| {
                    if let Val::List(t, mut l) = list.eval(vars, rng) {
                        let element = element.eval(vars, rng);
                        if element.r#type() == t {
                            l.push(element);
                            Val::List(t, l)
                        } else {
                            panic!("type mismatch");
                        }
                    } else {
                        panic!("type mismatch");
                    }
                })
            }
            Expression::Truncate(list) => {
                let list = FnExpression::from(*list);
                Box::new(move |vars, rng| {
                    if let Val::List(t, mut l) = list.eval(vars, rng) {
                        if !l.is_empty() {
                            let _ = l.pop();
                            Val::List(t, l)
                        } else {
                            panic!("type mismatch");
                        }
                    } else {
                        panic!("type mismatch");
                    }
                })
            }
            Expression::Len(list) => {
                let list = FnExpression::from(*list);
                Box::new(move |vars, rng| {
                    if let Val::List(_t, l) = list.eval(vars, rng) {
                        Val::Integer(l.len() as Integer)
                    } else {
                        panic!("type mismatch");
                    }
                })
            }
            Expression::Mod(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars, rng| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) =
                        (lhs.eval(vars, rng), rhs.eval(vars, rng))
                    {
                        Val::Integer(lhs % rhs)
                    } else {
                        panic!("type mismatch");
                    }
                })
            }
            Expression::RandBool(p) => Box::new(move |_, rng| Val::Boolean(rng.random_bool(p))),
            Expression::RandInt(l, u) => {
                Box::new(move |_, rng| Val::Integer(rng.random_range(l..u)))
            }
        })
    }
}
