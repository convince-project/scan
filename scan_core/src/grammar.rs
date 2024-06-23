//! The language used by PGs and CSs.
//!
//! The type [`Expression<V>`] encodes the used language,
//! where `V` is the type parameter of variables.
//! The language features base types and product types,
//! Boolean logic and basic arithmetic expressions.

/// They types supported by the language internally used by PGs and CSs.
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
/// [`Expression<V>`] encodes the language in which
/// `V` is the type parameter for the type of variables.
///
/// Note that not all expressions that can be formed are well-typed.
#[derive(Debug, Clone)]
pub enum Expression<V>
where
    V: Clone + Copy + PartialEq + Eq,
{
    /// Constant boolean value.
    Boolean(bool),
    /// Constant integer value.
    Integer(Integer),
    // General expressions
    /// A variable.
    Var(V),
    /// A tuple of expressions.
    Tuple(Vec<Expression<V>>),
    /// The component of a tuple.
    Component(usize, Box<Expression<V>>),
    // Logical operators
    /// n-uary logical conjunction.
    And(Vec<Expression<V>>),
    /// n-uary logical disjunction.
    Or(Vec<Expression<V>>),
    /// Logical implication.
    Implies(Box<(Expression<V>, Expression<V>)>),
    /// Logical negation.
    Not(Box<Expression<V>>),
    // Arithmetic operators
    /// Opposite of a numerical expression.
    Opposite(Box<Expression<V>>),
    /// Arithmetic n-ary sum.
    Sum(Vec<Expression<V>>),
    /// Arithmetic n-ary multiplication.
    Mult(Vec<Expression<V>>),
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

pub(crate) struct FnExpression(Box<dyn Fn(&[Val]) -> Val>);

impl std::fmt::Debug for FnExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Expression")
    }
}

impl FnExpression {
    pub(crate) fn eval(&self, vals: &[Val]) -> Val {
        self.0(vals)
    }
}

impl<V> From<Expression<V>> for FnExpression
where
    V: Clone + Copy + PartialEq + Eq + Into<usize> + 'static,
{
    fn from(value: Expression<V>) -> Self {
        FnExpression(match value.clone() {
            Expression::Boolean(b) => Box::new(move |_: &[Val]| Val::Boolean(b)),
            Expression::Integer(i) => Box::new(move |_: &[Val]| Val::Integer(i)),
            Expression::Var(var) => {
                Box::new(move |vars: &[Val]| vars[Into::<usize>::into(var)].clone())
            }
            Expression::Tuple(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Tuple(exprs.iter().map(|expr| expr.eval(vars)).collect::<Vec<_>>())
                })
            }
            Expression::Component(index, expr) => {
                let expr = Into::<FnExpression>::into(*expr).0;
                Box::new(move |vars: &[Val]| {
                    if let Val::Tuple(vals) = expr(vars) {
                        vals[index].clone()
                    } else {
                        panic!();
                    }
                })
            }
            Expression::And(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Boolean(exprs.iter().all(|expr| {
                        if let Val::Boolean(b) = expr.eval(vars) {
                            b
                        } else {
                            panic!()
                        }
                    }))
                })
            }
            Expression::Or(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Boolean(exprs.iter().any(|expr| {
                        if let Val::Boolean(b) = expr.eval(vars) {
                            b
                        } else {
                            panic!()
                        }
                    }))
                })
            }
            Expression::Implies(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Boolean(lhs), Val::Boolean(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(rhs || !lhs)
                    } else {
                        panic!()
                    }
                })
            }
            Expression::Not(expr) => {
                let expr = FnExpression::from(*expr);
                Box::new(move |vars: &[Val]| {
                    if let Val::Boolean(b) = expr.eval(vars) {
                        Val::Boolean(!b)
                    } else {
                        panic!()
                    }
                })
            }
            Expression::Opposite(expr) => {
                let expr = FnExpression::from(*expr);
                Box::new(move |vars: &[Val]| {
                    if let Val::Integer(i) = expr.eval(vars) {
                        Val::Integer(-i)
                    } else {
                        panic!()
                    }
                })
            }
            Expression::Sum(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Integer(
                        exprs
                            .iter()
                            .map(|expr| {
                                if let Val::Integer(i) = expr.eval(vars) {
                                    i
                                } else {
                                    panic!()
                                }
                            })
                            .sum(),
                    )
                })
            }
            Expression::Mult(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Integer(
                        exprs
                            .iter()
                            .map(|expr| {
                                if let Val::Integer(i) = expr.eval(vars) {
                                    i
                                } else {
                                    panic!()
                                }
                            })
                            .product(),
                    )
                })
            }
            Expression::Equal(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs == rhs)
                    } else {
                        panic!()
                    }
                })
            }
            Expression::Greater(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs > rhs)
                    } else {
                        panic!()
                    }
                })
            }
            Expression::GreaterEq(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs >= rhs)
                    } else {
                        panic!()
                    }
                })
            }
            Expression::Less(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs < rhs)
                    } else {
                        panic!()
                    }
                })
            }
            Expression::LessEq(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs <= rhs)
                    } else {
                        panic!()
                    }
                })
            }
        })
    }
}
