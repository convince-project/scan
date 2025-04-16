use scan_core::Integer;
use serde::Deserialize;

use super::Identifier;

/// an expression is constant if all subexpressions are constant, unless noted otherwise
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged, rename_all = "kebab-case")]
pub(crate) enum Expression {
    /// constant value
    ConstantValue(ConstantValue),
    /// constant or variable reference; has the type of the constant or variable;
    /// if this type is a bounded type with base type t, then it has type t instead;
    /// constant expression iff it is a constant reference
    Identifier(Identifier),
    /// if-then-else: computes if if then then else else
    IfThenElse {
        /// the result type is the type of then if that is assignable from the type of else,
        /// or the type of else if that is assignable from the type of then
        op: IteOp,
        /// the condition; type bool
        r#if: Box<Expression>,
        /// the consequence
        r#then: Box<Expression>,
        /// the alternative
        r#else: Box<Expression>,
    },
    /// disjunction / conjunction: computes left ∨ right / left ∧ right
    Bool {
        /// result type is bool
        op: BoolOp,
        /// the left operand; type bool
        left: Box<Expression>,
        /// the right operand; type bool
        right: Box<Expression>,
    },
    /// negation: computes ¬exp
    Neg {
        /// result type is bool
        op: NegOp,
        /// the single operand; type bool
        exp: Box<Expression>,
    },
    /// equality comparison: computes left = right / left ≠ right
    EqComp {
        /// result type is bool; left and right must be assignable to some common type
        op: EqCompOp,
        /// the left operand
        left: Box<Expression>,
        /// the right operand
        right: Box<Expression>,
    },
    /// numeric comparison: computes left < right / left ≤ right
    NumComp {
        /// result type is bool
        op: NumCompOp,
        /// the left operand; numeric type
        left: Box<Expression>,
        /// the right operand; numeric type
        right: Box<Expression>,
    },
    /// addition / subtraction / multiplication / modulo:
    IntOp {
        /// result type is int (if left and right are both assignable to int) or real
        op: IntOp,
        /// the left operand; numeric type (must be int if op is "%")
        left: Box<Expression>,
        /// the right operand; numeric type (must be int if op is "%")
        right: Box<Expression>,
    },
    /// division / exponentiation / logarithm:
    RealOp {
        /// result type is real (division is real division, no truncation for integers)
        op: RealOp,
        /// the left operand; numeric type
        left: Box<Expression>,
        /// the right operand; numeric type
        right: Box<Expression>,
    },
    /// floor / ceiling: computes ⌊exp⌋ / ⌈exp⌉
    Real2IntOp {
        /// result type is int
        op: Real2IntOp,
        /// the single operand; numeric type
        exp: Box<Expression>,
    },
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum Constant {
    /// Euler's number (the base of the natural logarithm); type real
    #[serde(rename = "e")]
    Euler,
    /// π (the ratio of a circle's circumference to its diameter); type real
    #[serde(rename = "π")]
    Pi,
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(untagged, rename_all = "kebab-case")]
pub(crate) enum ConstantValue {
    /// Boolean value; has type bool
    Boolean(bool),
    /// mathematical constants that cannot be expressed using numeric values and basic jani-model expressions
    Constant(Constant),
    /// numeric value; has type int if it is an integer and type real otherwise
    #[serde(rename = "number")]
    NumberInt(Integer),
    #[serde(rename = "number")]
    NumberReal(f64),
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum IteOp {
    Ite,
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum BoolOp {
    #[serde(rename = "∧")]
    And,
    #[serde(rename = "∨")]
    Or,
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum NegOp {
    #[serde(rename = "¬")]
    Neg,
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum EqCompOp {
    #[serde(rename = "=")]
    Eq,
    #[serde(rename = "≠")]
    Neq,
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum NumCompOp {
    #[serde(rename = "<")]
    Less,
    #[serde(rename = "≤")]
    Leq,
    #[serde(rename = ">")]
    Greater,
    #[serde(rename = "≥")]
    Geq,
}

/// computes left + right / left - right / left * right / left modulo right
#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum IntOp {
    #[serde(rename = "+")]
    Plus,
    #[serde(rename = "-")]
    Minus,
    #[serde(rename = "*")]
    Mult,
    #[serde(rename = "%")]
    IntDiv,
}

/// computes left + right / left - right / left * right / left modulo right
#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum RealOp {
    #[serde(rename = "/")]
    Div,
    Pow,
    Log,
}

/// floor / ceiling: computes ⌊exp⌋ / ⌈exp⌉
#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum Real2IntOp {
    Floor,
    Ceil,
}
