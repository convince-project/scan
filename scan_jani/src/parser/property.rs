use super::{Expression, Identifier};
use serde::Deserialize;

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Property {
    /// the property's name, unique among all the properties of the model
    pub(crate) name: Identifier,
    /// the state-set formula
    pub(crate) expression: Expression,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}

// #[derive(Deserialize)]
// #[serde(rename_all = "kebab-case")]
// pub(crate) enum PropertyExpression {
//     /// constant value
//     ConstantValue(ConstantValue),
//     /// constant or variable reference; has the type of the constant or variable;
//     /// if this type is a bounded type with base type t, then it has type t instead;
//     /// constant expression iff it is a constant reference
//     Identifier(Identifier),
//     /// if-then-else: computes if if then then else else
//     IfThenElse {
//         /// the result type is the type of then if that is assignable from the type of else,
//         /// or the type of else if that is assignable from the type of then
//         op: IteOp,
//         /// the condition; type bool
//         r#if: Box<PropertyExpression>,
//         /// the consequence
//         r#then: Box<PropertyExpression>,
//         /// the alternative
//         r#else: Box<PropertyExpression>,
//     },
//     /// disjunction / conjunction: computes left ∨ right / left ∧ right
//     Bool {
//         /// result type is bool
//         op: BoolOp,
//         /// the left operand; type bool
//         left: Box<PropertyExpression>,
//         /// the right operand; type bool
//         right: Box<PropertyExpression>,
//     },
//     /// negation: computes ¬exp
//     Neg {
//         /// result type is bool
//         op: NegOp,
//         /// the single operand; type bool
//         exp: Box<PropertyExpression>,
//     },
//     /// equality comparison: computes left = right / left ≠ right
//     EqComp {
//         /// result type is bool; left and right must be assignable to some common type
//         op: EqCompOp,
//         /// the left operand
//         left: Box<PropertyExpression>,
//         /// the right operand
//         right: Box<PropertyExpression>,
//     },
//     /// numeric comparison: computes left < right / left ≤ right
//     NumComp {
//         /// result type is bool
//         op: NumCompOp,
//         /// the left operand; numeric type
//         left: Box<PropertyExpression>,
//         /// the right operand; numeric type
//         right: Box<PropertyExpression>,
//     },
//     /// addition / subtraction / multiplication / modulo:
//     IntOp {
//         /// result type is int (if left and right are both assignable to int) or real
//         op: IntOp,
//         /// the left operand; numeric type (must be int if op is "%")
//         left: Box<PropertyExpression>,
//         /// the right operand; numeric type (must be int if op is "%")
//         right: Box<PropertyExpression>,
//     },
//     /// division / exponentiation / logarithm:
//     RealOp {
//         /// result type is real (division is real division, no truncation for integers)
//         op: RealOp,
//         /// the left operand; numeric type
//         left: Box<PropertyExpression>,
//         /// the right operand; numeric type
//         right: Box<PropertyExpression>,
//     },
//     /// floor / ceiling: computes ⌊exp⌋ / ⌈exp⌉
//     Real2IntOp {
//         /// result type is int
//         op: Real2IntOp,
//         /// the single operand; numeric type
//         exp: Box<PropertyExpression>,
//     },
// }
