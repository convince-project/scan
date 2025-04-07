use super::{Expression, Identifier, Type};
use serde::Deserialize;

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct ConstantDeclaration {
    /// the constant's name, unique among all constants and variables
    name: Identifier,
    /// the constant's type; bounded types must not refer to this constant or
    /// constants declared after this one in the corresponding array
    r#type: Type,
    /// the constant's value, of type type; constant expression that must not refer to this
    /// constant or constants declared after this one in the corresponding array;
    /// if omitted, the constant is a model parameter
    #[serde(default)]
    value: Option<Expression>,
    /// an optional comment
    #[serde(skip)]
    comment: String,
}
