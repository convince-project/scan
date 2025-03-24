use super::{Expression, Identifier, Type};
use serde::Deserialize;

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct VariableDeclaration {
    /// the variable's name, unique among all constants and global variables
    /// as well as among local variables if the variable is declared within an automaton
    name: Identifier,
    /// the variable's type; must not be or contain "clock" or "continuous" if transient is true
    r#type: Type,
    /// transient variable if present and true; a transient variable behaves as follows:
    /// (a) when in a state, its value is that of the expression specified in
    ///     "transient-values" for the locations corresponding to that state, or its
    ///     initial value if no expression is specified in any of the locations
    ///     (and if multiple expressions are specified, that is a modelling error);
    /// (b) when taking a transition, its value is set to its initial value, then all
    ///     assignments of the edges corresponding to the transition are executed.
    #[serde(default)]
    transient: bool,
    /// if omitted: any value allowed by type (possibly restricted by the restrict-initial
    /// attributes of the model or an automaton); must be present if transient is present and true
    initial_value: Option<Expression>,
    /// an optional comment
    #[serde(skip)]
    comment: Option<String>,
}
