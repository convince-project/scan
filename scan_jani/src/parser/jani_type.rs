use super::Expression;
use serde::Deserialize;

/// Types.
/// We cover only the most basic types at the moment.
/// In the remainder of the specification, all requirements like "y must be of type x" are to be interpreted
/// as "type x must be assignable from y's type".
#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum BasicType {
    /// assignable from bool
    Bool,
    /// numeric; assignable from int and bounded int
    Int,
    /// numeric; assignable from all numeric types
    Real,
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum BoundedTypeKind {
    Bounded,
}

/// numeric if base is numeric; lower-bound or upper-bound or both must be present;
/// assignable from those types that base is assignable from#[derive(Deserialize)]
#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct BoundedType {
    kind: BoundedTypeKind,
    base: BasicType,
    /// smallest value allowed by the type; constant expression of the base type
    #[serde(default)]
    lower_bound: Option<Expression>,
    /// largest value allowed by the type; constant expression of the base type
    #[serde(default)]
    upper_bound: Option<Expression>,
}

#[derive(Deserialize)]
#[serde(untagged, rename_all = "kebab-case")]
pub(crate) enum Type {
    Basic(BasicType),
    Bounded(BoundedType),
    /// numeric; only allowed for TA, PTA, STA, HA, PHA and SHA; assignable from int and bounded int
    Clock(u32),
    /// numeric; continuous variable that changes over time as allowed by the current location's
    /// invariant; only allowed for HA, PHA and SHA; assignable from all numeric types
    Continuous(f64),
}
