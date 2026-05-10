use super::Expression;
use serde::Deserialize;

/// Types.
/// We cover only the most basic types at the moment.
/// In the remainder of the specification, all requirements like "y must be of type x" are to be interpreted
/// as "type x must be assignable from y's type".
#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum BasicType {
    /// assignable from bool
    Bool,
    /// numeric; assignable from int and bounded int
    Int,
    /// numeric; assignable from all numeric types
    Real,
}

impl Into<scan_core::Type> for BasicType {
    fn into(self) -> scan_core::Type {
        match self {
            BasicType::Bool => scan_core::Type::Boolean,
            BasicType::Int => scan_core::Type::Integer,
            BasicType::Real => scan_core::Type::Float,
        }
    }
}

#[derive(Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum BoundedTypeKind {
    #[default]
    Bounded,
}

/// numeric if base is numeric; lower-bound or upper-bound or both must be present;
/// assignable from those types that base is assignable from#[derive(Deserialize)]
#[derive(Deserialize)]
#[allow(dead_code)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub(crate) struct BoundedType {
    #[serde(default)]
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
#[allow(dead_code)]
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

impl TryInto<scan_core::Type> for &Type {
    type Error = ();

    fn try_into(self) -> Result<scan_core::Type, Self::Error> {
        match self {
            Type::Basic(basic_type) => Ok((*basic_type).into()),
            Type::Bounded(bounded_type) => Ok(bounded_type.base.into()),
            Type::Clock(_) => todo!(),
            Type::Continuous(_) => todo!(),
        }
    }
}
