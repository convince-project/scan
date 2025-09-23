use crate::Time;

/// Implementators are induced by a temporal property.
/// They can update their internal state when fed a new state of a trace,
/// and establish whether their corresponding property holds on such trace.
pub trait Oracle: Clone + Sync {
    /// Update the internal state of the [`Oracle`] with the latest state of a temporal trace.
    fn update(&mut self, state: &[bool], time: Time);

    /// Returns the values of the "assume" properties,
    /// if already determined.
    fn output_assumes(&self) -> impl Iterator<Item = Option<bool>>;

    /// Returns the values of the "guarantee" properties,
    /// if already determined.
    fn output_guarantees(&self) -> impl Iterator<Item = Option<bool>>;

    /// As the trace ends, the values of the "assume" properties is determined to be either true or false.
    fn final_output_assumes(&self) -> impl Iterator<Item = bool>;

    /// As the trace ends, the values of the "guarantee" properties is determined to be either true or false.
    fn final_output_guarantees(&self) -> impl Iterator<Item = bool>;
}

/// A type that can generate instances of an [`Oracle`].
pub trait OracleGenerator {
    /// The type of [`Oracle`] to be generated.
    type O<'a>: Oracle
    where
        Self: 'a;

    /// Generate a new instance of the [`Oracle`].
    fn generate<'a>(&'a self) -> Self::O<'a>;
}
