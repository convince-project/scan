use std::io::Write;

use crate::{
    Time, Val,
    channel_system::{Action, Event},
};

/// Trait that handles streaming of traces,
/// e.g., to print them to file.
pub trait Tracer<W: Write> {
    /// The extension to use for files of traces produced by the [`Tracer`].
    const EXTENSION: &str;

    /// Underlying model data to be traced.
    type ModelData;

    /// Creates and initializes the Tracer
    fn init(writer: W, data: &Self::ModelData) -> Self;

    /// Stream a new state of the trace.
    fn trace(
        &mut self,
        data: &Self::ModelData,
        action: Action,
        event: &Event,
        time: Time,
        ports: &[Vec<Val>],
    );
}
