use std::fs::File;

use flate2::write::GzEncoder;

use crate::{
    Time, Val,
    channel_system::{Action, Channel, Event},
};

/// A writer for traces
pub type TraceWriter = GzEncoder<File>;

/// Trait that handles streaming of traces,
/// e.g., to print them to file.
pub trait Tracer<'a> {
    /// The extension to use for files of traces produced by the [`Tracer`].
    const EXTENSION: &str;

    /// Underlying model data to be traced.
    type ModelData;

    /// Creates and initializes the Tracer
    fn init(writer: TraceWriter, data: &'a Self::ModelData) -> Self;

    /// Stream a new state of the trace.
    fn trace<'b>(
        &mut self,
        _action: Action,
        _event: Event<'_>,
        _time: Time,
        _ports: &'b impl Fn(Channel) -> Option<&'b [Val]>,
    ) {
        // do nothing
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DummyTracer;

impl Tracer<'_> for DummyTracer {
    const EXTENSION: &str = "";

    type ModelData = ();

    fn init(_writer: TraceWriter, _data: &Self::ModelData) -> Self {
        Self
    }
}
