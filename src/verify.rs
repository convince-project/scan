use std::collections::HashMap;

use clap::Parser;
use scan_core::{Oracle, Scan, Time, TransitionSystem};

use crate::report::Report;

/// Verify the model
#[derive(Debug, Clone, Parser)]
pub(crate) struct VerifyArgs {
    /// Confidence
    #[arg(short, long, default_value_t = 0.95)]
    pub(crate) confidence: f64,
    /// Precision or half-width parameter
    #[arg(short, long, default_value_t = 0.01)]
    pub(crate) precision: f64,
    /// Max duration of execution (in model-time)
    #[arg(short, long, default_value_t = 10000)]
    pub(crate) duration: Time,
    /// Run the verification on a single thread
    #[arg(long, default_value_t = false)]
    pub(crate) single_thread: bool,
}

impl VerifyArgs {
    pub(crate) fn verify<E, Ts, O>(
        &self,
        model: String,
        scan: Scan<E, Ts, O>,
        properties: &[String],
    ) -> anyhow::Result<Report>
    where
        Ts: TransitionSystem<E> + 'static,
        E: Clone + Send + Sync + 'static,
        O: Oracle + 'static,
    {
        scan.adaptive(
            self.confidence,
            self.precision,
            self.duration,
            self.single_thread,
        )?;
        let successes = scan.successes();
        let failures = scan.failures();
        let runs = successes + failures;
        let rate = successes as f64 / runs as f64;
        let property_failures = properties
            .iter()
            .cloned()
            .zip(scan.violations().into_iter().chain([0].into_iter().cycle()))
            .collect::<HashMap<String, u32>>();
        let report = Report {
            model,
            precision: self.precision,
            confidence: self.confidence,
            duration: self.duration,
            rate,
            runs,
            successes,
            failures,
            property_failures,
        };
        Ok(report)
    }
}
