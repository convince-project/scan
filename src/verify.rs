use std::collections::HashMap;

use anyhow::anyhow;
use clap::Parser;
use scan_core::{Oracle, Scan, Time, TransitionSystem};

use super::report::Report;

const NO_PROPS_ERR: &str = "missing properties to verify.
Pass a (space-separated) list of one or more properties specified in the model to be verified.
Alternatively, use the flag --all to verify all properties at once.\n
EXAMPLE: scan PATH/TO/MODEL verify PROPERTY ANOTHER_PROPERTY
EXAMPLE: scan PATH/TO/MODEL verify --all";

const ALL_PROPS_ERR: &str =
    "the --all flag is incompatible with individually-specified properties.\n
EXAMPLE: scan PATH/TO/MODEL verify PROPERTY ANOTHER_PROPERTY
EXAMPLE: scan PATH/TO/MODEL verify --all";

const BAD_CONFIDENCE: &str =
    "confidence has to be a value strictly greater than 0 and strictly less than 1.\n
EXAMPLE: scan PATH/TO/MODEL verify --confidence 0.98 PROPERTY";

const BAD_PRECISION: &str =
    "precision has to be a value strictly greater than 0 and strictly less than 1.\n
EXAMPLE: scan PATH/TO/MODEL verify --precision 0.005 PROPERTY";

/// Verify the model
#[derive(Debug, Clone, Parser)]
#[deny(missing_docs)]
pub(crate) struct VerifyArgs {
    /// Space-separated list of properties to verify.
    pub(crate) properties: Vec<String>,
    /// Verify all properties found in the model specification.
    /// It is equivalent to listing all of the properties.
    #[arg(short, long)]
    pub(crate) all: bool,
    /// Confidence.
    /// It has to be a value between 0 and 1 (bounds excluded).
    #[arg(short, long, default_value_t = 0.95)]
    pub(crate) confidence: f64,
    /// Precision or half-width parameter.
    /// It has to be a value between 0 and 1 (bounds excluded).
    #[arg(short, long, default_value_t = 0.01)]
    pub(crate) precision: f64,
    /// Max duration of execution (in model-time),
    /// to prevent infinite executions.
    #[arg(short, long, default_value_t = 10000)]
    pub(crate) duration: Time,
    /// Run the verification on a single thread.
    ///
    /// By default, SCAN uses multi-threading.
    /// Use this flag to run verification on a single thread.
    ///
    /// WARNING: only the verification itself is run single-threaded.
    /// Other related functionalities, including model parsing and building,
    /// updating progress bars, etc.,
    /// might still be running multiple threads.
    #[arg(long)]
    pub(crate) single_thread: bool,
}

impl VerifyArgs {
    pub(crate) fn validate(&self) -> anyhow::Result<()> {
        if self.properties.is_empty() && !self.all {
            Err(anyhow!(NO_PROPS_ERR))
        } else if !self.properties.is_empty() && self.all {
            Err(anyhow!(ALL_PROPS_ERR))
        } else if 0f64 >= self.confidence || self.confidence >= 1f64 {
            Err(anyhow!(BAD_CONFIDENCE))
        } else if 0f64 >= self.precision || self.precision >= 1f64 {
            Err(anyhow!(BAD_PRECISION))
        } else {
            Ok(())
        }
    }

    pub(crate) fn verify<E, Ts, O>(
        &self,
        model: String,
        scan: Scan<E, Ts, O>,
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
        let property_failures = self
            .properties
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
