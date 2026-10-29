//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! This crate is part of the [SCAN statistical model checker](https://convince-project.github.io/scan/)
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod channel_system;
mod grammar;
mod oracle;
pub mod program_graph;
mod smc;
mod time;
mod tracer;
mod transition_system;

pub use grammar::*;
use log::{info, trace};
pub use oracle::*;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
pub use smc::*;
use std::{
    fs::{File, create_dir, create_dir_all, rename},
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU32, Ordering},
    },
    time::Instant,
};
use thiserror::Error;
pub use time::*;
pub use tracer::{TraceWriter, Tracer};
pub use transition_system::{Atom, TransitionSystem};

use crate::tracer::DummyTracer;

const TEMP: &str = ".temp";
const SUCCESSES: &str = "successes";
const FAILURES: &str = "failures";

/// Errors that can be returned by a `[Scan]` method
#[derive(Clone, Copy, Debug, Error)]
pub enum ScanError {
    /// Precision value is out-of-bounds
    #[error("out-of-bounds precision value: {0}")]
    OutOfBoundsPrecision(f64),
    /// Confidence value is out-of-bounds
    #[error("out-of-bounds confidence value: {0}")]
    OutOfBoundsConfidence(f64),
}

/// Scheduling strategy used to sample executions
#[derive(Debug, Default, Clone, Copy)]
#[deny(missing_docs)]
pub enum Scheduler {
    /// Uniform distribution over non-deterministic choices.
    #[default]
    Uniform,
    /// Schedulers sampled from uniform distribution over scheduler space.
    ///
    /// Lightweight sampling strategy from:
    ///
    /// __An efficient statistical model checker for nondeterminism and rare events__
    /// (Carlos E. Budde et al.),
    /// International Journal on Software Tools for Technology Transfer (2020),
    /// [https://doi.org/10.1007/s10009-020-00563-2].
    Sampling,
    /// Exhaustive exploration of scheduler space.
    ///
    /// Search is optimized via Partial Order Reduction.
    Exhaustive,
    /// Exploration of subspace of schedulers that enforce an order of priority between processes,
    ///
    /// Search is optimized via Partial Order Reduction.
    Priority,
}

/// Final report for a verification run.
#[derive(Debug, Clone)]
pub struct Report {
    /// Total executions run (even partial).
    pub runs: u32,
    /// Successful executions.
    pub successes: u32,
    /// Failed executions.
    pub failures: u32,
    /// Breakdown of violations by property.
    pub violations: Vec<u32>,
}

// The possible outcomes of a model execution:
// - If the run was not completed (because the execution violated an assume), result is `None`.
// - If the run succeeded, or failed by violating the guarantees, result is `Some(violations)`
//   where the `violations` carries which (if any) guarantees were violated.
type RunOutcome = Option<Vec<bool>>;

/// The main type to interface with the verification capabilities of SCAN.
/// [`Scan`] holds the model, properties and other data necessary to run the verification process.
/// The type of properties is abstracted through the [`Oracle`] trait,
/// to provide a unified interface.
#[derive(Debug, Clone)]
pub struct Scan<O> {
    model: TransitionSystem,
    oracle: O,
    running: Arc<AtomicBool>,
    successes: Arc<AtomicU32>,
    failures: Arc<AtomicU32>,
    violations: Vec<Arc<AtomicU32>>,
}

impl<O: Oracle> Scan<O> {
    /// Create new [`Scan`] object.
    pub fn new(tsd: TransitionSystem, oracle: O) -> Self {
        let guarantees = oracle.output_guarantees().count();
        let violations = Vec::from_iter((0..guarantees).map(|_| Arc::new(AtomicU32::new(0))));
        Scan {
            model: tsd,
            oracle,
            running: Arc::new(AtomicBool::new(false)),
            successes: Arc::new(AtomicU32::new(0)),
            failures: Arc::new(AtomicU32::new(0)),
            violations,
        }
    }

    fn reset(&self) {
        self.running.store(true, Ordering::Relaxed);
        self.successes.store(0, Ordering::Relaxed);
        self.failures.store(0, Ordering::Relaxed);
        self.violations
            .iter()
            .for_each(|v| v.store(0, Ordering::Relaxed));
    }

    /// Tells whether a verification task is currently running.
    #[inline]
    pub fn running(&self) -> bool {
        self.running.load(Ordering::Relaxed)
    }

    /// Returns the number of successful executions in the current verification run.
    #[inline]
    pub fn successes(&self) -> u32 {
        self.successes.load(Ordering::Relaxed)
    }

    /// Returns the number of failed executions in the current verification run.
    #[inline]
    pub fn failures(&self) -> u32 {
        self.failures.load(Ordering::Relaxed)
    }

    /// Returns a vector where each entry contains the number of violations of the associated property in the current verification run.
    #[inline]
    pub fn violations(&self) -> impl Iterator<Item = u32> {
        self.violations.iter().map(|v| v.load(Ordering::Relaxed))
    }
}

impl<O: Oracle + Clone> Scan<O> {
    fn verification(&self, confidence: f64, precision: f64, scheduler: Scheduler) {
        assert!(0f64 < confidence && confidence < 1f64);
        assert!(0f64 < precision && precision < 1f64);

        let oracle = self.oracle.clone();
        let running = Arc::clone(&self.running);
        let outcome = self
            .model
            .experiment::<O, DummyTracer>(oracle, None, running, scheduler);
        if let Some(guarantees) = outcome
            && self.running.load(Ordering::Relaxed)
        {
            assert_eq!(guarantees.len(), self.violations().count());
            let local_successes;
            let local_failures;
            if guarantees.iter().all(|b| *b) {
                local_successes = self.successes.fetch_add(1, Ordering::Relaxed) + 1;
                local_failures = self.failures.load(Ordering::Relaxed);
                // If all guarantees are satisfied, the execution is successful
                trace!("runs: {local_successes} successes");
            } else {
                local_successes = self.successes.load(Ordering::Relaxed);
                local_failures = self.failures.fetch_add(1, Ordering::Relaxed) + 1;
                guarantees
                    .into_iter()
                    .zip(self.violations.iter())
                    .filter(|(success, _)| !success)
                    .for_each(|(_, violations)| {
                        let _ = violations.fetch_add(1, Ordering::Relaxed);
                    });
                // If guarantee is violated, we have found a counter-example!
                trace!("runs: {local_failures} failures");
            }
            let runs = local_successes + local_failures;
            // Division by 0 leads to Inf/NaN, which is not less than any other float number
            // so this actually works as expected even for runs = 0.
            let avg = local_successes as f64 / runs as f64;
            if adaptive_bound(avg, confidence, precision) <= runs as f64 {
                info!("adaptive bound satisfied");
                self.running.store(false, Ordering::Relaxed);
            }
        }
    }

    /// Statistically verifies the provided [`TransitionSystem`] using adaptive bound and the given parameters.
    pub fn adaptive(
        &self,
        confidence: f64,
        precision: f64,
        scheduler: Scheduler,
    ) -> Result<Report, ScanError> {
        if !(0f64 < confidence && confidence < 1f64) {
            return Err(ScanError::OutOfBoundsConfidence(confidence));
        }
        if !(0f64 < precision && precision < 1f64) {
            return Err(ScanError::OutOfBoundsPrecision(precision));
        }

        self.reset();

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        let runs = (0..)
            .map(|_| self.verification(confidence, precision, scheduler))
            .take_while(|_| self.running.load(Ordering::Relaxed))
            .count() as u32;

        let elapsed = start_time.elapsed();
        info!("verification completed in {elapsed:0.2?}");
        Ok(Report {
            runs,
            successes: self.successes(),
            failures: self.failures(),
            violations: self.violations().collect(),
        })
    }

    /// Produces and saves the traces for the given number of runs,
    /// using the provided [`Tracer`].
    pub fn traces<'a, T>(
        &self,
        runs: usize,
        path: &Path,
        model_data: &'a T::ModelData,
        scheduler: Scheduler,
    ) -> std::io::Result<()>
    where
        T: Tracer<'a>,
    {
        info!("tracing starting");
        let start_time = Instant::now();
        self.reset();
        create_traces_dirs_tree(path)?;

        (0..runs).try_for_each(|idx| self.trace::<T>(path, model_data, scheduler, idx))?;

        let elapsed = start_time.elapsed();
        info!("tracing completed in {elapsed:0.2?}");
        Ok(())
    }

    fn trace<'a, T>(
        &self,
        path: &Path,
        model_data: &'a T::ModelData,
        scheduler: Scheduler,
        idx: usize,
    ) -> std::io::Result<()>
    where
        T: Tracer<'a>,
    {
        let filename = PathBuf::new()
            .with_file_name(format!("{idx:04}"))
            .with_extension(T::EXTENSION);
        let mut path = path.join(TEMP);
        path.push(&filename);
        path.add_extension("gz");
        let file = File::create_new(&path)?;
        let writer = flate2::GzBuilder::new()
            .filename(filename.to_str().expect("file name is utf8"))
            .comment("Scan-generated execution trace")
            .write(file, flate2::Compression::best());
        let tracer = Some(T::init(writer, model_data));
        let oracle = self.oracle.clone();
        let running = Arc::clone(&self.running);
        let outcome = self.model.experiment(oracle, tracer, running, scheduler);
        if let Some(verified) = outcome {
            let mut new_path = path.clone();
            new_path.parent().expect("pop file name");
            new_path.parent().expect("pop temp folder");
            if verified.into_iter().all(|b| b) {
                new_path.push(SUCCESSES);
            } else {
                new_path.push(FAILURES);
            }
            new_path.push(path.file_name().expect("file name"));
            rename(&path, new_path)?;
        }
        Ok(())
    }
}

impl<O> Scan<O>
where
    O: Oracle + Clone + Sync,
{
    /// Statistically verifies the provided [`TransitionSystem`] using adaptive bound and the given parameters,
    /// spawning multiple threads.
    pub fn par_adaptive(
        &self,
        confidence: f64,
        precision: f64,
        scheduler: Scheduler,
    ) -> Result<Report, ScanError> {
        if !(0f64 < confidence && confidence < 1f64) {
            return Err(ScanError::OutOfBoundsConfidence(confidence));
        }
        if !(0f64 < precision && precision < 1f64) {
            return Err(ScanError::OutOfBoundsPrecision(precision));
        }

        self.reset();

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        let runs = (0..usize::MAX)
            .into_par_iter()
            .map(|_| self.verification(confidence, precision, scheduler))
            .take_any_while(|_| self.running.load(Ordering::Relaxed))
            .count() as u32;

        let elapsed = start_time.elapsed();
        info!("verification completed in {elapsed:0.2?}");
        Ok(Report {
            runs,
            successes: self.successes(),
            failures: self.failures(),
            violations: self.violations().collect(),
        })
    }

    /// Produces and saves the traces for the given number of runs,
    /// using the provided [`Tracer`],
    /// spawning multiple threads.
    pub fn par_traces<'a, T>(
        &self,
        runs: usize,
        path: &Path,
        model_data: &'a T::ModelData,
        scheduler: Scheduler,
    ) -> std::io::Result<()>
    where
        T: Tracer<'a>,
        T::ModelData: Sync,
    {
        info!("tracing starting");
        let start_time = Instant::now();
        self.reset();
        create_traces_dirs_tree(path)?;

        (0..runs)
            .into_par_iter()
            .try_for_each(|idx| self.trace::<T>(path, model_data, scheduler, idx))?;

        let elapsed = start_time.elapsed();
        info!("tracing completed in {elapsed:0.2?}");
        Ok(())
    }
}

fn create_traces_dirs_tree(path: &Path) -> std::io::Result<()> {
    create_dir_all(path)?;
    let mut path = path.join(TEMP);
    create_dir(&path)?;
    assert!(path.pop());
    path.push(SUCCESSES);
    create_dir(&path)?;
    assert!(path.pop());
    path.push(FAILURES);
    create_dir(&path)
}
