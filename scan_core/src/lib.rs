//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod channel_system;
mod cs_model;
mod dummy_rng;
mod grammar;
mod mtl;
mod oracle;
mod pg_model;
mod pmtl;
pub mod program_graph;
mod smc;
mod transition_system;

use core::marker::Sync;
pub use cs_model::{Atom, CsModel, CsModelRun};
use dummy_rng::DummyRng;
pub use grammar::{Expression, Float, Integer, Type, TypeError, Val};
use log::{info, trace};
pub use mtl::{Mtl, MtlOracle};
pub use oracle::{Oracle, OracleGenerator};
pub use pg_model::{PgModel, PgModelRun};
pub use pmtl::{Pmtl, PmtlOracle, PmtlOracleRun};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
pub use smc::*;
use std::{
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU32, Ordering},
    },
    time::Instant,
};
pub use transition_system::{Tracer, TransitionSystem, TransitionSystemGenerator};

/// The type that represents time.
pub type Time = u32;

/// The possible outcomes of a model execution.
#[derive(Debug, Clone)]
pub enum RunOutcome {
    /// The run was not completed.
    /// This can happen because:
    ///
    /// - Execution exceeded maximum length;
    /// - Execution exceeded maximum duration; or
    /// - Execution violated an assume.
    Incomplete,
    /// The run failed by violating the guarantees corresponding to the given index.
    Verified(Vec<bool>),
}

/// The main type to interface with the verification capabilities of SCAN.
/// [`Scan`] holds the model, properties and other data necessary to run the verification process.
/// The type of models and properties is abstracted through the [`TransitionSystem`] and [`Oracle`] traits,
/// to provide a unified interface.
#[derive(Debug, Clone)]
pub struct Scan<TsG, OG> {
    tsd: TsG,
    oracle: OG,
    running: Arc<AtomicBool>,
    successes: Arc<AtomicU32>,
    failures: Arc<AtomicU32>,
    violations: Arc<Mutex<Vec<u32>>>,
}

impl<TsG, OG> Scan<TsG, OG> {
    /// Create new [`Scan`] object.
    pub fn new(tsd: TsG, oracle: OG) -> Self {
        Scan {
            tsd,
            oracle,
            running: Arc::new(AtomicBool::new(false)),
            successes: Arc::new(AtomicU32::new(0)),
            failures: Arc::new(AtomicU32::new(0)),
            violations: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn reset(&self) {
        self.successes.store(0, Ordering::Relaxed);
        self.failures.store(0, Ordering::Relaxed);
        self.violations.lock().unwrap().clear();
        self.running.store(true, Ordering::Relaxed);
    }

    /// Tells whether a verification task is currently running.
    pub fn running(&self) -> bool {
        self.running.load(Ordering::Relaxed)
    }

    /// Returns the number of successful executions in the current verification run.
    pub fn successes(&self) -> u32 {
        self.successes.load(Ordering::Relaxed)
    }

    /// Returns the number of failed executions in the current verification run.
    pub fn failures(&self) -> u32 {
        self.failures.load(Ordering::Relaxed)
    }

    /// Returns a vector where each entry contains the number of violations of the associated property in the current verification run.
    pub fn violations(&self) -> Vec<u32> {
        self.violations.lock().expect("lock").clone()
    }
}

impl<TsG: TransitionSystemGenerator, OG: OracleGenerator> Scan<TsG, OG> {
    fn verification(&self, confidence: f64, precision: f64, duration: Time) {
        let local_successes;
        let local_failures;
        let oracle = self.oracle.generate();
        let mut ts = self.tsd.generate();

        let result = ts.experiment(duration, oracle, self.running.clone());
        if !self.running.load(Ordering::Relaxed) {
            return;
        }
        match result {
            RunOutcome::Verified(guarantees) => {
                if guarantees.iter().all(|b| *b) {
                    local_successes = self.successes.fetch_add(1, Ordering::Relaxed);
                    local_failures = self.failures.load(Ordering::Relaxed);
                    // If all guarantees are satisfied, the execution is successful
                    trace!("runs: {local_successes} successes");
                } else {
                    local_successes = self.successes.load(Ordering::Relaxed);
                    local_failures = self.failures.fetch_add(1, Ordering::Relaxed);
                    let violations = &mut *self.violations.lock().unwrap();
                    violations.resize(violations.len().max(guarantees.len()), 0);
                    guarantees.iter().zip(violations.iter_mut()).for_each(
                        |(success, violations)| {
                            if !success {
                                *violations += 1;
                            }
                        },
                    );
                    // If guarantee is violated, we have found a counter-example!
                    trace!("runs: {local_failures} failures");
                }
            }
            RunOutcome::Incomplete => return,
        }
        let runs = local_successes + local_failures;
        // Avoid division by 0
        let avg = if runs == 0 {
            0.5f64
        } else {
            local_successes as f64 / runs as f64
        };
        if adaptive_bound(avg, confidence, precision) <= runs as f64 {
            info!("adaptive bound satisfied");
            self.running.store(false, Ordering::Relaxed);
        }
    }

    /// Statistically verifies the provided [`TransitionSystem`] using adaptive bound and the given parameters.
    pub fn adaptive(&self, confidence: f64, precision: f64, duration: Time) {
        // Cannot return as a T::Err, don't want to include anyhow in scan_core
        assert!(0f64 < confidence && confidence < 1f64);
        assert!(0f64 < precision && precision < 1f64);

        self.reset();

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        let _ = (0..usize::MAX)
            .map(|_| self.verification(confidence, precision, duration))
            .take_while(|_| self.running.load(Ordering::Relaxed))
            .count();

        let elapsed = start_time.elapsed();
        info!("verification time elapsed: {elapsed:0.2?}");
        info!("verification terminating");
    }

    fn trace<'a, P>(&'a self, tracer: P, duration: Time)
    where
        P: Tracer<<<TsG as TransitionSystemGenerator>::Ts<'a> as TransitionSystem>::Event>,
    {
        let oracle = self.oracle.generate();
        let mut ts = self.tsd.generate();
        ts.trace(duration, oracle, tracer)
    }

    /// Produces and saves the traces for the given number of runs,
    /// using the provided [`Tracer`].
    pub fn traces<'a, T>(&'a self, runs: usize, tracer: T, duration: Time)
    where
        T: Tracer<<<TsG as TransitionSystemGenerator>::Ts<'a> as TransitionSystem>::Event>,
    {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("tracing starting");
        let start_time = Instant::now();

        (0..runs).for_each(|_| self.trace::<T>(tracer.clone(), duration));

        let elapsed = start_time.elapsed();
        info!("tracing time elapsed: {elapsed:0.2?}");
        info!("tracing terminating");
    }
}

impl<TsG, OG> Scan<TsG, OG>
where
    TsG: TransitionSystemGenerator + Sync,
    OG: OracleGenerator + Sync,
{
    /// Statistically verifies the provided [`TransitionSystem`] using adaptive bound and the given parameters,
    /// spawning multiple threads.
    pub fn par_adaptive(&self, confidence: f64, precision: f64, duration: Time) {
        assert!(0f64 < confidence && confidence < 1f64);
        assert!(0f64 < precision && precision < 1f64);

        self.reset();

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        let _ = (0..usize::MAX)
            .into_par_iter()
            .map(|_| self.verification(confidence, precision, duration))
            .take_any_while(|_| self.running.load(Ordering::Relaxed))
            .count();

        let elapsed = start_time.elapsed();
        info!("verification time elapsed: {elapsed:0.2?}");
        info!("verification terminating");
    }

    /// Produces and saves the traces for the given number of runs,
    /// using the provided [`Tracer`],
    /// spawning multiple threads.
    pub fn par_traces<'a, T>(&'a self, runs: usize, tracer: T, duration: Time)
    where
        T: Tracer<<<TsG as TransitionSystemGenerator>::Ts<'a> as TransitionSystem>::Event>,
    {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("tracing starting");
        let start_time = Instant::now();

        (0..runs)
            .into_par_iter()
            .for_each(|_| self.trace::<T>(tracer.clone(), duration));

        let elapsed = start_time.elapsed();
        info!("tracing time elapsed: {elapsed:0.2?}");
        info!("tracing terminating");
    }
}
