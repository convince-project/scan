//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

// #![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod channel_system;
mod grammar;
mod model;
mod mtl;
mod pg_model;
mod pmtl;
pub mod program_graph;
mod smc;
mod transition_system;

pub use grammar::*;
use log::{info, trace, warn};
pub use model::*;
pub use mtl::*;
pub use pg_model::PgModel;
pub use pmtl::*;
use rand::RngCore;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
pub use smc::*;
use std::{
    error::Error,
    marker::PhantomData,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU32, Ordering},
    },
    time::Instant,
};
pub use transition_system::*;

/// The type that represents time.
pub type Time = u32;

struct DummyRng;

impl RngCore for DummyRng {
    fn next_u32(&mut self) -> u32 {
        panic!("DummyRng should never be called")
    }

    fn next_u64(&mut self) -> u64 {
        panic!("DummyRng should never be called")
    }

    fn fill_bytes(&mut self, dst: &mut [u8]) {
        let _ = dst;
        panic!("DummyRng should never be called")
    }
}

/// The possible outcomes of a model execution.
#[derive(Debug, Clone, Copy)]
pub enum RunOutcome {
    /// The run was not completed.
    /// This can happen because:
    ///
    /// - Execution exceeded maximum lenght;
    /// - Execution exceeded maximum duration; or
    /// - Execution violated an assume.
    Incomplete,
    /// The run completed successfully.
    Success,
    /// The run failed by violating the guarantee corresponding to the given index.
    Fail(usize),
}

pub trait Oracle: Clone + Send + Sync {
    fn update(&mut self, state: &[bool], time: Time);

    fn output_assumes(&self) -> impl Iterator<Item = Option<bool>>;

    fn output_guarantees(&self) -> impl Iterator<Item = Option<bool>>;

    fn final_output_assumes(&self) -> impl Iterator<Item = bool>;

    fn final_output_guarantees(&self) -> impl Iterator<Item = bool>;
}

#[derive(Clone)]
pub struct Scan<Event, Err, Ts, O>
where
    Err: Error,
    Ts: TransitionSystem<Event, Err>,
    O: Oracle,
{
    ts: Arc<Ts>,
    oracle: Arc<O>,
    running: Arc<AtomicBool>,
    successes: Arc<AtomicU32>,
    failures: Arc<AtomicU32>,
    violations: Arc<Mutex<Vec<u32>>>,
    _event: PhantomData<Event>,
    _err: PhantomData<Err>,
}

impl<Event, Err, T, O> Scan<Event, Err, T, O>
where
    Event: Sync,
    Err: Error + Sync,
    T: TransitionSystem<Event, Err> + 'static,
    O: Oracle + 'static,
{
    pub fn new(ts: T, oracle: O) -> Self {
        Self {
            ts: Arc::new(ts),
            oracle: Arc::new(oracle),
            running: Arc::new(AtomicBool::new(false)),
            successes: Arc::new(AtomicU32::new(0)),
            failures: Arc::new(AtomicU32::new(0)),
            violations: Arc::new(Mutex::new(Vec::new())),
            _event: PhantomData,
            _err: PhantomData,
        }
    }

    pub fn running(&self) -> bool {
        self.running.load(Ordering::Relaxed)
    }

    pub fn successes(&self) -> u32 {
        self.successes.load(Ordering::Relaxed)
    }

    pub fn failures(&self) -> u32 {
        self.failures.load(Ordering::Relaxed)
    }

    pub fn violations(&self) -> Vec<u32> {
        self.violations.lock().expect("lock").clone()
    }

    /// Statistically verifies [`CsModel`] using adaptive bound and the given parameters.
    /// It allows to optionally pass a [`Tracer`] object to record the produced traces,
    /// and a state [`Mutex`] to be updated with the results as they are produced.
    pub fn adaptive<P>(
        &self,
        confidence: f64,
        precision: f64,
        // length: usize,
        duration: Time,
        tracer: Option<P>,
        single_thread: bool,
    ) where
        P: Tracer<Event> + 'static,
    {
        self.successes.store(0, Ordering::Relaxed);
        self.failures.store(0, Ordering::Relaxed);
        self.violations.lock().unwrap().clear();
        self.running.store(true, Ordering::Relaxed);

        let ts = self.ts.clone();
        let oracle = self.oracle.clone();
        let running = self.running.clone();
        let successes = self.successes.clone();
        let failures = self.failures.clone();
        let violations = self.violations.clone();

        let verification = || {
            let local_successes;
            let local_failures;

            match ts.as_ref().clone().experiment(
                duration,
                oracle.as_ref().clone(),
                tracer.clone(),
                running.clone(),
            ) {
                Ok(result) => {
                    if !running.load(Ordering::Relaxed) {
                        return false;
                    }
                    match result {
                        RunOutcome::Success => {
                            local_successes = successes.fetch_add(1, Ordering::Relaxed);
                            local_failures = failures.load(Ordering::Relaxed);
                            // If all guarantees are satisfied, the execution is successful
                            trace!("runs: {} successes", local_successes);
                        }
                        RunOutcome::Fail(guarantee) => {
                            local_successes = successes.load(Ordering::Relaxed);
                            local_failures = failures.fetch_add(1, Ordering::Relaxed);
                            let violations = &mut *violations.lock().unwrap();
                            violations.resize(violations.len().max(guarantee + 1), 0);
                            violations[guarantee] += 1;
                            // If guarantee is violated, we have found a counter-example!
                            trace!("runs: {} failures", local_failures);
                        }
                        RunOutcome::Incomplete => return true,
                    }
                }
                Err(err) => {
                    warn!("run returned error: {err}");
                    return true;
                }
            };
            let runs = local_successes + local_failures;
            // Avoid division by 0
            let avg = if runs == 0 {
                0.5f64
            } else {
                local_successes as f64 / runs as f64
            };
            if adaptive_bound(avg, confidence, precision) <= runs as f64 {
                info!("adaptive bound satisfied");
                running.store(false, Ordering::Relaxed);
                false
            } else {
                true
            }
        };

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        if single_thread {
            (0..usize::MAX).take_while(|_| verification()).count();
        } else {
            (0..usize::MAX)
                .into_par_iter()
                .take_any_while(|_| verification())
                .count();
        }

        let elapsed = start_time.elapsed();
        info!("verification time elapsed: {elapsed:0.2?}");
        info!("verification terminating");
    }
}
