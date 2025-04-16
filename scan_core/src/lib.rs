//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod channel_system;
mod grammar;
mod model;
mod pmtl;
pub mod program_graph;
mod smc;
mod transition_system;

pub use grammar::*;
use log::{info, warn};
pub use model::*;
pub use pmtl::*;
use rand::RngCore;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
pub use smc::*;
use std::{
    error::Error,
    marker::PhantomData,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU16, Ordering},
    },
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

pub struct Scan<Event, Err, Ts>
where
    Err: Error,
    Ts: TransitionSystem<Event, Err>,
{
    ts: Arc<Ts>,
    oracle: Arc<PmtlOracle>,
    running: Arc<AtomicBool>,
    successes: Arc<AtomicU16>,
    failures: Arc<AtomicU16>,
    violations: Arc<Mutex<Vec<u16>>>,
    _event: PhantomData<Event>,
    _err: PhantomData<Err>,
}

impl<Event, Err, T> Scan<Event, Err, T>
where
    Event: Sync,
    Err: Error + Sync,
    T: TransitionSystem<Event, Err> + 'static,
{
    pub fn new(ts: T, oracle: PmtlOracle) -> Self {
        Self {
            ts: Arc::new(ts),
            oracle: Arc::new(oracle),
            running: Arc::new(AtomicBool::new(false)),
            successes: Arc::new(AtomicU16::new(0)),
            failures: Arc::new(AtomicU16::new(0)),
            violations: Arc::new(Mutex::new(Vec::new())),
            _event: PhantomData,
            _err: PhantomData,
        }
    }

    pub fn running(&self) -> bool {
        self.running.load(Ordering::Relaxed)
    }

    pub fn successes(&self) -> u16 {
        self.successes.load(Ordering::Relaxed)
    }

    pub fn failures(&self) -> u16 {
        self.failures.load(Ordering::Relaxed)
    }

    pub fn violations(&self) -> Vec<u16> {
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
    ) where
        P: Tracer<Event> + 'static,
    {
        info!("verification starting");
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

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        std::thread::spawn(move || {
            (0..usize::MAX)
                .into_par_iter()
                .take_any_while(|_| {
                    // .take_while(|_| {

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
                                    info!("runs: {} successes", local_successes);
                                }
                                RunOutcome::Fail(guarantee) => {
                                    local_successes = successes.load(Ordering::Relaxed);
                                    local_failures = failures.fetch_add(1, Ordering::Relaxed);
                                    let violations = &mut *violations.lock().unwrap();
                                    violations.resize(violations.len().max(guarantee + 1), 0);
                                    violations[guarantee] += 1;
                                    // If guarantee is violated, we have found a counter-example!
                                    info!("runs: {} failures", local_failures);
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
                })
                .count();
        });
        info!("verification terminating");
    }
}
