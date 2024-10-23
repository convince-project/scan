use crate::Mtl;
use log::info;
use rand::prelude::*;
use rayon::prelude::*;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Atom<A: Clone + PartialEq> {
    Predicate(usize),
    Event(A),
}

/// Trait implementing a Transition System (TS), as defined in [^1].
/// As such, it is possible to verify it against MTL specifications.
///
/// [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.
pub trait TransitionSystem: Clone + Send + Sync {
    /// The type of the actions that trigger transitions between states in the TS.
    type Action: Clone + PartialEq + Send + Sync;

    const TIMEOUT: usize = usize::MAX;

    /// The label function of the TS valuates the propositions of its set of propositions for the current state.
    // TODO FIXME: bitset instead of Vec<bool>?
    fn labels(&self) -> Vec<bool>;

    /// The transition relation relates [`Self::Action`]s and post-states that constitutes possible transitions from the current state.
    fn transitions(self) -> Vec<(Self::Action, Self)>;

    /// Verifies the TS against the provided [`Mtl`] specifications,
    /// and, when it finds a counterexample, it returns iys execution trace.
    ///
    /// It uses a depth-first, exhaustive search algorithm.
    /// Search is parallelized.
    // fn check_exhaustive(self, props: Properties) -> Option<Vec<(Self::Action, Vec<bool>)>> {
    //     // let oracle_guarantees: Vec<Box<dyn Oracle>> =
    //     //     props.guarantees.into_iter().map(|_| todo!()).collect();
    //     // let oracle_assumes: Vec<Box<dyn Oracle>> =
    //     //     props.assumes.into_iter().map(|_| todo!()).collect();
    //     self.transitions()
    //         .into_par_iter()
    //         .find_map_first(move |(action, ts)| {
    //             let mut queue = Vec::from([(0, action, ts)]);
    //             let mut trace = Vec::new();
    //             while let Some((trace_len, action, ts)) = queue.pop() {
    //                 assert!(trace_len <= trace.len());
    //                 trace.truncate(trace_len);
    //                 let labels = ts.labels();
    //                 if !props.assumes.iter_mut().all(|p| p(&labels)) {
    //                     // If some assume is not satisfied,
    //                     // disregard state and move on.
    //                     continue;
    //                 } else if props.guarantees.iter_mut().all(|p| p(&labels)) {
    //                     // If all guarantees are satisfied,
    //                     // expand branching search for a counterexample along this trace.
    //                     trace.push((action, labels));

    //                     // pop from back and push back (stack): depth-first-search
    //                     // Generate all possible transitions and resulting state.
    //                     queue.extend(
    //                         ts.transitions()
    //                             .into_iter()
    //                             .map(|(a, ts)| (trace_len + 1, a, ts)),
    //                     );

    //                     // pop from back and push in front (FIFO queue): width-first-search
    //                     // WARN: requires memorizing all traces and uses too much memory.
    //                 } else {
    //                     // If guarantee is violated, we have found a counter-example!
    //                     trace.push((action, labels));
    //                     return Some(trace);
    //                 }
    //             }
    //             None
    //         })
    // }

    fn find_counterexample(
        &self,
        guarantees: &[Mtl<Atom<Self::Action>>],
        assumes: &[Mtl<Atom<Self::Action>>],
        confidence: f64,
        precision: f64,
    ) -> Option<Vec<(Self::Action, Vec<bool>)>> {
        // WARN FIXME TODO: Account for inconclusive traces (e.g. where assumes are violated)
        // Pass s=1, f=0 to adaptive criterion so that avarage success value v=1.
        // In this case, the adaptive criterion is (much) lower than Okamoto criterion
        // because v=1 is the furthest possible from v=0.5 where the two criteria coincide.
        let runs = adaptive_bound(1f64, confidence, precision).ceil() as u32;
        (0..runs).into_par_iter().find_map_any(|_| {
            let mut rng = rand::thread_rng();
            self.clone()
                .trace_counterexample(guarantees, assumes, &mut rng)
        })
    }

    fn trace_counterexample<R: Rng>(
        mut self,
        guarantees: &[Mtl<Atom<Self::Action>>],
        assumes: &[Mtl<Atom<Self::Action>>],
        rng: &mut R,
    ) -> Option<Vec<(Self::Action, Vec<bool>)>> {
        let mut trace = Vec::new();
        while let Some((action, new_ts)) = self.transitions().choose(rng) {
            trace.push((action.to_owned(), new_ts.labels()));
            self = new_ts.to_owned();
        }
        if !assumes.iter().all(|p| p.eval(trace.as_slice())) {
            // If some assume is not satisfied,
            // disregard state and move on.
            None
        } else if guarantees.iter().all(|p| p.eval(trace.as_slice())) {
            // If all guarantees are satisfied,
            None
        } else {
            // If guarantee is violated, we have found a counter-example!
            Some(trace)
        }
    }

    fn experiment<R: Rng>(
        mut self,
        guarantees: &[Mtl<Atom<Self::Action>>],
        assumes: &[Mtl<Atom<Self::Action>>],
        rng: &mut R,
    ) -> Option<bool> {
        let mut trace = Vec::new();
        while let Some((action, new_ts)) = self.transitions().choose(rng) {
            trace.push((action.to_owned(), new_ts.labels()));
            self = new_ts.to_owned();
            if trace.len() > Self::TIMEOUT {
                break;
            }
        }
        if !assumes.iter().all(|p| p.eval(trace.as_slice())) {
            // If some assume is not satisfied,
            // disregard state and move on.
            None
        } else if guarantees.iter().all(|p| p.eval(trace.as_slice())) {
            // If all guarantees are satisfied,
            Some(true)
        } else {
            // If guarantee is violated, we have found a counter-example!
            Some(false)
        }
    }

    fn par_adaptive(
        &self,
        guarantees: &[Mtl<Atom<Self::Action>>],
        assumes: &[Mtl<Atom<Self::Action>>],
        confidence: f64,
        precision: f64,
        s: Arc<AtomicU32>,
        f: Arc<AtomicU32>,
    ) {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        (0..usize::MAX)
            .into_par_iter()
            .take_any_while(|_| {
                let mut rng = rand::thread_rng();
                let local_s;
                let local_f;
                if let Some(result) = self.clone().experiment(guarantees, assumes, &mut rng) {
                    if result {
                        // If all guarantees are satisfied, the execution is successful
                        local_s = s.fetch_add(1, Ordering::Relaxed) + 1;
                        local_f = f.load(Ordering::Relaxed);
                        info!("runs: {local_s} successes");
                    } else {
                        // If guarantee is violated, we have found a counter-example!
                        local_s = s.load(Ordering::Relaxed);
                        local_f = f.fetch_add(1, Ordering::Relaxed) + 1;
                        info!("runs: {local_f} failures");
                    }
                } else {
                    local_f = f.load(Ordering::Relaxed);
                    local_s = s.load(Ordering::Relaxed);
                }
                let n = local_s + local_f;
                // Avoid division by 0
                let avg = if n == 0 {
                    0.5f64
                } else {
                    local_s as f64 / n as f64
                };
                adaptive_bound(avg, confidence, precision) > n as f64
            })
            .count();
    }
}

// An efficient statistical model checker for nondeterminism and rare events,
// Carlos E. Budde, Pedro R. D’Argenio, Arnd Hartmanns, Sean Sedwards.
// International Journal on Software Tools for Technology Transfer (2020) 22:759–780
// https://doi.org/10.1007/s10009-020-00563-2

/// Computes Okamoto bound for given confidence and precision.
pub fn okamoto_bound(confidence: f64, precision: f64) -> f64 {
    (2f64 / (1f64 - confidence)).ln() / (2f64 * precision.powf(2f64))
}

/// Computes adaptive bound for given confidence, precision and (partial) experimental results.
pub fn adaptive_bound(avg: f64, confidence: f64, precision: f64) -> f64 {
    4f64 * okamoto_bound(confidence, precision)
        * (0.25f64 - ((avg - 0.5f64).abs() - (2f64 * precision / 3f64)).powf(2f64))
}

/// Computes precision for given experimental results and confidence
/// deriving it from adaptive bound through quadratic equation.
pub fn derive_precision(s: u32, f: u32, confidence: f64) -> f64 {
    let n = s + f;
    let avg = s as f64 / n as f64;
    let k = 2f64 * (2f64 / (1f64 - confidence)).ln();
    // Compute quadratic equation coefficients.
    let a = (n as f64) + (4f64 * k / 9f64);
    let b = -4f64 * k * (avg - 0.5f64).abs() / 3f64;
    let c = k * ((avg - 0.5f64).powf(2f64) - 0.25f64);
    // Take (larger positive) quadratic equation solution.
    (-b + (b.powf(2f64) - 4f64 * a * c).sqrt()) / (2f64 * a)
}
