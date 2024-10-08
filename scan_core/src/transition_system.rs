use crate::Mtl;
use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use log::info;
use rand::prelude::*;
use rayon::prelude::*;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::Relaxed;

pub struct Properties {
    pub guarantees: Vec<Mtl<usize>>,
    pub assumes: Vec<Mtl<usize>>,
}

// trait Oracle: Clone + Sync + Send + FnMut(&[bool]) -> bool {}

/// Trait implementing a Transition System (TS), as defined in [^1].
/// As such, it is possible to verify it against MTL specifications.
///
/// [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.
pub trait TransitionSystem: Clone + Send + Sync {
    /// The type of the actions that trigger transitions between states in the TS.
    type Action: Clone + Send;

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
        props: Properties,
        confidence: f64,
        precision: f64,
    ) -> Option<Vec<(Self::Action, Vec<bool>)>> {
        // WARN FIXME TODO: Account for inconclusive traces (e.g. where assumes are violated)
        let okamoto = okamoto(confidence, precision).ceil() as u32;
        (0..okamoto).into_par_iter().find_map_any(|_| {
            let mut rng = rand::thread_rng();
            self.clone().trace_counterexample(&props, &mut rng)
        })
    }

    // fn check_avarage(&self, props: Properties, experiments: usize) -> (u32, u32) {
    //     (0..experiments)
    //         .into_par_iter()
    //         .filter_map(|_| {
    //             let mut rng = rand::thread_rng();
    //             let mut trace = Vec::new();
    //             let mut actions = Vec::new();
    //             let mut ts = self.to_owned();
    //             while let Some((action, new_ts)) = ts.transitions().choose(&mut rng) {
    //                 trace.push(new_ts.labels());
    //                 actions.push(action.to_owned());
    //                 ts = new_ts.to_owned();
    //             }
    //             if !props.assumes.iter().all(|p| p.eval(trace.as_slice())) {
    //                 // If some assume is not satisfied,
    //                 // disregard state and move on.
    //                 None
    //             } else if props.guarantees.iter().all(|p| p.eval(trace.as_slice())) {
    //                 // If all guarantees are satisfied, execution succeeded.
    //                 Some((1, 0))
    //             } else {
    //                 // If guarantee is violated, we have found a counter-example!
    //                 Some((0, 1))
    //             }
    //         })
    //         .reduce(|| (0, 0), |a, b| (a.0 + b.0, a.1 + b.1))
    // }

    fn trace_counterexample<R: Rng>(
        mut self,
        props: &Properties,
        rng: &mut R,
    ) -> Option<Vec<(Self::Action, Vec<bool>)>> {
        let mut trace = Vec::new();
        let mut actions = Vec::new();
        while let Some((action, new_ts)) = self.transitions().choose(rng) {
            trace.push(new_ts.labels());
            actions.push(action.to_owned());
            self = new_ts.to_owned();
        }
        if !props.assumes.iter().all(|p| p.eval(trace.as_slice())) {
            // If some assume is not satisfied,
            // disregard state and move on.
            None
        } else if props.guarantees.iter().all(|p| p.eval(trace.as_slice())) {
            // If all guarantees are satisfied,
            None
        } else {
            // If guarantee is violated, we have found a counter-example!
            let annotated_trace: Vec<(Self::Action, Vec<bool>)> =
                actions.into_iter().zip(trace).collect();
            Some(annotated_trace)
        }
    }

    fn experiment<R: Rng>(mut self, props: &Properties, rng: &mut R) -> Option<bool> {
        let mut trace = Vec::new();
        while let Some((_, new_ts)) = self.transitions().choose(rng) {
            trace.push(new_ts.labels());
            self = new_ts.to_owned();
        }
        if !props.assumes.iter().all(|p| p.eval(trace.as_slice())) {
            // If some assume is not satisfied,
            // disregard state and move on.
            None
        } else if props.guarantees.iter().all(|p| p.eval(trace.as_slice())) {
            // If all guarantees are satisfied,
            Some(true)
        } else {
            // If guarantee is violated, we have found a counter-example!
            Some(false)
        }
    }

    fn adaptive(&self, props: &Properties, confidence: f64, precision: f64) -> f64 {
        let (s, f) = (0..)
            .fold_while((0, 0), |(s, f), _| {
                let mut rng = rand::thread_rng();
                let mut trace = Vec::new();
                // let mut actions = Vec::new();
                let mut ts = self.to_owned();
                while let Some((_action, new_ts)) = ts.transitions().choose(&mut rng) {
                    trace.push(new_ts.labels());
                    // actions.push(action.to_owned());
                    ts = new_ts.to_owned();
                }
                let mut s = s;
                let mut f = f;
                if !props.assumes.iter().all(|p| p.eval(trace.as_slice())) {
                    // If some assume is not satisfied,
                    // disregard state and move on.
                    return Continue((s, f));
                } else if props.guarantees.iter().all(|p| p.eval(trace.as_slice()))
                    && rng.gen_bool(0.666)
                {
                    // If all guarantees are satisfied,
                    s += 1;
                } else {
                    // If guarantee is violated, we have found a counter-example!
                    f += 1;
                }
                if adaptive_criterion(s, f, confidence, precision) {
                    info!("runs: {s} successes, {f} failures");
                    Continue((s, f))
                } else {
                    Done((s, f))
                }
            })
            .into_inner();
        s as f64 / (s + f) as f64
    }

    fn par_adaptive(&self, props: &Properties, confidence: f64, precision: f64) -> f64 {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        let s = AtomicUsize::new(0);
        let f = AtomicUsize::new(0);
        (0..usize::MAX)
            .into_par_iter()
            .filter_map(|_| {
                let mut rng = rand::thread_rng();
                self.clone().experiment(props, &mut rng)
            })
            .inspect(|result| {
                if *result {
                    // If all guarantees are satisfied,
                    let s = s
                        .fetch_update(Relaxed, Relaxed, |s| s.checked_add(1))
                        .expect("");
                    info!("runs: {s} successes");
                } else {
                    // If guarantee is violated, we have found a counter-example!
                    let f = f
                        .fetch_update(Relaxed, Relaxed, |f| f.checked_add(1))
                        .expect("");
                    info!("runs: {f} failures");
                }
            })
            .take_any_while(|_| {
                let s = s.load(Relaxed) as u32;
                let f = f.load(Relaxed) as u32;
                adaptive_criterion(s, f, confidence, precision)
            })
            .count();
        let s = s.into_inner();
        let f = f.into_inner();
        s as f64 / (s + f) as f64
    }
}

// An efficient statistical model checker for nondeterminism and rare events,
// Carlos E. Budde, Pedro R. D’Argenio, Arnd Hartmanns, Sean Sedwards.
// International Journal on Software Tools for Technology Transfer (2020) 22:759–780
// https://doi.org/10.1007/s10009-020-00563-2
fn okamoto(confidence: f64, precision: f64) -> f64 {
    2f64 * (2f64 / (1f64 - confidence)).ln() / precision.powf(2f64)
}

fn adaptive_criterion(s: u32, f: u32, confidence: f64, precision: f64) -> bool {
    let n = s + f;
    let avg = s as f64 / (s + f) as f64;
    (n as f64)
        < okamoto(confidence, precision)
            * (0.25f64 - ((avg - 0.5f64).abs() - (2f64 * precision / 3f64)).powf(2f64))
}
