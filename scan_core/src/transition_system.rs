use crate::Mtl;
use rand::prelude::*;
use rayon::prelude::*;

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

    fn check_statistics(&self, props: Properties) -> Option<Vec<(Self::Action, Vec<bool>)>> {
        (0..).par_bridge().find_map_any(|_| {
            let mut rng = rand::thread_rng();
            let mut ts = self.clone();
            let mut trace = Vec::new();
            let mut actions = Vec::new();
            while let Some((action, new_ts)) = ts.transitions().choose(&mut rng) {
                trace.push(new_ts.labels());
                actions.push(action.to_owned());
                ts = new_ts.to_owned();
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
        })
    }
}
