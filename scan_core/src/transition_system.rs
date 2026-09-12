use std::hash::{Hash, Hasher};
use std::mem;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Instant;

use bumpalo::Bump;
use fixedbitset::FixedBitSet;
use log::{info, trace};
use petgraph::acyclic::Acyclic;
use petgraph::data::Build;
use petgraph::graph::DiGraph;
use rand::rngs::SmallRng;
use thiserror::Error;

use crate::channel_system::{
    Action, Channel, ChannelSystem, ChannelSystemRun, CsError, EventType, Location, Message, PgId,
    PgIndex,
};
use crate::{BooleanExpr, Oracle, RunOutcome, Time, Tracer, Val};

type Dag = Acyclic<DiGraph<(), (), PgIndex>>;

/// Errors produced by a [`TransitionSystem`].
#[derive(Debug, Clone, Copy, Error)]
pub enum TsError {
    /// The CS returned an error of its own.
    #[error("error from channel system {0:?}")]
    ChannelSystem(CsError),
    /// The default value set for the port is not the right type,
    /// i.e., the type of messages of the channel.
    #[error("default port value is not the type of the channel {0:?}")]
    WrongPortType(Channel),
}

impl From<CsError> for TsError {
    fn from(value: CsError) -> Self {
        Self::ChannelSystem(value)
    }
}

/// An atomic variable exposed by the [`ChannelSystem to the TransitionSystem`].
#[derive(Debug, Clone, Copy)]
pub enum Atom {
    /// A predicate.
    State(Channel, usize),
    /// A send event.
    Event(Channel),
}

/// A definition type that instances new [`CsModelRun`].
#[derive(Debug, Clone)]
pub struct TransitionSystem {
    cs: ChannelSystem,
    // ports are supposed to be ordered by channel
    ports: Vec<Channel>,
    vals: Vec<Vec<Val>>,
    predicates: Vec<BooleanExpr<Atom>>,
    pg_list: Vec<PgId>,
}

impl TransitionSystem {
    /// Creates a new [`CsModel`] from a [`ChannelSystemBuilder`].
    pub fn new(cs: ChannelSystem) -> Self {
        let mut pg_list = Vec::from_iter(cs.program_graph_ids());
        pg_list.shrink_to_fit();
        Self {
            ports: Vec::new(),
            vals: Vec::new(),
            cs,
            predicates: Vec::new(),
            pg_list,
        }
    }

    /// Adds a new port to the [`CsModel`],
    /// which is given by an [`Channel`] and a default [`Val`] value.
    pub fn add_port(&mut self, channel: Channel, mut value: Vec<Val>) -> Result<(), TsError> {
        let types = self.cs.channel(channel)?.0;
        if types.len() != value.len() || types.iter().zip(&value).any(|(t, val)| val.r#type() != *t)
        {
            return Err(TsError::WrongPortType(channel));
        }
        // Keep ports list ordered
        // Don't insert duplicated ports
        if let Err(index) = self.ports.binary_search(&channel) {
            self.ports.insert(index, channel);
            value.shrink_to_fit();
            self.vals.insert(index, value);
        }
        assert!(self.ports.is_sorted());
        assert_eq!(self.ports.len(), self.vals.len());
        Ok(())
    }

    /// Adds a new predicate to the [`CsModel`],
    /// which is an expression over the CS's channels.
    pub fn add_predicate(&mut self, predicate: BooleanExpr<Atom>) -> Result<(), TsError> {
        // Make sure predicate type-checks
        let _ = predicate.eval::<SmallRng>(
            &|port| match port {
                Atom::State(channel, idx) => {
                    let index = self
                        .ports
                        .binary_search(&channel)
                        .expect("port must have been initialized");
                    self.vals[index][idx]
                }
                Atom::Event(..) => Val::Boolean(false),
            },
            None,
        );
        self.predicates.push(predicate);
        Ok(())
    }

    /// Shrink ports storage to optimize space use.
    /// To be called after having added all ports.
    pub fn shrink(&mut self) {
        self.ports.shrink_to_fit();
        self.vals.shrink_to_fit();
    }

    /// Generates an executable run of the model.
    pub fn new_run(&self) -> TransitionSystemRun<'_> {
        let mut vals = self.vals.clone();
        vals.shrink_to_fit();
        info!(
            "create new transition system run with {} ports and {} predicates",
            self.ports.len(),
            self.predicates.len(),
        );
        TransitionSystemRun {
            ts: self,
            cs: self.cs.new_instance(),
            vals,
            last_event: None,
        }
    }

    pub(crate) fn experiment<O: Oracle + Clone>(
        &self,
        mut oracle: O,
        running: Arc<AtomicBool>,
    ) -> Option<bool> {
        // Diagnostics/stats
        let start_time = Instant::now();
        let mut branches: u32 = 0;

        type BranchData<'a, O> = (O, TransitionSystemRun<'a>, Vec<(FixedBitSet, bool)>, Dag);
        let mut executions_stack: Vec<BranchData<O>> = Vec::new();
        // TODO: measure impact of recycling memory to check wether it is worth it
        let mut recycling_sets: Vec<Vec<(FixedBitSet, bool)>> = Vec::new();
        let mut recycling_dags: Vec<Dag> = Vec::new();
        let pgs = self.pg_list.len();
        let mut amples = vec![(FixedBitSet::with_capacity(pgs), true); pgs];
        let mut restricted_ample = FixedBitSet::with_capacity(pgs);
        let mut restricted_ample_temp = FixedBitSet::with_capacity(pgs);
        let mut bump = Bump::new();
        let mut run = self.new_run();
        let mut dag = Dag::new();
        let dag_ids = (0..pgs).map(|_| dag.add_node(())).collect::<Vec<_>>();

        // Initialize oracle with TS initial state
        oracle.update_state(&Vec::from_iter(run.labels()));
        run.fastforward(&bump);

        let mut len_lower_bound = 1;

        // FILO stack: depth-first search
        'l: while running.load(Ordering::Relaxed) {
            // NOTE: at the start of the loop, ample sets need to be updated.
            // Do so only if no viable set is left,
            // to reduce expensive calls to the ample method.
            if amples
                .iter()
                .all(|(set, invalid)| *invalid || set.is_clear())
            {
                run.ample(&mut amples);
                len_lower_bound = 1;
            }

            // Find most suitable ample set
            let mut len_min = usize::MAX;
            let mut ample = None;
            'f: for (set, invalid) in amples.iter() {
                // only consider viable sets (i.e., neither invalidated nor empty)
                if !(*invalid || set.is_clear()) {
                    restricted_ample_temp.clear();
                    let mut len = 0;
                    for i in set.ones().filter(|&i| {
                        let i_node = dag_ids[i];
                        set.ones()
                            .all(|j| j == i || dag.is_valid_edge(i_node, dag_ids[j]))
                    }) {
                        len += 1;
                        restricted_ample_temp.insert(i);
                        if len == len_min {
                            // If set is no smaller than the currently smallest one,
                            // discard it and try next set.
                            continue 'f;
                        }
                    }
                    // new set is smaller than previous ones
                    mem::swap(&mut restricted_ample, &mut restricted_ample_temp);
                    ample = Some(set);
                    len_min = len;
                    assert!(len >= len_lower_bound);
                    if len == len_lower_bound {
                        break 'f;
                    }
                }
            }
            assert!(len_min >= len_lower_bound);
            len_lower_bound = len_min;
            if let Some(ample) = ample {
                assert!(!restricted_ample.is_clear());
                assert!(restricted_ample.is_subset(ample));
                let mut transitions = restricted_ample
                    .ones()
                    .flat_map(|b| {
                        run.cs
                            .nosync_possible_transitions_pg(PgId(b as u16))
                            .unwrap()
                            .flat_map(|(action, transitions)| {
                                transitions.map(move |post| (action, post))
                            })
                    })
                    .peekable();
                // There must be active transitions because we checked earlier
                assert!(transitions.peek().is_some());
                'w: while let Some((action, post)) = transitions.next() {
                    if transitions.peek().is_some() {
                        let mut branch_run = run.clone();
                        let mut branch_oracle = oracle.clone();
                        bump.reset();
                        branch_run
                            .transition(&mut branch_oracle, action, post, &bump)
                            .unwrap();
                        if branch_oracle
                            .output_guarantees()
                            .any(|b| b.is_some_and(|b| !b))
                        {
                            // Guarantee violated
                            branches += 1;
                            trace!(
                                "run violates a guarantee: processed {branches} branches in {:?}",
                                start_time.elapsed()
                            );
                            return Some(false);
                        } else if branch_oracle.output_guarantees().any(|b| b.is_none()) {
                            bump.reset();
                            branch_run.fastforward(&bump);
                            // Prepare new DAG
                            let mut branch_dag = if let Some(mut rec_dag) = recycling_dags.pop() {
                                rec_dag.clone_from(&dag);
                                rec_dag
                            } else {
                                dag.clone()
                            };
                            let a = u16::from(action.0) as usize;
                            let node_a = dag_ids[a];
                            restricted_ample.ones().for_each(|b| {
                                if b != a {
                                    let node_b = dag_ids[b];
                                    let _ = branch_dag
                                        .try_update_edge(node_a, node_b, ())
                                        .expect("edge must be valid");
                                }
                            });
                            let mut branch_amples = if let Some(mut set) = recycling_sets.pop() {
                                // clone amples into recycled set;
                                set.iter_mut().zip(amples.iter()).for_each(
                                    |((to_set, to_invalid), (from_set, from_invalid))| {
                                        to_set.clone_from(from_set);
                                        *to_invalid = *from_invalid;
                                    },
                                );
                                set
                            } else {
                                amples.clone()
                            };
                            branch_amples
                                .iter_mut()
                                .for_each(|(set, invalidate)| *invalidate |= set.contains(a));
                            executions_stack.push((
                                branch_oracle,
                                branch_run,
                                branch_amples,
                                branch_dag,
                            ));
                        } else {
                            // Branch satisfies all guarantees and can be discarded even though execution is incomplete
                            branches += 1;
                        }
                    } else {
                        drop(transitions);
                        bump.reset();
                        run.transition(&mut oracle, action, post, &bump).unwrap();
                        if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                            // Guarantee violated
                            branches += 1;
                            trace!(
                                "run violates a guarantee: processed {branches} branches in {:?}",
                                start_time.elapsed()
                            );
                            return Some(false);
                        } else if oracle.output_guarantees().any(|b| b.is_none()) {
                            bump.reset();
                            run.fastforward(&bump);
                            // Update DAG
                            let a = u16::from(action.0) as usize;
                            let node_a = dag_ids[a];
                            restricted_ample.ones().for_each(|b| {
                                if b != a {
                                    let node_b = dag_ids[b];
                                    // update_edge avoids creating duplicated edges
                                    let _ = dag
                                        .try_update_edge(node_a, node_b, ())
                                        .expect("edge must be valid");
                                }
                            });
                            // NOTE: Ample sets, in this case, are closed under intersection,
                            // because A = U_{a in A} Ample(a) for every ample set A
                            // so A /\ B = U_{a in A /\ B} Ample(a) is an ample set.
                            // NOTE: restricted_ample is **not** an ample set,
                            // and ample is not necessarily the smallest ample set containing restricted_ample!
                            amples
                                .iter_mut()
                                .for_each(|(set, invalidate)| *invalidate |= set.contains(a));
                            // continue loop with same run and oracle
                            continue 'l;
                        } else {
                            // Branch satisfies all guarantees and can be discarded even though execution is incomplete
                            branches += 1;
                            recycling_sets.push(amples);
                            recycling_dags.push(dag);
                            // No transitions left in this branch so exit transitions iteration
                            // NOTE: break while loop needed to satisfy borrow checker
                            break 'w;
                        }
                    }
                }
            } else if run.cs.is_waiting(&bump) {
                run.time_tick();
                oracle.update_time(run.time());
                if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                    // Guarantee violated
                    branches += 1;
                    trace!(
                        "run violates a guarantee: processed {branches} branches in {:?}",
                        start_time.elapsed()
                    );
                    return Some(false);
                } else if oracle.output_guarantees().any(|b| b.is_none()) {
                    bump.reset();
                    run.fastforward(&bump);
                    amples
                        .iter_mut()
                        .for_each(|(_set, invalidate)| *invalidate = true);
                    // continue loop with same run and oracle
                    continue 'l;
                } else {
                    // Branch satisfies all guarantees and can be discarded even though execution is incomplete
                    branches += 1;
                    recycling_sets.push(amples);
                    recycling_dags.push(dag);
                }
            } else if oracle.final_output_guarantees().all(|b| b) {
                // Branch execution terminates and satisfies all guarantees and thus can be discarded
                branches += 1;
                recycling_sets.push(amples);
                recycling_dags.push(dag);
            } else {
                // Branch execution terminates without satisfying all guarantees
                branches += 1;
                trace!(
                    "run violates a guarantee: processed {branches} branches in {:?}",
                    start_time.elapsed()
                );
                return Some(false);
            }

            if let Some((branch_oracle, branch_run, branch_amples, branch_dag)) =
                executions_stack.pop()
            {
                // continue loop with next run and oracle
                run = branch_run;
                oracle = branch_oracle;
                dag = branch_dag;
                amples = branch_amples;
            } else {
                // Model execution terminates and satisfies all guarantees
                trace!(
                    "run verifies all guarantees: processed {branches} branches in {:?}",
                    start_time.elapsed()
                );
                return Some(true);
            }
        }
        trace!(
            "run stopped: processed {branches} branches in {:?}",
            start_time.elapsed()
        );
        None
    }

    // pub(crate) fn new_experiment<O: Oracle + Clone>(
    //     &self,
    //     mut oracle: O,
    //     running: Arc<AtomicBool>,
    // ) -> Option<bool> {
    //     let mut executions_stack: Vec<(O, TransitionSystemRun, usize)> = Vec::new();
    //     let mut state_trace: Vec<TransitionSystemRun> = Vec::new();
    //     let mut states = HashMap::new();
    //     let mut ample_sets_stack: Vec<(Vec<(FixedBitSet, bool)>, u16)> = Vec::new();
    //     let mut recycling_stack: Vec<Vec<(FixedBitSet, bool)>> = Vec::new();
    //     let pgs = self.pg_list.len();
    //     let mut amples = vec![(FixedBitSet::with_capacity(pgs), true); pgs];
    //     let mut ample_bkp = FixedBitSet::with_capacity(pgs);
    //     let mut bump = Bump::new();
    //     let mut run = self.new_run();
    //     // Initialize oracle with TS initial state
    //     oracle.update_state(&Vec::from_iter(run.labels()));
    //     run.fastforward(&bump);
    //     states.insert(run.clone(), 0);
    //     state_trace.push(run.clone());

    //     // FILO stack: depth-first search
    //     'l: while running.load(Ordering::Relaxed) {
    //         // States have to be synchronized with state trace
    //         assert_eq!(states.len(), state_trace.len());
    //         assert!(ample_sets_stack.len() <= executions_stack.len());
    //         assert!(executions_stack.len() <= state_trace.len());
    //         // Compute ample sets
    //         run.ample(&mut amples);
    //         if let Some((ample, _)) = amples
    //             .iter()
    //             .find(|(set, _)| set.count_ones(..) == 1)
    //             .or_else(|| {
    //                 amples
    //                     .iter()
    //                     .filter(|(set, _)| !set.is_clear())
    //                     .min_by_key(|(set, _)| set.count_ones(..))
    //             })
    //         {
    //             // Compute transitions restricted to ample set
    //             let mut ample_transitions = ample
    //                 .ones()
    //                 .flat_map(|b| {
    //                     run.cs
    //                         .nosync_possible_transitions_pg(PgId(b as u16))
    //                         .unwrap()
    //                         .flat_map(|(action, transitions)| {
    //                             transitions.map(move |post| (action, post))
    //                         })
    //                 })
    //                 .peekable();
    //             // There must be active transitions because we checked earlier
    //             assert!(ample_transitions.peek().is_some());
    //             let mut ample_branches: u16 = 0;
    //             while let Some((action, post)) = ample_transitions.next() {
    //                 if ample_transitions.peek().is_some() {
    //                     let mut branch_run = run.clone();
    //                     let mut branch_oracle = oracle.clone();
    //                     bump.reset();
    //                     branch_run
    //                         .transition(&mut branch_oracle, action, post, &bump)
    //                         .unwrap();
    //                     if !branch_oracle
    //                         .output_guarantees()
    //                         .all(|b| b.is_none_or(|b| b))
    //                     {
    //                         // Guarantee violated
    //                         trace!("run violates a guarantee before termination");
    //                         return Some(false);
    //                     } else if branch_oracle.output_guarantees().any(|b| b.is_none()) {
    //                         // Can do branch fast-forwarding here because properties are invariant under forwarding
    //                         bump.reset();
    //                         branch_run.fastforward(&bump);
    //                         if let Some(len) = states.get(&branch_run) {
    //                             // Found loop: what to do?
    //                             // if executions_stack
    //                             //     .last()
    //                             //     .is_some_and(|(_, _, branch_len)| branch_len >= len)
    //                             // {
    //                             //     // Loop can be exited,
    //                             //     // terminate branch exploration
    //                             // } else {
    //                             // Inescapable loop
    //                             trace!("(in)escapable loop found, run fails");
    //                             return Some(false);
    //                             // }
    //                         } else {
    //                             executions_stack.push((
    //                                 branch_oracle,
    //                                 branch_run,
    //                                 state_trace.len(),
    //                             ));
    //                             ample_branches += 1;
    //                         }
    //                     } else {
    //                         // All properties verified
    //                         // Terminate branch exploration
    //                         continue;
    //                     }
    //                 } else {
    //                     drop(ample_transitions);
    //                     bump.reset();
    //                     run.transition(&mut oracle, action, post, &bump).unwrap();
    //                     // TODO FIXME: is it possible to avoid clone? Is it expensive?
    //                     ample.clone_into(&mut ample_bkp);
    //                     amples.iter_mut().for_each(|(set, invalidate)| {
    //                         *invalidate = set.is_superset(&ample_bkp)
    //                     });
    //                     // Add ample set to stack
    //                     if ample_branches > 0 {
    //                         // If possible, recycle memory
    //                         if let Some(mut set) = recycling_stack.pop() {
    //                             // Clone ample set into recycled memory
    //                             set.iter_mut().zip(amples.iter()).for_each(
    //                                 |((to_set, to_invalid), (from_set, from_invalid))| {
    //                                     to_set.clone_from(from_set);
    //                                     *to_invalid = *from_invalid;
    //                                 },
    //                             );
    //                             ample_sets_stack.push((set, ample_branches));
    //                         } else {
    //                             ample_sets_stack.push((amples.clone(), ample_branches));
    //                         }
    //                         // trace!("branches: {ample_branches}");
    //                     }
    //                     if !oracle.output_guarantees().all(|b| b.is_none_or(|b| b)) {
    //                         // Guarantee violated
    //                         trace!("run violates a guarantee before termination");
    //                         return Some(false);
    //                     } else if oracle.output_guarantees().any(|b| b.is_none()) {
    //                         bump.reset();
    //                         run.fastforward(&bump);
    //                         if let Some(len) = states.get(&run) {
    //                             // Found loop: what to do?
    //                             // if executions_stack
    //                             //     .last()
    //                             //     .is_some_and(|(_, _, branch_len)| branch_len >= len)
    //                             // {
    //                             //     // Loop can be exited,
    //                             //     // terminate branch exploration
    //                             //     break 'w;
    //                             // } else {
    //                             // Inescapable loop
    //                             trace!("inescapable loop found, run fails");
    //                             // return Some(false);
    //                             panic!("inescapable loop found, run fails");
    //                             // }
    //                         } else {
    //                             // NOTE: First insert state with state trace len,
    //                             // then push to state trace, so indexes correspond.
    //                             assert!(states.insert(run.clone(), state_trace.len()).is_none());
    //                             state_trace.push(run.clone());
    //                             // continue loop with same run and oracle
    //                             continue 'l;
    //                         }
    //                     } else {
    //                         // All properties verified
    //                         // Terminate branch exploration
    //                         // No transitions left in this branch so exit transitions iteration
    //                         // NOTE: breaking needed to satisfy borrow checker
    //                         break;
    //                     }
    //                 }
    //             }
    //         } else if run.cs.is_waiting(&bump) {
    //             run.time_tick();
    //             oracle.update_time(run.time());
    //             if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
    //                 // Guarantee violated
    //                 trace!("execution branch violates a guarantee (partial execution)");
    //                 return Some(false);
    //             } else if oracle.output_guarantees().any(|b| b.is_none()) {
    //                 bump.reset();
    //                 run.fastforward(&bump);
    //                 assert!(
    //                     states.insert(run.clone(), state_trace.len()).is_none(),
    //                     "no state with the same time can already be in"
    //                 );
    //                 state_trace.push(run.clone());
    //                 amples
    //                     .iter_mut()
    //                     .for_each(|(_set, invalidate)| *invalidate = true);
    //                 // continue loop with same run and oracle
    //                 continue 'l;
    //             } else {
    //                 // Execution branch satisfies all guarantees
    //                 recycling_stack.push(amples);
    //             }
    //         } else if oracle.final_output_guarantees().any(|b| !b) {
    //             // Branch execution terminated
    //             assert_eq!(
    //                 run.cs
    //                     .nosync_possible_transitions()
    //                     .flat_map(
    //                         |(action, transitions)| transitions.map(move |post| (action, post))
    //                     )
    //                     .count(),
    //                 0
    //             );
    //             assert!(!run.cs.is_waiting(&bump));
    //             // Guarantee violated
    //             trace!("execution branch violates a guarantee (full execution)");
    //             return Some(false);
    //         } else {
    //             // Branch execution terminated
    //             // Execution branch satisfies all guarantees
    //             recycling_stack.push(amples);
    //         }

    //         if let Some((branch_oracle, branch_run, trace_len)) = executions_stack.pop() {
    //             trace!("branches: {}", executions_stack.len());
    //             assert!(trace_len <= state_trace.len());
    //             for state in state_trace.drain(trace_len..) {
    //                 // state must be present
    //                 assert!(states.remove(&state).is_some());
    //             }
    //             // continue loop with next run and oracle
    //             run = branch_run;
    //             oracle = branch_oracle;
    //             assert!(states.insert(run.clone(), state_trace.len()).is_none());
    //             state_trace.push(run.clone());
    //             // Recover ample set corresponding to new state
    //             if ample_sets_stack.last().unwrap().1 > 1 {
    //                 let (stack_amples, count) = ample_sets_stack.last_mut().unwrap();
    //                 *count -= 1;
    //                 // If possible, recycle memory
    //                 if let Some(mut set) = recycling_stack.pop() {
    //                     // Clone ample set into recycled memory
    //                     set.iter_mut().zip(stack_amples.iter()).for_each(
    //                         |((to_set, to_invalid), (from_set, from_invalid))| {
    //                             to_set.clone_from(from_set);
    //                             *to_invalid = *from_invalid;
    //                         },
    //                     );
    //                     amples = set;
    //                 } else {
    //                     amples = stack_amples.clone();
    //                 }
    //             } else {
    //                 amples = ample_sets_stack.pop().unwrap().0;
    //             }
    //         } else {
    //             // run terminated
    //             trace!("run verifies all guarantees");
    //             return Some(true);
    //         }
    //     }
    //     trace!("run stopped");
    //     None
    // }

    // #[inline]
    // pub fn is_stutter(&self, action: Action) -> bool {
    //     self.cs
    //         .communication(action)
    //         .is_none_or(|(c, _)| self.ports.binary_search(&c).is_err())
    // }

    // #[inline]
    // pub fn are_independent(&self, action_1: Action, action_2: Action) -> bool {
    //     action_1.0 != action_2.0
    //         && self.cs.communication(action_1).is_none_or(|(ch_1, _)| {
    //             self.cs
    //                 .communication(action_2)
    //                 .is_none_or(|(ch_2, _)| ch_1 != ch_2)
    //         })
    // }
}

/// Transition system model based on a [`ChannelSystem`].
///
/// It is essentially a CS which keeps track of the [`Event`]s produced by the execution
/// and determining a set of predicates.
#[derive(Debug, Clone)]
pub struct TransitionSystemRun<'def> {
    ts: &'def TransitionSystem,
    cs: ChannelSystemRun<'def>,
    vals: Vec<Vec<Val>>,
    last_event: Option<Channel>,
}

impl<'def> PartialEq for TransitionSystemRun<'def> {
    fn eq(&self, other: &Self) -> bool {
        self.cs == other.cs && self.vals == other.vals && self.last_event == other.last_event
    }
}

impl<'def> Eq for TransitionSystemRun<'def> {}

impl<'def> Hash for TransitionSystemRun<'def> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cs.hash(state);
        self.vals.hash(state);
        self.last_event.hash(state);
    }
}

impl<'def> TransitionSystemRun<'def> {
    // /// Perform a random transition.
    // ///
    // /// Used to generate Montecarlo-like executions
    // pub fn transition(&mut self) {
    //     self.last_event = self.montecarlo_transition();
    //     if let Some((_, ref event)) = self.last_event
    //         && let EventType::Send(ref vals) = event.event_type
    //         && let Ok(index) = self.ts.ports.binary_search(&event.channel)
    //     {
    //         // Since we have to update old values,
    //         // the vectors are already allocated and their is always the same.
    //         // Copying from slice should be faster than cloning.
    //         self.vals[index].copy_from_slice(vals);
    //     }
    // }

    // pub fn transition_pg(&mut self, pg_id: PgId) {
    //     self.last_event = self.montecarlo_transition_pg(pg_id);
    //     if let Some((_, ref event)) = self.last_event
    //         && let EventType::Send(ref vals) = event.event_type
    //         && let Ok(index) = self.ts.ports.binary_search(&event.channel)
    //     {
    //         // Since we have to update old values,
    //         // the vectors are already allocated and their is always the same.
    //         // Copying from slice should be faster than cloning.
    //         self.vals[index].copy_from_slice(vals);
    //     }
    // }

    /// Returns last event processed by model.
    #[inline]
    pub fn last_event(&self) -> Option<Channel> {
        self.last_event
    }

    #[inline]
    fn time(&self) -> Time {
        self.cs.time()
    }

    #[inline]
    fn time_tick(&mut self) {
        self.cs.wait(1).expect("time error")
    }

    fn labels(&self) -> impl Iterator<Item = bool> {
        self.ts.predicates.iter().map(|prop| {
            prop.eval::<SmallRng>(
                &|port| match port {
                    Atom::State(channel, idx) => {
                        let port_idx = self
                            .ts
                            .ports
                            .binary_search(&channel)
                            .expect("port must exist and be initialized");
                        self.vals[port_idx][idx]
                    }
                    Atom::Event(event_channel) => Val::Boolean(
                        self.last_event
                            .as_ref()
                            .is_some_and(|&channel| channel == event_channel),
                    ),
                },
                None,
            )
        })
    }

    #[inline]
    fn state(&self) -> &[Vec<Val>] {
        &self.vals
    }

    // /// Runs a single execution of the [`TransitionSystem`] with a given [`Oracle`] and returns a [`RunOutcome`].
    // pub(crate) fn experiment<O: Oracle>(
    //     &mut self,
    //     mut oracle: O,
    //     running: Arc<AtomicBool>,
    // ) -> RunOutcome {
    //     // reuse vector to avoid allocations
    //     let mut labels = Vec::from_iter(self.labels());
    //     // Initialize oracle with TS initial state
    //     oracle.update_state(&labels);
    //     while oracle.output_guarantees().any(|b| b.is_none()) {
    //         self.transition();
    //         if !running.load(Ordering::Relaxed) {
    //             trace!("run stopped");
    //             return None;
    //         } else if self.last_event().is_some() {
    //             labels.clear();
    //             labels.extend(self.labels());
    //             oracle.update_state(&labels);
    //         } else if self.cs.is_waiting() {
    //             self.time_tick();
    //             oracle.update_time(self.time());
    //         } else {
    //             break;
    //         }
    //     }
    //     trace!("run complete");
    //     let verified = Vec::from_iter(oracle.final_output_guarantees());
    //     Some(verified)
    // }

    pub(crate) fn transition<'a, O: Oracle>(
        &'a mut self,
        oracle: &mut O,
        action: Action,
        post: Location,
        bump: &'a Bump,
    ) -> Result<(), CsError> {
        let pg_id = action.0;
        if self
            .cs
            .program_graph(pg_id)
            .expect("pg exists")
            .current_states()
            .len()
            == 1
        {
            let last_event = self
                .cs
                .transition(action, &[post], bump)?
                .map(|event| (action, event));
            if let Some((_, ref event)) = last_event
                && let EventType::Send(ref vals) = event.event_type
                && let Ok(index) = self.ts.ports.binary_search(&event.channel)
            {
                // Since we have to update old values,
                // the vectors are already allocated and their is always the same.
                // Copying from slice should be faster than cloning.
                self.last_event = Some(event.channel);
                self.vals[index].copy_from_slice(vals);
                let labels = Vec::from_iter(self.labels());
                oracle.update_state(&labels);
                // trace!("event {action:?} {event:?}");
            } else {
                self.last_event = None;
            }
            Ok(())
        } else {
            unimplemented!()
        }
    }

    /// Runs a single execution of the [`TransitionSystem`] with a given [`Oracle`]
    /// and process the execution trace via the given [`Tracer`].
    pub(crate) fn trace<T, O: Oracle>(
        &mut self,
        mut oracle: O,
        mut tracer: T,
        model_data: &T::ModelData,
    ) -> RunOutcome
    where
        T: Tracer,
    {
        trace!("new run starting");
        // reuse vector to avoid allocations
        let mut labels = Vec::from_iter(self.labels());
        // Initialize oracle with TS initial state
        oracle.update_state(&labels);
        // WARN FIXME TODO: Initial state is not written as there is no corresponding action/event
        // Same issue for time-tick events
        while oracle.output_guarantees().any(|b| b.is_none()) {
            // self.transition();
            todo!();
            // if let Some((action, event)) = self.last_event() {
            //     tracer.trace(model_data, *action, event, self.time(), self.state());
            //     labels.clear();
            //     labels.extend(self.labels());
            //     oracle.update_state(&labels);
            // } else if self.cs.is_waiting() {
            //     self.time_tick();
            //     oracle.update_time(self.time());
            // } else {
            //     break;
            // }
        }
        trace!("run complete");
        let verified = Vec::from_iter(oracle.final_output_guarantees());
        Some(verified)
    }

    // fn montecarlo_transition(&mut self) -> Option<(Action, Event)> {
    //     // Setting pgs_left as length resets the queue
    //     let mut pgs_left = self.ts.pg_list.len();
    //     while pgs_left > 0 {
    //         // Select random pg within 0..pgs_left
    //         let pg_select = self.rng.random_range(0..pgs_left);
    //         let pg_id = self.ts.pg_list[pg_select];
    //         // Swap selected pg with last element of the queue (possibly itself, probably not worth checking)
    //         // Decrease the length of the queue (so that selected element is removed)
    //         pgs_left -= 1;
    //         self.ts.pg_list.swap(pg_select, pgs_left);
    //         // Execute randomly chosen transitions on the picked PG until an event is generated,
    //         // or no more transition is possible
    //         // NOTE: Special treatment for PGs with single-location state for optimization of this common case.
    //         // Hopefully it will be possible to treat all cases in a general way eventually.
    //         if let Some((action, event)) = self.montecarlo_transition_pg(pg_id) {
    //             return Some((action, event));
    //         }
    //     }
    //     None
    // }

    // fn montecarlo_transition_pg(&mut self, pg_id: PgId) -> Option<(Action, Event)> {
    //     let mut rng_extra = SmallRng::from_rng(&mut self.rng);
    //     // Execute randomly chosen transitions on the picked PG until an event is generated,
    //     // or no more transition is possible
    //     // NOTE: Special treatment for PGs with single-location state for optimization of this common case.
    //     // Hopefully it will be possible to treat all cases in a general way eventually.
    //     if self
    //         .cs
    //         .program_graph(pg_id)
    //         .expect("pg exists")
    //         .current_states()
    //         .len()
    //         == 1
    //     {
    //         while let Some((action, post_state)) = self
    //             .cs
    //             .nosync_possible_transitions_pg(pg_id)
    //             .expect("pg exists")
    //             .filter_map(|(action, post_states)| {
    //                 post_states.choose(&mut rng_extra).map(|loc| (action, loc))
    //             })
    //             .choose(&mut self.rng)
    //         {
    //             let event = self
    //                 .cs
    //                 .transition(action, &[post_state])
    //                 .expect("successful transition");
    //             if event.is_some() {
    //                 return event.map(|ev| (action, ev));
    //             }
    //         }
    //     } else {
    //         use bumpalo::collections::Vec as BumpVec;

    //         self.bump.reset();
    //         while let Some((action, post_states)) = self
    //             .cs
    //             .possible_transitions_pg(pg_id)
    //             .expect("pg exists")
    //             .filter_map(|(action, post_states)| {
    //                 post_states
    //                     .map(|locs| locs.choose(&mut rng_extra))
    //                     .collect_in::<Option<BumpVec<Location>>>(&self.bump)
    //                     // .collect::<Option<Vec<Location>>>()
    //                     .map(|locs| (action, locs))
    //             })
    //             .choose(&mut self.rng)
    //         {
    //             let event = self
    //                 .cs
    //                 .transition(action, post_states.as_slice())
    //                 .expect("successful transition");
    //             if event.is_some() {
    //                 return event.map(|ev| (action, ev));
    //             }
    //         }
    //     }
    //     None
    // }

    // fn por_transitions(mut self) -> Vec<TransitionSystemRun<'def>> {
    //     let mut transitions = Vec::new();
    //     for &pg_id in &self.ts.pg_list {
    //         let mut ts_run = self.clone();
    //         ts_run.transition_pg(pg_id);
    //         transitions.push((false, ts_run));
    //         self.bump.reset();
    //     }
    //     let mut ample = Vec::new();
    //     'search: for idx in 0..self.ts.pg_list.len() {
    //         let (b, tsr) = transitions.get_mut(idx).unwrap();
    //         if let Some((action, ref event)) = tsr.last_event
    //             && self.ts.is_stutter(action)
    //         {
    //             *b = true;
    //             ample.push(event.channel);
    //             while let Some(channel) = ample.pop() {
    //                 for (b, tsr) in &mut transitions {
    //                     if !*b && let Some((action, ref event)) = tsr.last_event {
    //                         let pg_id = action.0;
    //                         if self.ts.cs.communicates_to(pg_id, channel) {
    //                             if self.ts.is_stutter(action) {
    //                                 *b = true;
    //                                 let channel = event.channel;
    //                                 ample.push(channel);
    //                             } else {
    //                                 transitions.iter_mut().for_each(|(b, ..)| *b = false);
    //                                 continue 'search;
    //                             }
    //                         }
    //                     }
    //                 }
    //                 ample.sort_unstable();
    //                 ample.dedup();
    //             }
    //             break 'search;
    //         }
    //     }

    //     if transitions.iter().any(|(b, ..)| *b) {
    //         transitions
    //             .into_iter()
    //             .filter(|&(b, ..)| b)
    //             .map(|(.., tsr)| tsr)
    //             .collect()
    //     } else {
    //         transitions.into_iter().map(|(.., tsr)| tsr).collect()
    //     }
    // }

    fn ample(&self, amples: &mut [(FixedBitSet, bool)]) {
        self.ts
            .pg_list
            .iter()
            .zip(amples.iter_mut())
            .filter(|(_pg_id, (_set, invalidate))| *invalidate)
            .for_each(|(pg_id, (set, _))| self.ample_pg(*pg_id, set));

        for i in 0..amples.len() {
            // skip j == i
            for j in 0..amples.len() {
                // only consider invalid j_set's
                if i != j
                    && let Ok([(i_set, _), (j_set, true)]) = amples.get_disjoint_mut([i, j])
                    && j_set.contains(i)
                {
                    let j_bit = j_set[j];
                    j_set.set(i, false);
                    j_set.union_with(i_set);
                    j_set.set(j, j_bit);
                }
            }
        }

        amples
            .iter_mut()
            .for_each(|(_, invalidate)| *invalidate = false);
    }

    fn ample_pg(&self, pg_id: PgId, ample_pg: &mut FixedBitSet) {
        ample_pg.clear();
        let mut ample_self = false;
        for action in self
            .cs
            .program_graph(pg_id)
            .unwrap()
            .nosync_active_actions()
            .unwrap()
        {
            let action = Action(pg_id, action);
            if let Some((channel, message)) = self.ts.cs.communication(action) {
                if self.cs.check_message(channel, message) {
                    ample_self = true;
                    match message {
                        Message::Send if self.ts.ports.contains(&channel) => {
                            // Channel is a port so action is not stutter
                            ample_pg.insert_range(..);
                            break;
                        }
                        Message::Send => {
                            ample_pg.union_with(self.ts.cs.senders_to_set(channel).unwrap());
                            // NOTE: If an execution fragment has a ProbeEmptyQueue action on a channel that is not initially empty,
                            // then it must be preceded by a Receive action;
                            // so, if the channel is not empty and Receive actions belong to the ample set, ProbeEmptyQueue does not need to belong to the ample set.
                            // Otherwise, we add ProbeEmptyQueue actions to the ample set.
                            if self.cs.is_empty(channel)
                                || !ample_pg
                                    .is_superset(self.ts.cs.receivers_from_set(channel).unwrap())
                            {
                                ample_pg
                                    .union_with(self.ts.cs.probe_empty_queue_set(channel).unwrap());
                            }
                        }
                        Message::ProbeEmptyQueue => {
                            // Channel is empty
                            ample_pg.union_with(self.ts.cs.senders_to_set(channel).unwrap());
                        }
                        Message::Receive => {
                            // Channel is not empty
                            ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                        }
                        Message::ProbeFullQueue => todo!(),
                    }
                } else {
                    // non-active action is not added to ample set,
                    // but ample set must contain all actions which could potentially activate it.
                    // Probes (empty/full-queue) never need to be added
                    match message {
                        Message::Send => {
                            ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                            unreachable!("send actions are always possible");
                        }
                        Message::ProbeEmptyQueue => {
                            // Channel is not empty
                            ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                        }
                        Message::Receive => {
                            // Channel is empty
                            ample_pg.union_with(self.ts.cs.senders_to_set(channel).unwrap());
                        }
                        Message::ProbeFullQueue => todo!(),
                    }
                }
            } else {
                ample_self = true;
            }
        }
        ample_pg.set(u16::from(pg_id) as usize, ample_self);
    }

    #[inline]
    fn fastforward<'a>(&'a mut self, bump: &'a Bump) {
        for &pg_id in &self.ts.pg_list {
            self.fastforward_pg(pg_id, bump);
        }
    }

    // Resolves tsr steps made of non-communication transition and pushes them to the stack
    fn fastforward_pg<'a>(&'a mut self, pg_id: PgId, bump: &'a Bump) {
        loop {
            let choice;
            {
                let mut transitions = self
                    .cs
                    .program_graph(pg_id)
                    .unwrap()
                    .nosync_possible_transitions()
                    .unwrap()
                    .flat_map(|(action, transitions)| {
                        transitions.map(move |post| (Action(pg_id, action), Location(pg_id, post)))
                    });
                if let Some((action, post)) = transitions.next()
                    && self.ts.cs.communication(action).is_none()
                    && transitions.next().is_none()
                {
                    choice = Some((action, post));
                } else {
                    choice = None;
                }
            }
            if let Some((action, post)) = choice {
                self.cs
                    .transition(action, &[post], bump)
                    .expect("transition must succeed");
            } else {
                break;
            }
        }
    }
}
