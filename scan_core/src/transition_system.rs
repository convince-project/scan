use std::mem;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use bumpalo::Bump;
use bumpalo::collections::CollectIn;
use fixedbitset::FixedBitSet;
use itertools::Itertools;
use log::{info, trace};
use rand::rngs::SmallRng;
use thiserror::Error;

use crate::channel_system::{
    Action, Channel, ChannelSystem, ChannelSystemRun, CsError, EventType, Location, Message, PgId,
};
use crate::{BooleanExpr, Oracle, RunOutcome, Time, Tracer, Val};

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
        let mut executions_stack: Vec<(O, TransitionSystemRun)> = Vec::new();
        let mut bump = Bump::new();
        let mut run = self.new_run();
        let labels = Vec::from_iter(run.labels());
        // Initialize oracle with TS initial state
        oracle.update_state(&labels);
        run.fastforward(&bump);
        let mut branches: u32 = 0;

        let mut pgs: FixedBitSet = FixedBitSet::with_capacity(self.pg_list.len());
        let mut channels_senders: FixedBitSet =
            FixedBitSet::with_capacity(self.cs.channels().len());
        let mut channels_receivers: FixedBitSet =
            FixedBitSet::with_capacity(self.cs.channels().len());
        let mut probe_empty_queues: FixedBitSet =
            FixedBitSet::with_capacity(self.cs.channels().len());

        // FILO stack: depth-first search
        'l: loop {
            if !running.load(Ordering::Relaxed) {
                trace!("run stopped");
                return None;
            }
            bump.reset();
            if run
                .cs
                .nosync_possible_transitions()
                .flat_map(|(_, transitions)| transitions)
                .next()
                .is_some()
            {
                run.ample(
                    &mut pgs,
                    &mut channels_senders,
                    &mut channels_receivers,
                    &mut probe_empty_queues,
                );
                // assert!(ample.is_none_or(|ample| !ample.is_clear()));
                let mut ample_transitions = self
                    .pg_list
                    .iter()
                    .filter(|&&pg_id| pgs.contains(u16::from(pg_id) as usize))
                    .flat_map(|&pg_id| {
                        run.cs
                            .nosync_possible_transitions_pg(pg_id)
                            .unwrap()
                            .flat_map(|(action, transitions)| {
                                transitions.map(move |post| (action, post))
                            })
                    })
                    .peekable();
                // There must be active transitions because we checked earlier
                // assert!(ample_transitions.peek().is_some());
                'w: while let Some((action, post)) = ample_transitions.next() {
                    if ample_transitions.peek().is_none() {
                        drop(ample_transitions);
                        bump.reset();
                        run.transition(&mut oracle, action, post, &bump).unwrap();
                        if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                            // Guarantee violated
                            trace!("run violates a guarantee");
                            return Some(false);
                        } else if oracle.output_guarantees().any(|b| b.is_none()) {
                            bump.reset();
                            run.fastforward(&bump);
                            // continue loop with same run and oracle
                            continue 'l;
                        } else {
                            branches += 1;
                            if branches.is_power_of_two() {
                                trace!("executed branches: {branches}");
                            }
                            // No transitions left in this branch so exit transitions iteration
                            break 'w;
                        }
                    } else {
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
                            trace!("run violates a guarantee");
                            return Some(false);
                        } else if branch_oracle.output_guarantees().any(|b| b.is_none()) {
                            bump.reset();
                            branch_run.fastforward(&bump);
                            executions_stack.push((branch_oracle, branch_run));
                        } else {
                            branches += 1;
                            if branches.is_power_of_two() {
                                trace!("executed branches: {branches}");
                            }
                        }
                    }
                }
            } else if run.cs.is_waiting(&bump) {
                run.time_tick();
                oracle.update_time(run.time());
                trace!("time: {}", run.time());
                if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                    // Guarantee violated
                    trace!("execution branch violates a guarantee (partial execution)");
                    return Some(false);
                } else if oracle.output_guarantees().any(|b| b.is_none()) {
                    bump.reset();
                    run.fastforward(&bump);
                    // continue loop with same run and oracle
                    continue 'l;
                } else {
                    branches += 1;
                    if branches.is_power_of_two() {
                        trace!("executed branches: {branches}");
                    }
                }
            } else {
                branches += 1;
                if branches.is_power_of_two() {
                    trace!("executed branches: {branches}");
                }
            }

            if let Some((branch_oracle, branch_run)) = executions_stack.pop() {
                // continue loop with next run and oracle
                run = branch_run;
                oracle = branch_oracle;
            } else {
                // run terminated
                trace!("run verifies all guarantees");
                return Some(true);
            }
        }
    }

    pub(crate) fn fast_experiment<O: Oracle + Clone>(
        &self,
        mut oracle: O,
        running: Arc<AtomicBool>,
    ) -> Option<bool> {
        let mut executions_stack: Vec<(O, TransitionSystemRun)> = Vec::new();
        let mut bump = Bump::new();
        let mut run = self.new_run();
        let labels = Vec::from_iter(run.labels());
        // Initialize oracle with TS initial state
        oracle.update_state(&labels);
        run.fastforward(&bump);
        let mut branches: u32 = 0;

        // FILO stack: depth-first search
        'l: loop {
            if !running.load(Ordering::Relaxed) {
                trace!("run stopped");
                return None;
            }
            bump.reset();
            if run
                .cs
                .nosync_possible_transitions()
                .flat_map(|(_, transitions)| transitions)
                .next()
                .is_some()
            {
                // assert!(!amples.is_empty());
                let mut ample_transitions = run
                    .fast_ample(&bump)
                    .map(|ample| {
                        self.pg_list
                            .iter()
                            .filter(|&&pg_id| ample.contains(u16::from(pg_id) as usize))
                            .flat_map(|&pg_id| {
                                run.cs
                                    .nosync_possible_transitions_pg(pg_id)
                                    .unwrap()
                                    .flat_map(|(action, transitions)| {
                                        transitions.map(move |post| (action, post))
                                    })
                            })
                            .collect::<Vec<_>>()
                    })
                    .multi_cartesian_product()
                    .peekable();
                assert!(
                    ample_transitions.peek().is_some(),
                    "ample: {ample_transitions:?}"
                );
                // There must be active transitions because we checked earlier
                // assert!(ample_transitions.peek().is_some());
                'w: while let Some(transitions) = ample_transitions.next() {
                    if ample_transitions.peek().is_none() {
                        drop(ample_transitions);
                        for (action, post) in transitions {
                            bump.reset();
                            run.transition(&mut oracle, action, post, &bump).unwrap();
                        }
                        if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                            // Guarantee violated
                            trace!("run violates a guarantee");
                            return Some(false);
                        } else if oracle.output_guarantees().any(|b| b.is_none()) {
                            bump.reset();
                            run.fastforward(&bump);
                            // continue loop with same run and oracle
                            continue 'l;
                        } else {
                            branches += 1;
                            if branches.is_power_of_two() {
                                trace!("executed branches: {branches}");
                            }
                            // No transitions left in this branch so exit transitions iteration
                            break 'w;
                        }
                    } else {
                        let mut branch_run = run.clone();
                        let mut branch_oracle = oracle.clone();
                        for (action, post) in transitions {
                            bump.reset();
                            branch_run
                                .transition(&mut branch_oracle, action, post, &bump)
                                .unwrap();
                        }
                        if branch_oracle
                            .output_guarantees()
                            .any(|b| b.is_some_and(|b| !b))
                        {
                            // Guarantee violated
                            trace!("run violates a guarantee");
                            return Some(false);
                        } else if branch_oracle.output_guarantees().any(|b| b.is_none()) {
                            bump.reset();
                            branch_run.fastforward(&bump);
                            executions_stack.push((branch_oracle, branch_run));
                        } else {
                            branches += 1;
                            if branches.is_power_of_two() {
                                trace!("executed branches: {branches}");
                            }
                        }
                    }
                }
            } else if run.cs.is_waiting(&bump) {
                run.time_tick();
                oracle.update_time(run.time());
                trace!("time: {}", run.time());
                if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                    // Guarantee violated
                    trace!("execution branch violates a guarantee (partial execution)");
                    return Some(false);
                } else if oracle.output_guarantees().any(|b| b.is_none()) {
                    bump.reset();
                    run.fastforward(&bump);
                    // continue loop with same run and oracle
                    continue 'l;
                } else {
                    branches += 1;
                    if branches.is_power_of_two() {
                        trace!("executed branches: {branches}");
                    }
                }
            } else {
                branches += 1;
                if branches.is_power_of_two() {
                    trace!("executed branches: {branches}");
                }
            }

            if let Some((branch_oracle, branch_run)) = executions_stack.pop() {
                // continue loop with next run and oracle
                run = branch_run;
                oracle = branch_oracle;
            } else {
                // run terminated
                trace!("run verifies all guarantees");
                return Some(true);
            }
        }
    }

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

    fn ample(
        &self,
        pgs: &mut FixedBitSet,
        channels_senders: &mut FixedBitSet,
        channels_receivers: &mut FixedBitSet,
        probe_empty_queues: &mut FixedBitSet,
    ) {
        pgs.insert_range(..);
        let mut ample_len = pgs.count_ones(..);
        assert_eq!(ample_len, self.ts.pg_list.len());
        let mut ample = FixedBitSet::with_capacity(self.ts.pg_list.len());
        // Consider only PGs that have active transitions
        for pg_id in self.ts.pg_list.iter().copied().filter(|&pg_id| {
            self.cs
                .nosync_possible_transitions_pg(pg_id)
                .unwrap()
                .flat_map(|(_, transitions)| transitions)
                .next()
                .is_some()
        }) {
            ample.clear();
            channels_senders.clear();
            channels_receivers.clear();
            probe_empty_queues.clear();
            ample.insert(u16::from(pg_id) as usize);
            if self
                .add_pg_to_ample(
                    pg_id,
                    &mut ample,
                    channels_senders,
                    channels_receivers,
                    probe_empty_queues,
                )
                .is_ok()
                && ample.count_ones(..) < ample_len
            {
                ample_len = ample.count_ones(..);
                mem::swap(pgs, &mut ample);
                if ample_len == 1 {
                    return;
                }
            }
        }
        // pgs might include PG's that have no active transitions but that does not invalidate the result
    }

    fn fast_ample<'a>(&self, bump: &'a Bump) -> impl Iterator<Item = &'a FixedBitSet> {
        let mut amples: bumpalo::collections::Vec<_> = self
            .ts
            .pg_list
            .iter()
            .map(|&pg_id| self.fast_ample_pg(pg_id))
            .collect_in(bump);
        let diag = FixedBitSet::from_iter(
            amples
                .iter()
                .enumerate()
                .filter(|&(i, set_i)| set_i.contains(i))
                .map(|(i, _)| i),
        );
        for i in 0..amples.len() {
            let i_set = amples[i].clone();
            for j_set in amples.iter_mut().filter(|j_set| j_set.contains(i)) {
                j_set.remove(i);
                j_set.union_with(&i_set);
            }
        }
        amples.iter_mut().for_each(|set| set.intersect_with(&diag));
        amples.retain(|set| !set.is_clear());
        amples.sort_unstable_by_key(|set| set.count_ones(..));
        let amples = amples.into_bump_slice();
        amples
            .iter()
            .enumerate()
            .filter(|(i, set)| amples[..*i].iter().all(|prev| set.is_disjoint(prev)))
            .map(|(_, set)| set)
    }

    fn fast_ample_pg(&self, pg_id: PgId) -> FixedBitSet {
        let mut ample_pg = FixedBitSet::with_capacity(self.ts.pg_list.len());
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
                    if matches!(message, Message::Send) && self.ts.ports.contains(&channel) {
                        // Channel is a port so action is not stutter
                        ample_pg.insert_range(..);
                        break;
                    }
                    // If channel is empty, no message can be received until a message is sent to the channel first
                    // NOTE: if channel is empty and action is active then action must be a `Send`
                    if !self.cs.is_empty(channel) {
                        ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                    }
                    // NOTE: Probings are independent from one another so there is no need to add other probings.
                    // NOTE: If an execution branch has a ProbeEmptyQueue action on a channel that is not currently empty,
                    // then it must be preceded by a Receive action, which is in the ample set;
                    // so, if the channel is not empty, ProbeEmptyQueue does not need to belong to the ample set.
                    // If the channel is empty, instead, we add ProbeEmptyQueue actions to the ample set.
                    if !matches!(message, Message::ProbeEmptyQueue | Message::ProbeFullQueue)
                        && self.cs.is_empty(channel)
                    {
                        ample_pg.union_with(self.ts.cs.probe_empty_queue_set(channel).unwrap());
                    }
                    // TODO: add ProbeFullQueue
                    // TODO: add full-queue condition
                    ample_pg.union_with(self.ts.cs.senders_to_set(channel).unwrap());
                } else {
                    // non-active action is not added to ample set,
                    // but ample set must contain all actions which could potentially activate it.
                    // Probes (empty/full-queue) never need to be added
                    match message {
                        // Channel is a port so action is not stutter
                        Message::Send if self.ts.ports.contains(&channel) => {
                            ample_pg.insert_range(..);
                            break;
                        }
                        Message::Send => {
                            ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                        }
                        Message::ProbeEmptyQueue => {
                            ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                        }
                        Message::Receive => {
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
        ample_pg
    }

    fn add_pg_to_ample(
        &self,
        pg_id: PgId,
        pgs: &mut FixedBitSet,
        channels_senders: &mut FixedBitSet,
        channels_receivers: &mut FixedBitSet,
        probe_empty_queues: &mut FixedBitSet,
    ) -> Result<(), ()> {
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
                    if matches!(message, Message::Send) && self.ts.ports.contains(&channel) {
                        // Channel is a port so action is not stutter
                        return Err(());
                    }
                    // If channel is empty, no message can be received until a message is sent to the channel first
                    // NOTE: if channel is empty and action is active then action must be a `Send` or `ProbeEmptyQueue`
                    if !self.cs.is_empty(channel)
                        // WARN: last condition has effects!
                        && !channels_receivers.put(u16::from(channel) as usize)
                    {
                        self.add_receivers_to_ample(
                            channel,
                            pgs,
                            channels_senders,
                            channels_receivers,
                            probe_empty_queues,
                        )?;
                    }
                    // NOTE: Probings are independent from one another so there is no need to add other probings.
                    // NOTE: If an execution branch has a ProbeEmptyQueue action on a channel that is not currently empty,
                    // then it must be preceded by a Receive action, which is in the ample set;
                    // so, if the channel is not empty, ProbeEmptyQueue does not need to belong to the ample set.
                    // If the channel is empty, instead, we add ProbeEmptyQueue actions to the ample set.
                    if !matches!(message, Message::ProbeEmptyQueue | Message::ProbeFullQueue) && self.cs.is_empty(channel)
                        // WARN: last condition has effects!
                        && !probe_empty_queues.put(u16::from(channel) as usize)
                    {
                        self.add_empty_queues_to_ample(
                            channel,
                            pgs,
                            channels_senders,
                            channels_receivers,
                            probe_empty_queues,
                        )?;
                    }
                    // TODO: add ProbeFullQueue
                    // TODO: add full-queue condition
                    if !channels_senders.put(u16::from(channel) as usize) {
                        self.add_senders_to_ample(
                            channel,
                            // ample,
                            pgs,
                            channels_senders,
                            channels_receivers,
                            probe_empty_queues,
                        )?;
                    }
                } else {
                    // non-active action is not added to ample set,
                    // but ample set must contain all actions which could potentially activate it.
                    // Probes (empty/full-queue) never need to be added
                    match message {
                        // Channel is a port so action is not stutter
                        Message::Send => unreachable!("Send is always possible"),
                        // Message::Send if self.ts.ports.contains(&channel) => return Err(()),
                        // Message::Send => {
                        //     if !channels_receivers.put(u16::from(channel) as usize) {
                        //         self.add_receivers_to_ample(
                        //             channel,
                        //             pgs,
                        //             channels_senders,
                        //             channels_receivers,
                        //             probe_empty_queues,
                        //         )?;
                        //     }
                        // }
                        Message::ProbeEmptyQueue => {
                            // NOTE: channel must be non-empty
                            if !channels_receivers.put(u16::from(channel) as usize) {
                                self.add_receivers_to_ample(
                                    channel,
                                    pgs,
                                    channels_senders,
                                    channels_receivers,
                                    probe_empty_queues,
                                )?;
                            }
                        }
                        Message::Receive => {
                            // NOTE: channel must be empty
                            if !channels_senders.put(u16::from(channel) as usize) {
                                self.add_senders_to_ample(
                                    channel,
                                    pgs,
                                    channels_senders,
                                    channels_receivers,
                                    probe_empty_queues,
                                )?;
                            }
                        }
                        Message::ProbeFullQueue => todo!(),
                    }
                }
            }
        }
        Ok(())
    }

    #[inline]
    fn add_senders_to_ample(
        &self,
        channel: Channel,
        pgs: &mut FixedBitSet,
        channels_senders: &mut FixedBitSet,
        channels_receivers: &mut FixedBitSet,
        probe_empty_queues: &mut FixedBitSet,
    ) -> Result<(), ()> {
        for pg_id in self.ts.cs.senders_to(channel).unwrap() {
            if !pgs.put(u16::from(pg_id) as usize) {
                self.add_pg_to_ample(
                    pg_id,
                    pgs,
                    channels_senders,
                    channels_receivers,
                    probe_empty_queues,
                )?;
            }
        }
        Ok(())
    }

    #[inline]
    fn add_receivers_to_ample(
        &self,
        channel: Channel,
        pgs: &mut FixedBitSet,
        channels_senders: &mut FixedBitSet,
        channels_receivers: &mut FixedBitSet,
        probe_empty_queues: &mut FixedBitSet,
    ) -> Result<(), ()> {
        for pg_id in self.ts.cs.receivers_from(channel).unwrap() {
            if !pgs.put(u16::from(pg_id) as usize) {
                self.add_pg_to_ample(
                    pg_id,
                    pgs,
                    channels_senders,
                    channels_receivers,
                    probe_empty_queues,
                )?;
            }
        }
        Ok(())
    }

    #[inline]
    fn add_empty_queues_to_ample(
        &self,
        channel: Channel,
        pgs: &mut FixedBitSet,
        channels_senders: &mut FixedBitSet,
        channels_receivers: &mut FixedBitSet,
        probe_empty_queues: &mut FixedBitSet,
    ) -> Result<(), ()> {
        for pg_id in self.ts.cs.probe_empty_queue(channel).unwrap() {
            if !pgs.put(u16::from(pg_id) as usize) {
                self.add_pg_to_ample(
                    pg_id,
                    pgs,
                    channels_senders,
                    channels_receivers,
                    probe_empty_queues,
                )?;
            }
        }
        Ok(())
    }

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
