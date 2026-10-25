mod run;

use std::mem;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Instant;

use bumpalo::Bump;
use fixedbitset::FixedBitSet;
use itertools::Itertools;
use log::{info, trace};
use petgraph::acyclic::Acyclic;
use petgraph::data::Build;
use petgraph::graph::DiGraph;
use rand::make_rng;
use rand::rngs::SmallRng;
use thiserror::Error;

use crate::channel_system::{
    Action, Channel, ChannelSystem, CsError, Event, EventType, PgId, PgIndex,
};
use crate::{BooleanExpr, Oracle, RunOutcome, Scheduler, Time, Tracer, Val};
use run::TransitionSystemRun;

type Dag = Acyclic<DiGraph<(), (), PgIndex>>;

/// A Channel System event related to a channel.
#[derive(Debug, Clone, PartialEq)]
pub struct OwnedEvent {
    /// The PG producing the event in the course of a transition.
    pub pg_id: PgId,
    /// The channel involved in the event.
    pub channel: Channel,
    /// The type of event produced.
    pub event_type: OwnedEventType,
}

impl From<Event<'_>> for OwnedEvent {
    #[inline]
    fn from(value: Event<'_>) -> Self {
        OwnedEvent {
            pg_id: value.pg_id,
            channel: value.channel,
            event_type: value.event_type.into(),
        }
    }
}

impl<'a> From<&'a OwnedEvent> for Event<'a> {
    #[inline]
    fn from(value: &'a OwnedEvent) -> Self {
        Event {
            pg_id: value.pg_id,
            channel: value.channel,
            event_type: (&value.event_type).into(),
        }
    }
}

/// A Channel System event type related to a channel.
#[derive(Debug, Clone, PartialEq)]
pub enum OwnedEventType {
    /// Sending a value to a channel.
    Send(Box<[Val]>),
    /// Retrieving a value out of a channel.
    Receive(Box<[Val]>),
    /// Checking whether a channel is empty.
    ProbeEmptyQueue,
    /// Checking whether a channel is full.
    ProbeFullQueue,
}

impl<'a> From<&'a OwnedEventType> for EventType<'a> {
    #[inline]
    fn from(value: &'a OwnedEventType) -> Self {
        match value {
            OwnedEventType::Send(vals) => EventType::Send(vals.as_ref()),
            OwnedEventType::Receive(vals) => EventType::Receive(vals.as_ref()),
            OwnedEventType::ProbeEmptyQueue => EventType::ProbeEmptyQueue,
            OwnedEventType::ProbeFullQueue => EventType::ProbeFullQueue,
        }
    }
}

impl From<EventType<'_>> for OwnedEventType {
    #[inline]
    fn from(value: EventType<'_>) -> Self {
        match value {
            EventType::Send(vals) => OwnedEventType::Send(vals.to_owned().into_boxed_slice()),
            EventType::Receive(vals) => OwnedEventType::Receive(vals.to_owned().into_boxed_slice()),
            EventType::ProbeEmptyQueue => OwnedEventType::ProbeEmptyQueue,
            EventType::ProbeFullQueue => OwnedEventType::ProbeFullQueue,
        }
    }
}

type Trace<'a> = Vec<(Time, Action, OwnedEvent, Box<[Val]>)>;

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
    #[inline]
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

    fn port_idxs(&self) -> Box<[usize]> {
        [0].into_iter()
            .chain(self.vals.iter().scan(0, |counter, val| {
                *counter += val.len();
                Some(*counter)
            }))
            .collect()
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
    fn new_run(&self) -> TransitionSystemRun<'_> {
        info!(
            "create new transition system run with {} ports and {} predicates",
            self.ports.len(),
            self.predicates.len(),
        );
        TransitionSystemRun::new(self)
    }

    pub(crate) fn experiment<'a, O: Oracle + Clone, T: Tracer<'a>>(
        &self,
        oracle: O,
        tracer: Option<T>,
        running: Arc<AtomicBool>,
        scheduler: Scheduler,
    ) -> RunOutcome {
        // NOTE: Uniform scheduler writes the trace on-the-fly,
        // whether or not it violates the guarantees,
        // while Exhaustive and Priority schedulers return a complete trace that violates the guarantees,
        // or nothing if all guarantees where satisfied by all considered schedulers.
        match scheduler {
            Scheduler::Uniform => self.experiment_sample(oracle, tracer, running),
            // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
            Scheduler::Sampling => todo!(),
            Scheduler::Exhaustive => self.experiment_exhaustive(oracle, tracer, running),
            Scheduler::Priority => self.experiment_priority(oracle, tracer, running),
        }
    }

    /// Runs a single execution of the [`TransitionSystem`] with a given [`Oracle`] and returns a [`RunOutcome`].
    /// If available, process the execution trace via the given [`Tracer`].
    fn experiment_sample<'a, O: Oracle, T: Tracer<'a>>(
        &self,
        mut oracle: O,
        mut tracer: Option<T>,
        running: Arc<AtomicBool>,
    ) -> RunOutcome {
        let mut bump = Bump::new();
        let mut rng: SmallRng = make_rng();
        let mut pg_list = self.pg_list.clone();
        let mut run = self.new_run();
        run.fastforward(&bump);

        // Initialize oracle with TS initial state
        let labels = Vec::from_iter(run.labels());
        oracle.update_state(&labels);

        while oracle.output_guarantees().any(|b| b.is_none()) {
            bump.reset();
            if !running.load(Ordering::Relaxed) {
                trace!("run stopped");
                return None;
            } else if let Some((action, event)) =
                run.montecarlo_transition(&mut oracle, &mut pg_list, &mut rng, &bump)
            {
                if let Some(ref mut t) = tracer {
                    t.trace(action, event, run.time(), &|channel| run.state(channel));
                }
            } else if run.is_waiting() {
                run.time_tick();
                oracle.update_time(run.time());
            } else {
                break;
            }
        }
        trace!("run complete");
        Some(Vec::from_iter(oracle.final_output_guarantees()))
    }

    fn record_trace<'a, T: Tracer<'a>>(&self, mut tracer: T, trace: Trace) {
        let ports_idxs = self.port_idxs();
        for (time, action, event, state) in trace {
            tracer.trace(action, (&event).into(), time, &|channel| {
                self.ports
                    .binary_search(&channel)
                    .ok()
                    .map(|idx| &state[ports_idxs[idx]..ports_idxs[idx + 1]])
            });
        }
    }

    fn experiment_exhaustive<'a, O: Oracle + Clone, T: Tracer<'a>>(
        &self,
        mut oracle: O,
        tracer: Option<T>,
        running: Arc<AtomicBool>,
    ) -> RunOutcome {
        // Diagnostics/stats
        let start_time = Instant::now();
        let mut branches: u32 = 0;
        let oracle = &mut oracle;

        type BranchData<'a, O> = (
            O,
            TransitionSystemRun<'a>,
            Box<[(FixedBitSet, bool)]>,
            usize,
        );
        let mut executions_stack: Vec<BranchData<O>> = Vec::new();
        // TODO: measure impact of recycling memory to check wether it is worth it
        let mut recycling_sets: Vec<Box<[(FixedBitSet, bool)]>> = Vec::new();
        let pgs = self.pg_list.len();
        let mut amples = vec![(FixedBitSet::with_capacity(pgs), true); pgs].into_boxed_slice();
        let mut bump = Bump::new();
        let mut run = self.new_run();
        let mut trace = Vec::new();

        // Initialize oracle with TS initial state
        let state: Box<[bool]> = run.labels().collect();
        oracle.update_state(&state);
        run.fastforward(&bump);

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
            }

            // Find most suitable ample set
            if let Some((ample, _)) = amples
                .iter()
                // only consider viable sets (i.e., neither invalidated nor empty)
                .filter(|(set, invalid)| !(*invalid || set.is_clear()))
                .min_by_key(|(set, _)| set.len())
            {
                let mut transitions = ample
                    .ones()
                    .flat_map(|b| {
                        run.possible_transitions_pg(PgId(b as PgIndex))
                            .unwrap()
                            .flat_map(|(action, posts)| {
                                posts
                                    .multi_cartesian_product()
                                    .map(move |posts| (action, posts))
                            })
                    })
                    .peekable();
                // There must be active transitions because we checked earlier
                assert!(transitions.peek().is_some());
                'w: while let Some((action, posts)) = transitions.next() {
                    if transitions.peek().is_some() {
                        let mut branch_run = run.clone();
                        let mut branch_oracle = oracle.clone();
                        bump.reset();
                        branch_run
                            .transition(&mut branch_oracle, action, &posts, &bump)
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
                            if let Some(tracer) = tracer {
                                self.record_trace(tracer, trace);
                            }
                            return Some(Vec::from_iter(
                                branch_oracle
                                    .output_guarantees()
                                    .map(|b| b.is_none_or(|b| b)),
                            ));
                        } else if branch_oracle.output_guarantees().any(|b| b.is_none()) {
                            bump.reset();
                            branch_run.fastforward(&bump);
                            let a = u16::from(action.program_gramph_id()) as usize;
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
                                trace.len(),
                            ));
                        } else {
                            // Branch satisfies all guarantees and can be discarded even though execution is incomplete
                            branches += 1;
                        }
                    } else {
                        drop(transitions);
                        bump.reset();
                        let event = run.transition(oracle, action, &posts, &bump).unwrap();
                        if tracer.is_some()
                            && let Some(event) = event
                        {
                            trace.push((
                                run.time(),
                                action,
                                event.into(),
                                run.whole_state().into(),
                            ));
                        }
                        if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                            // Guarantee violated
                            branches += 1;
                            trace!(
                                "run violates a guarantee: processed {branches} branches in {:?}",
                                start_time.elapsed()
                            );
                            if let Some(tracer) = tracer {
                                self.record_trace(tracer, trace);
                            }
                            return Some(Vec::from_iter(
                                oracle.output_guarantees().map(|b| b.is_none_or(|b| b)),
                            ));
                        } else if oracle.output_guarantees().any(|b| b.is_none()) {
                            bump.reset();
                            run.fastforward(&bump);
                            // Update DAG
                            let a = u16::from(action.program_gramph_id()) as usize;
                            // NOTE: Ample sets, in this case, are closed under intersection,
                            // because A = ⋃ {Ample(a) | a ∈ A} for every ample set A
                            // so A ∩ B = ⋃ {Ample(a) | a ∈ A ∩ B} is an ample set.
                            amples
                                .iter_mut()
                                .for_each(|(set, invalidate)| *invalidate |= set.contains(a));
                            // continue loop with same run and oracle
                            continue 'l;
                        } else {
                            // Branch satisfies all guarantees and can be discarded even though execution is incomplete
                            branches += 1;
                            recycling_sets.push(amples);
                            // No transitions left in this branch so exit transitions iteration
                            // NOTE: break while loop needed to satisfy borrow checker
                            break 'w;
                        }
                    }
                }
            } else if run.is_waiting() {
                run.time_tick();
                oracle.update_time(run.time());
                if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                    // Guarantee violated
                    branches += 1;
                    trace!(
                        "run violates a guarantee: processed {branches} branches in {:?}",
                        start_time.elapsed()
                    );
                    if let Some(tracer) = tracer {
                        self.record_trace(tracer, trace);
                    }
                    return Some(Vec::from_iter(
                        oracle.output_guarantees().map(|b| b.is_none_or(|b| b)),
                    ));
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
                }
            } else if oracle.final_output_guarantees().all(|b| b) {
                // Branch execution terminates and satisfies all guarantees and thus can be discarded
                branches += 1;
                recycling_sets.push(amples);
            } else {
                // Branch execution terminates without satisfying all guarantees
                branches += 1;
                trace!(
                    "run violates a guarantee: processed {branches} branches in {:?}",
                    start_time.elapsed()
                );
                if let Some(tracer) = tracer {
                    self.record_trace(tracer, trace);
                }
                return Some(Vec::from_iter(oracle.final_output_guarantees()));
            }

            if let Some((branch_oracle, branch_run, branch_amples, trace_len)) =
                executions_stack.pop()
            {
                // restore branch state of trace
                trace.truncate(trace_len);
                // continue loop with next run and oracle
                run = branch_run;
                *oracle = branch_oracle;
                amples = branch_amples;
            } else {
                // Model execution terminates and satisfies all guarantees
                trace!(
                    "run verifies all guarantees: processed {branches} branches in {:?}",
                    start_time.elapsed()
                );
                return Some(Vec::from_iter(oracle.final_output_guarantees()));
            }
        }
        trace!(
            "run stopped: processed {branches} branches in {:?}",
            start_time.elapsed()
        );
        None
    }

    fn experiment_priority<'a, O: Oracle + Clone, T: Tracer<'a>>(
        &self,
        mut oracle: O,
        tracer: Option<T>,
        running: Arc<AtomicBool>,
    ) -> RunOutcome {
        // Diagnostics/stats
        let start_time = Instant::now();
        let mut branches: u32 = 0;
        let oracle = &mut oracle;

        type BranchData<'a, O> = (
            O,
            TransitionSystemRun<'a>,
            Box<[(FixedBitSet, bool)]>,
            Dag,
            usize,
        );
        let mut executions_stack: Vec<BranchData<O>> = Vec::new();
        // TODO: measure impact of recycling memory to check wether it is worth it
        let mut recycling_sets: Vec<Box<[(FixedBitSet, bool)]>> = Vec::new();
        let mut recycling_dags: Vec<Dag> = Vec::new();
        let pgs = self.pg_list.len();
        let mut amples = vec![(FixedBitSet::with_capacity(pgs), true); pgs].into_boxed_slice();
        let mut restricted_ample = FixedBitSet::with_capacity(pgs);
        let mut restricted_ample_temp = FixedBitSet::with_capacity(pgs);
        let mut bump = Bump::new();
        let mut run = self.new_run();
        let mut dag = Dag::new();
        let dag_ids = (0..pgs).map(|_| dag.add_node(())).collect::<Vec<_>>();
        let mut trace = Vec::new();

        // Initialize oracle with TS initial state
        oracle.update_state(&Vec::from_iter(run.labels()));
        run.fastforward(&bump);

        let mut len_lower_bound = 1;

        let mut posts_last = None;
        let mut action_l = None;
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
                        run.possible_transitions_pg(PgId(b as u16))
                            .unwrap()
                            .flat_map(|(action, posts)| {
                                posts
                                    .multi_cartesian_product()
                                    .map(move |posts| (action, posts))
                            })
                    })
                    .peekable();
                // There must be active transitions because we checked earlier
                assert!(transitions.peek().is_some());
                'w: while let Some((action, posts)) = transitions.next() {
                    action_l = Some(action);
                    if transitions.peek().is_some() {
                        let mut branch_run = run.clone();
                        let mut branch_oracle = oracle.clone();
                        bump.reset();
                        branch_run
                            .transition(&mut branch_oracle, action, &posts, &bump)
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
                            if let Some(tracer) = tracer {
                                self.record_trace(tracer, trace);
                            }
                            return Some(Vec::from_iter(
                                branch_oracle
                                    .output_guarantees()
                                    .map(|b| b.is_none_or(|b| b)),
                            ));
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
                            let a = u16::from(action.program_gramph_id()) as usize;
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
                                trace.len(),
                            ));
                        } else {
                            // Branch satisfies all guarantees and can be discarded even though execution is incomplete
                            branches += 1;
                        }
                    } else {
                        posts_last = Some(posts);
                        break 'w;
                    }
                }
                assert!(transitions.next().is_none());
                drop(transitions);
                let action = action_l.expect("active transition");
                bump.reset();
                let event = run
                    .transition(
                        oracle,
                        action,
                        posts_last.as_ref().expect("active transition"),
                        &bump,
                    )
                    .unwrap();
                if tracer.is_some()
                    && let Some(event) = event
                {
                    trace.push((run.time(), action, event.into(), run.whole_state().into()));
                }
                if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                    // Guarantee violated
                    branches += 1;
                    trace!(
                        "run violates a guarantee: processed {branches} branches in {:?}",
                        start_time.elapsed()
                    );
                    if let Some(tracer) = tracer {
                        self.record_trace(tracer, trace);
                    }
                    return Some(Vec::from_iter(
                        oracle.output_guarantees().map(|b| b.is_none_or(|b| b)),
                    ));
                } else if oracle.output_guarantees().any(|b| b.is_none()) {
                    bump.reset();
                    run.fastforward(&bump);
                    // Update DAG
                    let a = u16::from(action.program_gramph_id()) as usize;
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
                    // because A = ⋃ {Ample(a) | a ∈ A} for every ample set A
                    // so A ∩ B = ⋃ {Ample(a) | a ∈ A ∩ B} is an ample set.
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
                }
            } else if run.is_waiting() {
                run.time_tick();
                oracle.update_time(run.time());
                if oracle.output_guarantees().any(|b| b.is_some_and(|b| !b)) {
                    // Guarantee violated
                    branches += 1;
                    trace!(
                        "run violates a guarantee: processed {branches} branches in {:?}",
                        start_time.elapsed()
                    );
                    if let Some(tracer) = tracer {
                        self.record_trace(tracer, trace);
                    }
                    return Some(Vec::from_iter(
                        oracle.output_guarantees().map(|b| b.is_none_or(|b| b)),
                    ));
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
                if let Some(tracer) = tracer {
                    self.record_trace(tracer, trace);
                }
                return Some(Vec::from_iter(oracle.final_output_guarantees()));
            }

            if let Some((branch_oracle, branch_run, branch_amples, branch_dag, trace_len)) =
                executions_stack.pop()
            {
                // restore branch state of trace
                trace.truncate(trace_len);
                // continue loop with next run and oracle
                run = branch_run;
                *oracle = branch_oracle;
                dag = branch_dag;
                amples = branch_amples;
            } else {
                // Model execution terminates and satisfies all guarantees
                trace!(
                    "run verifies all guarantees: processed {branches} branches in {:?}",
                    start_time.elapsed()
                );
                return Some(Vec::from_iter(oracle.final_output_guarantees()));
            }
        }
        trace!(
            "run stopped: processed {branches} branches in {:?}",
            start_time.elapsed()
        );
        None
    }
}

#[cfg(test)]
mod test {
    use crate::{channel_system::ChannelSystemBuilder, *};

    /// A dummy oracle that verifies nothing.
    /// Useful for testing.
    struct DummyOracle;

    impl Oracle for DummyOracle {}

    #[test]
    fn counter_pg() {
        let mut cs = ChannelSystemBuilder::new();
        let pg = cs.new_program_graph();
        let initial = cs.new_initial_location(pg).expect("new initial location");
        let action = cs.new_action(pg).expect("new action");
        let var = cs.new_var(pg, Val::Integer(0)).expect("new var");
        cs.add_effect(
            pg,
            action,
            var,
            (Expression::from_var(var, Type::Integer) + Expression::from(Val::Integer(1))).unwrap(),
        )
        .unwrap();
        for counter in 0..10 {
            let guard = Expression::equal_to(
                Expression::from_var(var, Type::Integer),
                Expression::from(Val::Integer(counter)),
            )
            .unwrap();
            cs.add_transition(pg, initial, action, initial, Some(guard))
                .unwrap();
        }
        let cs = cs.build();
        let ts = TransitionSystem::new(cs);
        let oracle = DummyOracle;
        ts.experiment_sample::<_, DummyTracer>(oracle, None, AtomicBool::new(true).into());
    }
}
