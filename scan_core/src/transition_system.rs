mod run;

use std::any::type_name;
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
    Action, Channel, ChannelSystem, CsError, Event, EventType, Location, PgId, PgIndex,
};
use crate::{BooleanExpr, Oracle, OracleTree, RunOutcome, Scheduler, Tracer, Val};
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

// type Trace<'a> = Vec<(Time, Action, OwnedEvent, Box<[Val]>)>;

/// Errors produced by a [`TransitionSystem`].
#[derive(Debug, Clone, Copy, Error)]
pub enum TsError {
    /// Error returned by the underlying [`ChannelSystem`].
    #[error("error from channel system: {0:?}")]
    ChannelSystem(#[from] CsError),
    /// The default value set for the port is not the right type,
    /// i.e., the type of messages of the channel.
    #[error("default port value is not the type of the channel {0:?}")]
    WrongPortType(Channel),
    /// The oracle tasked with the verification does not support the chosen kind of scheduler.
    #[error("the oracle {0} does not support the scheduler {1:?}")]
    UnsupportedScheduler(&'static str, Scheduler),
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
    #[inline]
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
    ) -> Result<RunOutcome, TsError> {
        // NOTE: Uniform scheduler writes the trace on-the-fly,
        // whether or not it violates the guarantees,
        // while Exhaustive and Priority schedulers return a complete trace that violates the guarantees,
        // or nothing if all guarantees where satisfied by all considered schedulers.
        match scheduler {
            Scheduler::Uniform => Ok(self.experiment_sample(oracle, tracer, running)),
            // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
            Scheduler::Sampling => todo!(),
            Scheduler::Exhaustive | Scheduler::Priority => {
                Err(TsError::UnsupportedScheduler(type_name::<O>(), scheduler))
            }
        }
    }

    pub(crate) fn experiment_tree<O: OracleTree + Clone>(
        &self,
        oracle: O,
        running: Arc<AtomicBool>,
        scheduler: Scheduler,
    ) -> Result<RunOutcome, TsError> {
        match scheduler {
            Scheduler::Uniform | Scheduler::Sampling => {
                Err(TsError::UnsupportedScheduler(type_name::<O>(), scheduler))
            }
            Scheduler::Exhaustive => Ok(self.experiment_tree_exhaustive(oracle, running)),
            Scheduler::Priority => Ok(self.experiment_tree_priority(oracle, running)),
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

    // fn record_trace<'a, T: Tracer<'a>>(&self, mut tracer: T, trace: Trace) {
    //     let ports_idxs = self.port_idxs();
    //     for (time, action, event, state) in trace {
    //         tracer.trace(action, (&event).into(), time, &|channel| {
    //             self.ports
    //                 .binary_search(&channel)
    //                 .ok()
    //                 .map(|idx| &state[ports_idxs[idx]..ports_idxs[idx + 1]])
    //         });
    //     }
    // }

    fn experiment_tree_exhaustive<O: OracleTree + Clone>(
        &self,
        mut oracle: O,
        running: Arc<AtomicBool>,
    ) -> RunOutcome {
        // Diagnostics/stats
        let start_time = Instant::now();

        type BranchData<'a, O> = (
            TransitionSystemRun<'a>,
            Box<[(FixedBitSet, bool)]>,
            Vec<(Action, Vec<Location>)>,
            O,
        );
        let mut executions_stack: Vec<BranchData<O>> = Vec::new();
        let pgs = self.pg_list.len();
        let mut amples = vec![(FixedBitSet::with_capacity(pgs), true); pgs].into_boxed_slice();
        let mut bump = Bump::new();
        let mut run = self.new_run();

        // Initialize oracle with TS initial state
        let state: Box<[bool]> = run.labels().collect();
        // NOTE: root of the oracle tree should be initial state
        oracle.update_state(&state);
        // // add initial system state as root node so that CTL state semantics correct even without top-level quantifier
        // executions_stack.push((run.clone(), amples.clone(), Vec::new(), oracle.clone()));

        // FILO stack: depth-first search
        while running.load(Ordering::Relaxed) {
            if oracle.output_guarantees().any(|o| o.is_none()) {
                bump.reset();
                run.fastforward(&bump);
                run.ample(&mut amples);

                // Find most suitable ample set
                if let Some((ample, _)) = amples
                    .iter()
                    // only consider viable sets (i.e., neither invalidated nor empty)
                    .filter(|(set, _invalid)| !set.is_clear())
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
                        .collect::<Vec<_>>();
                    // There must be active transitions because we checked earlier
                    let (action, posts) = transitions.pop().expect("valid transition");
                    // NOTE: for optimization, avoid pushing and popping first execution branch into/out of stack
                    // and do not add new node to stack if that is the only branch
                    if !transitions.is_empty() {
                        executions_stack.push((
                            run.clone(),
                            amples.clone(),
                            transitions,
                            oracle.clone(),
                        ));
                    }
                    bump.reset();
                    let _event = run.transition(&mut oracle, action, &posts, &bump).unwrap();
                    let action_pg_idx = u16::from(action.program_graph_id()) as usize;
                    // NOTE: Ample sets, in this case, are closed under intersection,
                    // because A = ⋃ {Ample(a) | a ∈ A} for every ample set A
                    // so A ∩ B = ⋃ {Ample(a) | a ∈ A ∩ B} is an ample set.
                    amples
                        .iter_mut()
                        .for_each(|(set, invalidate)| *invalidate |= set.contains(action_pg_idx));
                    // Continue execution of same TS and oracle
                    continue;
                } else if run.is_waiting() {
                    executions_stack.push((
                        run.clone(),
                        amples.clone(),
                        Vec::new(),
                        oracle.clone(),
                    ));
                    run.time_tick();
                    oracle.update_time(run.time());
                    amples
                        .iter_mut()
                        .for_each(|(_set, invalidate)| *invalidate = true);
                    // Continue execution of same TS and oracle
                    continue;
                }
            }

            // Close current tree branch exploration and pop next one (if any)
            if let Some((branch_run, branch_amples, actions, branch_oracle)) =
                executions_stack.last_mut()
            {
                let (action, posts) = actions.pop().expect("action list is never empty");
                branch_oracle.fold_branch(oracle);
                if actions.is_empty() {
                    let (branch_run, branch_amples, actions, branch_oracle) =
                        executions_stack.pop().unwrap();
                    assert!(actions.is_empty());
                    run = branch_run;
                    amples = branch_amples;
                    oracle = branch_oracle;
                } else {
                    run = branch_run.clone();
                    amples.clone_from(branch_amples);
                    oracle = branch_oracle.clone();
                }
                let _event = run.transition(&mut oracle, action, &posts, &bump).unwrap();
                // Update DAG
                let action_pg_idx = u16::from(action.program_graph_id()) as usize;
                // NOTE: Ample sets, in this case, are closed under intersection,
                // because A = ⋃ {Ample(a) | a ∈ A} for every ample set A
                // so A ∩ B = ⋃ {Ample(a) | a ∈ A ∩ B} is an ample set.
                amples
                    .iter_mut()
                    .for_each(|(set, invalidate)| *invalidate |= set.contains(action_pg_idx));
            } else {
                // Model execution terminates
                assert!(executions_stack.is_empty(),);
                trace!("run terminates in {:?}", start_time.elapsed());
                return Some(Vec::from_iter(oracle.final_output_guarantees()));
            }
        }
        trace!("run stopped in {:?}", start_time.elapsed());
        None
    }

    fn experiment_tree_priority<O: OracleTree + Clone>(
        &self,
        mut oracle: O,
        running: Arc<AtomicBool>,
    ) -> RunOutcome {
        // Diagnostics/stats
        let start_time = Instant::now();

        type BranchData<'a, O> = (
            TransitionSystemRun<'a>,
            Dag,
            Box<[(FixedBitSet, bool)]>,
            Vec<(Action, Vec<Location>)>,
            FixedBitSet,
            O,
        );
        let mut executions_stack: Vec<BranchData<O>> = Vec::new();
        let pgs = self.pg_list.len();
        let mut amples = vec![(FixedBitSet::with_capacity(pgs), true); pgs].into_boxed_slice();
        let mut bump = Bump::new();
        let mut restricted_ample = FixedBitSet::with_capacity(pgs);
        let mut restricted_ample_temp = FixedBitSet::with_capacity(pgs);
        let mut run = self.new_run();
        let mut dag = Dag::new();
        let dag_ids = (0..pgs).map(|_| dag.add_node(())).collect::<Vec<_>>();

        // Initialize oracle with TS initial state
        let state: Box<[bool]> = run.labels().collect();
        // NOTE: root of the oracle tree should be initial state
        oracle.update_state(&state);
        // // add initial system state as root node so that CTL state semantics correct even without top-level quantifier
        // executions_stack.push((
        //     run.clone(),
        //     dag.clone(),
        //     amples.clone(),
        //     Vec::new(),
        //     restricted_ample.clone(),
        //     oracle.clone(),
        // ));

        // FILO stack: depth-first search
        while running.load(Ordering::Relaxed) {
            if oracle.output_guarantees().any(|o| o.is_none()) {
                bump.reset();
                run.fastforward(&bump);
                run.ample(&mut amples);

                // Find most suitable ample set
                let mut len_min = usize::MAX;
                restricted_ample.clear();
                // only consider non-empty sets
                'f: for (set, _) in amples.iter().filter(|(set, _)| !set.is_clear()) {
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
                    len_min = len;
                }

                if !restricted_ample.is_clear() {
                    let mut transitions = restricted_ample
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
                        .collect::<Vec<_>>();
                    // There must be active transitions because we checked earlier
                    let (action, posts) = transitions.pop().expect("valid transition");
                    // NOTE: for optimization, avoid pushing and popping first execution branch into/out of stack
                    // and do not add new node to stack if that is the only branch
                    if !transitions.is_empty() {
                        executions_stack.push((
                            run.clone(),
                            dag.clone(),
                            amples.clone(),
                            transitions,
                            restricted_ample.clone(),
                            oracle.clone(),
                        ));
                    }
                    bump.reset();
                    let _event = run.transition(&mut oracle, action, &posts, &bump).unwrap();
                    // Update DAG
                    let action_pg_idx = u16::from(action.program_graph_id()) as usize;
                    // NOTE: Ample sets, in this case, are closed under intersection,
                    // because A = ⋃ {Ample(a) | a ∈ A} for every ample set A
                    // so A ∩ B = ⋃ {Ample(a) | a ∈ A ∩ B} is an ample set.
                    amples
                        .iter_mut()
                        .for_each(|(set, invalidate)| *invalidate |= set.contains(action_pg_idx));
                    // Prepare DAG
                    let a = u16::from(action.program_graph_id()) as usize;
                    let node_a = dag_ids[a];
                    restricted_ample.ones().for_each(|b| {
                        if b != a {
                            let _ = dag
                                .try_update_edge(node_a, dag_ids[b], ())
                                .expect("edge must be valid");
                        }
                    });
                    // Continue execution of same TS and oracle
                    continue;
                } else if run.is_waiting() {
                    run.time_tick();
                    oracle.update_time(run.time());
                    amples
                        .iter_mut()
                        .for_each(|(_set, invalidate)| *invalidate = true);
                    // Continue execution of same TS and oracle
                    continue;
                }
            }

            // Close current tree branch exploration and pop next one (if any)
            if let Some((
                branch_run,
                branch_dag,
                branch_amples,
                actions,
                branch_restricted_ample,
                branch_oracle,
            )) = executions_stack.last_mut()
            {
                let (action, posts) = actions.pop().expect("action list is never empty");
                branch_oracle.fold_branch(oracle);
                if actions.is_empty() {
                    let (
                        branch_run,
                        branch_dag,
                        branch_amples,
                        _actions,
                        branch_restricted_ample,
                        branch_oracle,
                    ) = executions_stack.pop().unwrap();
                    run = branch_run;
                    dag = branch_dag;
                    amples = branch_amples;
                    restricted_ample = branch_restricted_ample;
                    oracle = branch_oracle;
                } else {
                    run = branch_run.clone();
                    dag.clone_from(branch_dag);
                    amples.clone_from(branch_amples);
                    restricted_ample.clone_from(branch_restricted_ample);
                    oracle = branch_oracle.clone();
                }
                let _event = run.transition(&mut oracle, action, &posts, &bump).unwrap();
                // Update DAG
                let action_pg_idx = u16::from(action.program_graph_id()) as usize;
                // NOTE: Ample sets, in this case, are closed under intersection,
                // because A = ⋃ {Ample(a) | a ∈ A} for every ample set A
                // so A ∩ B = ⋃ {Ample(a) | a ∈ A ∩ B} is an ample set.
                amples
                    .iter_mut()
                    .for_each(|(set, invalidate)| *invalidate |= set.contains(action_pg_idx));
                // Prepare DAG
                let a = u16::from(action.program_graph_id()) as usize;
                let node_a = dag_ids[a];
                restricted_ample.ones().for_each(|b| {
                    if b != a {
                        let _ = dag
                            .try_update_edge(node_a, dag_ids[b], ())
                            .expect("edge must be valid");
                    }
                });
            } else {
                // Model execution terminates
                assert!(executions_stack.is_empty(),);
                trace!("run terminates in {:?}", start_time.elapsed());
                return Some(Vec::from_iter(oracle.final_output_guarantees()));
            }
        }
        trace!("run stopped in {:?}", start_time.elapsed());
        None
    }
}

#[cfg(test)]
mod test {
    use crate::{channel_system::ChannelSystemBuilder, *};

    /// A dummy oracle that verifies nothing.
    /// Useful for testing.
    #[derive(Debug, Clone, Copy)]
    struct DummyOracle;

    impl Oracle for DummyOracle {}

    impl OracleTree for DummyOracle {
        fn fold_branch(&mut self, _closed: Self) {}

        // fn end_branch(&mut self) {}
    }

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
        ts.experiment::<_, DummyTracer>(
            oracle,
            None,
            AtomicBool::new(true).into(),
            Scheduler::Uniform,
        )
        .expect("run experiment");
        ts.experiment::<_, DummyTracer>(
            oracle,
            None,
            AtomicBool::new(true).into(),
            Scheduler::Exhaustive,
        )
        .expect("run experiment");
        ts.experiment::<_, DummyTracer>(
            oracle,
            None,
            AtomicBool::new(true).into(),
            Scheduler::Priority,
        )
        .expect("run experiment");
    }
}
