use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use log::trace;

use crate::channel_system::{
    Action, Channel, ChannelSystem, ChannelSystemBuilder, ChannelSystemRun, Event, EventType,
};
use crate::{BooleanExpr, DummyRng, Oracle, RunOutcome, Time, Tracer, Val};

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
    ports: Vec<Channel>,
    vals: Vec<Vec<Val>>,
    predicates: Vec<BooleanExpr<Atom>>,
}

impl TransitionSystem {
    /// Creates a new [`CsModel`] from a [`ChannelSystemBuilder`].
    pub fn new(cs: ChannelSystemBuilder) -> Self {
        let cs = cs.build();
        Self {
            ports: Vec::new(),
            vals: Vec::new(),
            cs,
            predicates: Vec::new(),
        }
    }

    /// Adds a new port to the [`CsModel`],
    /// which is given by an [`Channel`] and a default [`Val`] value.
    pub fn add_port(&mut self, channel: Channel, mut value: Vec<Val>) {
        let len = self.cs.channels()[u16::from(channel) as usize].0.len();
        assert_eq!(len, value.len());
        // TODO FIXME: error handling and type checking.
        // Keep ports list ordered
        // Don't insert duplicated ports
        if let Err(index) = self.ports.binary_search(&channel) {
            self.ports.insert(index, channel);
            value.shrink_to_fit();
            self.vals.insert(index, value);
        }
        assert!(self.ports.is_sorted());
        assert_eq!(self.ports.len(), self.vals.len());
    }

    /// Adds a new predicate to the [`CsModel`],
    /// which is an expression over the CS's channels.
    pub fn add_predicate(&mut self, predicate: BooleanExpr<Atom>) {
        // Make sure predicate type-checks
        let _ = predicate.eval(
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
            &mut DummyRng,
        );
        self.predicates.push(predicate);
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
        TransitionSystemRun {
            cs: self.cs.new_instance(),
            ports: &self.ports,
            vals,
            predicates: &self.predicates,
            last_event: None,
        }
    }
}

/// Transition system model based on a [`ChannelSystem`].
///
/// It is essentially a CS which keeps track of the [`Event`]s produced by the execution
/// and determining a set of predicates.
#[derive(Debug, Clone)]
pub struct TransitionSystemRun<'def> {
    cs: ChannelSystemRun<'def>,
    ports: &'def [Channel],
    vals: Vec<Vec<Val>>,
    predicates: &'def [BooleanExpr<Atom>],
    last_event: Option<(Action, Event)>,
}

impl<'def> TransitionSystemRun<'def> {
    /// Perform a random transition.
    ///
    /// Used to generate Montecarlo-like executions
    pub fn transition(&mut self) {
        self.last_event = self.cs.montecarlo_execution();
        if let Some((_, ref event)) = self.last_event
            && let EventType::Send(ref vals) = event.event_type
            && let Ok(index) = self.ports.binary_search(&event.channel)
        {
            // Since we have to update old values,
            // the vectors are already allocated and their is always the same.
            // Copying from slice should be faster than cloning.
            self.vals[index].copy_from_slice(vals);
        }
    }

    /// Returns last event processed by model.
    #[inline]
    pub fn last_event(&self) -> Option<&(Action, Event)> {
        self.last_event.as_ref()
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
        self.predicates.iter().map(|prop| {
            prop.eval(
                &|port| match port {
                    Atom::State(channel, idx) => {
                        let port_idx = self
                            .ports
                            .binary_search(&channel)
                            .expect("port must exist and be initialized");
                        self.vals[port_idx][idx]
                    }
                    Atom::Event(channel) => {
                        Val::Boolean(self.last_event.as_ref().is_some_and(|(_, e)| {
                            e.channel == channel && matches!(e.event_type, EventType::Send(..))
                        }))
                    }
                },
                &mut DummyRng,
            )
        })
    }

    #[inline]
    fn state(&self) -> impl Iterator<Item = Val> {
        self.vals.iter().flatten().copied()
    }

    /// Runs a single execution of the [`TransitionSystem`] with a given [`Oracle`] and returns a [`RunOutcome`].
    pub(crate) fn experiment<O: Oracle>(
        &mut self,
        duration: Time,
        mut oracle: O,
        running: Arc<AtomicBool>,
    ) -> RunOutcome {
        trace!("new run starting");
        // reuse vector to avoid allocations
        let mut labels = Vec::from_iter(self.labels());
        // Initialize oracle with TS initial state
        oracle.update_state(&labels);
        while self.time() <= duration {
            self.transition();
            if self.last_event().is_some() {
                labels.clear();
                labels.extend(self.labels());
                oracle.update_state(&labels);
            } else {
                self.time_tick();
                oracle.update_time(self.time());
            }
            if !running.load(Ordering::Relaxed) {
                trace!("run stopped");
                return RunOutcome::Incomplete;
            } else if oracle.output_guarantees().all(|b| b.is_some()) {
                trace!("run complete early");
                let verified = Vec::from_iter(oracle.output_guarantees().map(Option::unwrap));
                return RunOutcome::Verified(verified);
            }
        }
        trace!("run complete");
        let verified = Vec::from_iter(oracle.final_output_guarantees());
        RunOutcome::Verified(verified)
    }

    /// Runs a single execution of the [`TransitionSystem`] with a given [`Oracle`]
    /// and process the execution trace via the given [`Tracer`].
    pub(crate) fn trace<T, O: Oracle>(&mut self, duration: Time, mut oracle: O, mut tracer: T)
    where
        T: Tracer,
    {
        trace!("new run starting");
        // reuse vector to avoid allocations
        let mut labels = Vec::new();
        tracer.init();
        while self.time() <= duration {
            self.transition();
            if let Some((action, event)) = self.last_event() {
                tracer.trace(*action, event, self.time(), self.state());
                labels.clear();
                labels.extend(self.labels());
                oracle.update_state(&labels);
            } else {
                self.time_tick();
                oracle.update_time(self.time());
            }
        }
        trace!("run complete");
        let verified = Vec::from_iter(oracle.final_output_guarantees());
        tracer.finalize(&RunOutcome::Verified(verified));
    }
}
