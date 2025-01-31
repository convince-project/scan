use crate::channel_system::{Channel, ChannelSystem, Event, EventType};
use crate::{Expression, FnExpression, Pmtl, PmtlOracle, Time, Tracer, Val};
use log::{info, trace};
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// An atomic variable for [`Pmtl`] formulae.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Atom {
    /// A predicate.
    State(Channel),
    /// An event.
    Event(Event),
}

/// A builder type for [`CsModel`].
#[derive(Debug)]
pub struct CsModelBuilder {
    cs: ChannelSystem,
    ports: HashMap<Channel, Val>,
    predicates: Vec<FnExpression<Atom>>,
    assumes: Vec<Pmtl<usize>>,
    guarantees: Vec<Pmtl<usize>>,
}

impl CsModelBuilder {
    /// Creates new [`CsModelBuilder`] from a [`ChannelSystem`].
    pub fn new(initial_state: ChannelSystem) -> Self {
        // TODO: Check predicates are Boolean expressions and that conversion does not fail
        Self {
            cs: initial_state,
            ports: HashMap::new(),
            predicates: Vec::new(),
            assumes: Vec::new(),
            guarantees: Vec::new(),
        }
    }

    /// Adds a new port to the [`CsModelBuilder`],
    /// which is given by an [`Channel`] and a default [`Val`] value.
    pub fn add_port(&mut self, channel: Channel, default: Val) {
        // TODO FIXME: error handling and type checking.
        if let std::collections::hash_map::Entry::Vacant(e) = self.ports.entry(channel) {
            e.insert(default);
        } else {
            panic!("entry is already taken");
        }
    }

    /// Adds a new predicate to the [`CsModelBuilder`],
    /// which is an expression over the CS's channels.
    pub fn add_predicate(&mut self, predicate: Expression<Atom>) -> usize {
        let predicate = FnExpression::<Atom>::from(predicate);
        let _ = predicate.eval(&|port| match port {
            Atom::State(channel) => self.ports.get(&channel).unwrap().clone(),
            Atom::Event(_event) => Val::Boolean(false),
        });
        self.predicates.push(predicate);
        self.predicates.len() - 1
    }

    /// Adds an assume [`Pmtl`] formula to the [`CsModelBuilder`].
    pub fn add_assume(&mut self, assume: Pmtl<usize>) {
        self.assumes.push(assume);
    }

    /// Adds an guarantee [`Pmtl`] formula to the [`CsModelBuilder`].
    pub fn add_guarantee(&mut self, guarantee: Pmtl<usize>) {
        self.guarantees.push(guarantee);
    }

    /// Creates a new [`CsModel`] with the given underlying [`ChannelSystem`] and set of predicates.
    ///
    /// Predicates have to be passed all at once,
    /// as it is not possible to add any further ones after the [`CsModel`] has been initialized.
    pub fn build(self) -> CsModel {
        CsModel {
            cs: self.cs,
            ports: self.ports,
            predicates: Arc::new(self.predicates),
            oracle: PmtlOracle::new(&self.assumes, &self.guarantees),
        }
    }
}

/// Transition system model based on a [`ChannelSystem`].
///
/// It is essentially a CS which keeps track of the [`Event`]s produced by the execution
/// and determining a set of predicates.
#[derive(Debug, Clone)]
pub struct CsModel {
    cs: ChannelSystem,
    ports: HashMap<Channel, Val>,
    predicates: Arc<Vec<FnExpression<Atom>>>,
    oracle: PmtlOracle,
}

impl CsModel {
    /// Gets the underlying [`ChannelSystem`].
    #[inline(always)]
    pub fn channel_system(&self) -> &ChannelSystem {
        &self.cs
    }

    fn labels(&self, last_event: &Event) -> Vec<bool> {
        self.predicates
            .iter()
            .map(|prop| {
                if let Val::Boolean(b) = prop.eval(&|port| match port {
                    Atom::State(channel) => self.ports.get(&channel).unwrap().clone(),
                    Atom::Event(event) => Val::Boolean(*last_event == event),
                }) {
                    Some(b)
                } else {
                    None
                }
            })
            .collect::<Option<Vec<_>>>()
            // FIXME: handle error or guarantee it won't happen
            .unwrap()
    }

    #[inline(always)]
    fn time(&self) -> Time {
        self.cs.time()
    }

    /// Statistically verifies [`CsModel`] using adaptive bound and the given parameters.
    /// It allows to optionally pass a [`Tracer`] object to record the produced traces,
    /// and a state [`Mutex`] to be updated with the results as they are produced.
    pub fn par_adaptive<P>(
        &self,
        confidence: f64,
        precision: f64,
        length: usize,
        duration: Time,
        tracer: Option<P>,
        state: Arc<Mutex<(u32, u32, bool)>>,
    ) where
        P: Tracer<Event> + Clone + Send + Sync,
    {
        info!("verification starting");
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        (0..usize::MAX)
            .into_par_iter()
            .take_any_while(|_| {
                // .take_while(|_| {
                let result =
                    self.clone()
                        .experiment(tracer.clone(), length, duration, state.clone());
                let (s, f, running) = &mut *state.lock().expect("lock state");
                if *running {
                    if let Some(result) = result {
                        if result {
                            *s += 1;
                            // If all guarantees are satisfied, the execution is successful
                            info!("runs: {s} successes");
                        } else {
                            *f += 1;
                            // If guarantee is violated, we have found a counter-example!
                            info!("runs: {f} failures");
                        }
                        let n = *s + *f;
                        // Avoid division by 0
                        let avg = if n == 0 { 0.5f64 } else { *s as f64 / n as f64 };
                        if crate::adaptive_bound(avg, confidence, precision) <= n as f64 {
                            info!("adaptive bound satisfied");
                            *running = false;
                        }
                    }
                }
                info!("returning {running} to iter");
                *running
            })
            .count();
        info!("verification terminating");
    }

    fn experiment<P>(
        mut self,
        mut tracer: Option<P>,
        length: usize,
        duration: Time,
        run_state: Arc<Mutex<(u32, u32, bool)>>,
    ) -> Option<bool>
    where
        P: Tracer<Event>,
    {
        use rand::rngs::SmallRng;
        use rand::SeedableRng;

        let mut current_len = 0;
        let rng = &mut SmallRng::from_os_rng();
        if let Some(publisher) = tracer.as_mut() {
            publisher.init();
        }
        trace!("new run starting");
        while let Some(event) = self.cs.montecarlo_execution(rng, duration) {
            if let EventType::Send(ref val) = event.event_type {
                self.ports.insert(event.channel, val.clone());
            }
            current_len += 1;
            let state = self.labels(&event);
            let time = self.time();
            if let Some(tracer) = tracer.as_mut() {
                tracer.trace(&event, time, &state);
            }
            self.oracle = self.oracle.update(&state, time);
            match self.oracle.output() {
                Some(true) => {
                    if current_len >= length {
                        trace!("run exceeds maximum lenght");
                        if let Some(tracer) = tracer {
                            tracer.finalize(None);
                        }
                        return None;
                    }
                }
                Some(false) => {
                    trace!("run fails");
                    if let Some(tracer) = tracer {
                        tracer.finalize(Some(false));
                    }
                    return Some(false);
                }
                None => {
                    trace!("run undetermined");
                    if let Some(publisher) = tracer {
                        publisher.finalize(None);
                    }
                    return None;
                }
            }
            if !run_state.lock().expect("lock state").2 {
                if let Some(tracer) = tracer {
                    tracer.finalize(None);
                }
                return None;
            }
        }
        trace!("run succeeds");
        if let Some(tracer) = tracer {
            tracer.finalize(Some(true));
        }
        Some(true)
    }
}
