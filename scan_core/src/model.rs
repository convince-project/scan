use crate::channel_system::{Channel, ChannelSystem, CsError, Event, EventType};
use crate::{DummyRng, Expression, FnExpression, Time, TransitionSystem, Val};
use rand::{Rng, SeedableRng};
use std::collections::{BTreeMap, btree_map};
use std::sync::Arc;

/// An atomic variable for [`Pmtl`] formulae.
#[derive(Debug, Clone)]
pub enum Atom {
    /// A predicate.
    State(Channel),
    /// An event.
    Event(Event),
}

/// A builder type for [`CsModel`].
pub struct CsModelBuilder<R: Rng + SeedableRng> {
    cs: ChannelSystem<R>,
    ports: BTreeMap<Channel, Val>,
    predicates: Vec<FnExpression<Atom, DummyRng>>,
}

impl<R: Rng + SeedableRng> CsModelBuilder<R> {
    /// Creates new [`CsModelBuilder`] from a [`ChannelSystem`].
    pub fn new(initial_state: ChannelSystem<R>) -> Self {
        // TODO: Check predicates are Boolean expressions and that conversion does not fail
        Self {
            cs: initial_state,
            ports: BTreeMap::new(),
            predicates: Vec::new(),
        }
    }

    /// Adds a new port to the [`CsModelBuilder`],
    /// which is given by an [`Channel`] and a default [`Val`] value.
    pub fn add_port(&mut self, channel: Channel, default: Val) {
        // TODO FIXME: error handling and type checking.
        if let btree_map::Entry::Vacant(e) = self.ports.entry(channel) {
            e.insert(default);
        } else {
            panic!("entry is already taken");
        }
    }

    /// Adds a new predicate to the [`CsModelBuilder`],
    /// which is an expression over the CS's channels.
    pub fn add_predicate(&mut self, predicate: Expression<Atom>) -> usize {
        let predicate = FnExpression::<Atom, _>::from(predicate);
        let _ = predicate.eval(
            &|port| match port {
                Atom::State(channel) => self.ports.get(&channel).unwrap().clone(),
                Atom::Event(_event) => Val::Boolean(false),
            },
            &mut DummyRng,
        );
        self.predicates.push(predicate);
        self.predicates.len() - 1
    }

    /// Creates a new [`CsModel`] with the given underlying [`ChannelSystem`] and set of predicates.
    ///
    /// Predicates have to be passed all at once,
    /// as it is not possible to add any further ones after the [`CsModel`] has been initialized.
    pub fn build(self) -> CsModel<R> {
        CsModel {
            cs: self.cs,
            ports: self.ports,
            predicates: Arc::new(self.predicates),
            last_event: None,
        }
    }
}

/// Transition system model based on a [`ChannelSystem`].
///
/// It is essentially a CS which keeps track of the [`Event`]s produced by the execution
/// and determining a set of predicates.
#[derive(Clone)]
pub struct CsModel<R: Rng + SeedableRng> {
    cs: ChannelSystem<R>,
    ports: BTreeMap<Channel, Val>,
    // TODO: predicates should not use rng
    predicates: Arc<Vec<FnExpression<Atom, DummyRng>>>,
    last_event: Option<Event>,
}

impl<R: Rng + Clone + Send + Sync + SeedableRng> TransitionSystem<Event, CsError> for CsModel<R> {
    fn transition(&mut self, duration: Time) -> Result<Option<Event>, CsError> {
        let event = self.cs.montecarlo_execution(duration);
        if let Some(ref event) = event {
            if let btree_map::Entry::Occupied(mut e) = self.ports.entry(event.channel) {
                if let EventType::Send(ref val) = event.event_type {
                    e.insert(val.clone());
                }
            }
        }
        self.last_event = event.clone();
        Ok(event)
    }

    fn time(&self) -> Time {
        self.cs.time()
    }

    fn labels(&self) -> Vec<bool> {
        self.predicates
            .iter()
            .map(|prop| {
                if let Val::Boolean(b) = prop.eval(
                    &|port| match port {
                        Atom::State(channel) => self.ports.get(&channel).unwrap().clone(),
                        Atom::Event(event) => {
                            Val::Boolean(self.last_event.as_ref().is_some_and(|e| e == &event))
                        }
                    },
                    &mut DummyRng,
                ) {
                    Some(b)
                } else {
                    None
                }
            })
            .collect::<Option<Vec<_>>>()
            // FIXME: handle error or guarantee it won't happen
            .unwrap()
    }

    fn state(&self) -> impl Iterator<Item = &Val> {
        self.ports.values()
    }
}
