use crate::TransitionSystemGenerator;
use crate::channel_system::{
    Channel, ChannelSystem, ChannelSystemBuilder, ChannelSystemRun, Event, EventType,
};
use crate::grammar::FnExpression;
use crate::{DummyRng, Expression, Time, Val, transition_system::TransitionSystem};
use rand::{Rng, SeedableRng};

/// An atomic variable for [`crate::Pmtl`] formulae.
#[derive(Debug, Clone)]
pub enum Atom {
    /// A predicate.
    State(Channel),
    /// An event.
    Event(Event),
}

/// A definition type that instances new [`CsModelRun`].
#[derive(Debug, Clone)]
pub struct CsModel<R: Rng> {
    cs: ChannelSystem<R>,
    ports: Vec<Option<Val>>,
    predicates: Vec<FnExpression<Atom, DummyRng>>,
}

impl<R: Clone + Rng + SeedableRng + 'static> CsModel<R> {
    /// Creates new [`CsModelBuilder`] from a [`ChannelSystem`].
    pub fn new(cs: ChannelSystemBuilder<R>) -> Self {
        // TODO: Check predicates are Boolean expressions and that conversion does not fail
        let cs = cs.build();
        Self {
            ports: vec![None; cs.channels().len()],
            cs,
            predicates: Vec::new(),
        }
    }

    /// Adds a new port to the [`CsModelBuilder`],
    /// which is given by an [`Channel`] and a default [`Val`] value.
    pub fn add_port(&mut self, channel: Channel, default: Val) {
        // TODO FIXME: error handling and type checking.
        self.ports[u16::from(channel) as usize] = Some(default);
    }

    /// Adds a new predicate to the [`CsModelBuilder`],
    /// which is an expression over the CS's channels.
    pub fn add_predicate(&mut self, predicate: Expression<Atom>) -> usize {
        let predicate = FnExpression::<Atom, _>::from(predicate);
        let _ = predicate.eval(
            &|port| match port {
                Atom::State(channel) => self.ports[u16::from(channel) as usize]
                    .clone()
                    .expect("port must have been initialized"),
                Atom::Event(_event) => Val::Boolean(false),
            },
            &mut DummyRng,
        );
        self.predicates.push(predicate);
        self.predicates.len() - 1
    }
}

impl<R: Rng + SeedableRng> TransitionSystemGenerator for CsModel<R> {
    type Ts<'a>
        = CsModelRun<'a, R>
    where
        Self: 'a;

    fn generate<'a>(&'a self) -> Self::Ts<'a> {
        CsModelRun {
            cs: self.cs.new_instance(),
            ports: self.ports.clone(),
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
pub struct CsModelRun<'def, R: Rng + SeedableRng> {
    cs: ChannelSystemRun<'def, R>,
    ports: Vec<Option<Val>>,
    // TODO: predicates should not use rng
    predicates: &'def [FnExpression<Atom, DummyRng>],
    last_event: Option<Event>,
}

impl<'def, R: Rng + SeedableRng> TransitionSystem for CsModelRun<'def, R> {
    type Event = Event;

    fn transition(&mut self, duration: Time) -> Option<Event> {
        let event = self.cs.montecarlo_execution(duration);
        if let Some(ref event) = event
            && let EventType::Send(ref val) = event.event_type
            && let Some(port) = self
                .ports
                .get_mut(u16::from(event.channel) as usize)
                .expect("port must exist")
        {
            *port = val.clone();
        }
        self.last_event = event.clone();
        event
    }

    fn time(&self) -> Time {
        self.cs.time()
    }

    fn labels(&self) -> impl Iterator<Item = bool> {
        self.predicates.iter().map(|prop| {
            if let Val::Boolean(b) = prop.eval(
                &|port| match port {
                    Atom::State(channel) => self.ports[u16::from(channel) as usize]
                        .clone()
                        .expect("port must exist and be initialized"),
                    Atom::Event(event) => {
                        Val::Boolean(self.last_event.as_ref().is_some_and(|e| e == &event))
                    }
                },
                &mut DummyRng,
            ) {
                b
            } else {
                // FIXME: handle error or guarantee it won't happen
                panic!("propositions should evaluate to boolean values")
            }
        })
    }

    fn state(&self) -> impl Iterator<Item = &Val> {
        self.ports.iter().filter_map(|p| p.as_ref())
    }
}
