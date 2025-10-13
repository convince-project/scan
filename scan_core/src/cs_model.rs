use crate::TransitionSystemGenerator;
use crate::channel_system::{
    Channel, ChannelSystem, ChannelSystemBuilder, ChannelSystemRun, Event, EventType,
};
use crate::{DummyRng, Expression, Time, Val, transition_system::TransitionSystem};

/// An atomic variable for [`crate::Pmtl`] formulae.
#[derive(Debug, Clone)]
pub enum Atom {
    /// A predicate.
    State(Channel, usize),
    /// An event.
    Event(Event),
}

/// A definition type that instances new [`CsModelRun`].
#[derive(Debug, Clone)]
pub struct CsModel {
    cs: ChannelSystem,
    ports: Vec<Vec<Option<Val>>>,
    predicates: Vec<Expression<Atom>>,
}

impl CsModel {
    /// Creates a new [`CsModel`] from a [`ChannelSystemBuilder`].
    pub fn new(cs: ChannelSystemBuilder) -> Self {
        // TODO: Check predicates are Boolean expressions and that conversion does not fail
        let cs = cs.build();
        Self {
            ports: cs
                .channels()
                .iter()
                .map(|(types, _)| vec![None; types.len()])
                .collect(),
            cs,
            predicates: Vec::new(),
        }
    }

    /// Adds a new port to the [`CsModel`],
    /// which is given by an [`Channel`] and a default [`Val`] value.
    pub fn add_port(&mut self, channel: Channel, idx: usize, default: Val) {
        // TODO FIXME: error handling and type checking.
        self.ports[u16::from(channel) as usize][idx] = Some(default);
    }

    /// Adds a new predicate to the [`CsModel`],
    /// which is an expression over the CS's channels.
    pub fn add_predicate(&mut self, predicate: Expression<Atom>) -> usize {
        let _ = predicate.eval(
            &|port| match port {
                Atom::State(channel, idx) => self.ports[u16::from(channel) as usize][idx]
                    .expect("port must have been initialized"),
                Atom::Event(_event) => Val::Boolean(false),
            },
            &mut DummyRng,
        );
        self.predicates.push(predicate);
        self.predicates.len() - 1
    }
}

impl TransitionSystemGenerator for CsModel {
    type Ts<'a>
        = CsModelRun<'a>
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
pub struct CsModelRun<'def> {
    cs: ChannelSystemRun<'def>,
    ports: Vec<Vec<Option<Val>>>,
    // TODO: predicates should not use rng
    predicates: &'def [Expression<Atom>],
    last_event: Option<Event>,
}

impl<'def> TransitionSystem for CsModelRun<'def> {
    type Event = Event;

    fn transition(&mut self, duration: Time) -> Option<Event> {
        let event = self.cs.montecarlo_execution(duration);
        if let Some(ref event) = event
            && let EventType::Send(ref vals) = event.event_type
        {
            let port = self
                .ports
                .get_mut(u16::from(event.channel) as usize)
                .expect("port must exist");
            port.iter_mut().zip(vals).for_each(|(p, &v)| {
                if p.is_some() {
                    *p = Some(v)
                }
            });
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
                    Atom::State(channel, idx) => self.ports[u16::from(channel) as usize][idx]
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

    fn state(&self) -> impl Iterator<Item = Val> {
        self.ports.iter().flat_map(|p| p.iter().filter_map(|p| *p))
    }
}
