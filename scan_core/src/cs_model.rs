use crate::channel_system::{
    Channel, ChannelSystem, ChannelSystemBuilder, ChannelSystemRun, Event, EventType,
};
use crate::{BooleanExpr, DummyRng, Time, TransitionSystem, TransitionSystemGenerator, Val};

/// An atomic variable for [`crate::Pmtl`] formulae.
#[derive(Debug, Clone, Copy)]
pub enum Atom {
    /// A predicate.
    State(Channel, usize),
    /// A send event.
    Event(Channel),
}

/// A definition type that instances new [`CsModelRun`].
#[derive(Debug, Clone)]
pub struct CsModel {
    cs: ChannelSystem,
    ports: Vec<Channel>,
    vals: Vec<Vec<Val>>,
    predicates: Vec<BooleanExpr<Atom>>,
}

impl CsModel {
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
}

impl TransitionSystemGenerator for CsModel {
    type Ts<'a>
        = CsModelRun<'a>
    where
        Self: 'a;

    fn generate<'a>(&'a self) -> Self::Ts<'a> {
        let mut vals = self.vals.clone();
        vals.shrink_to_fit();
        CsModelRun {
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
pub struct CsModelRun<'def> {
    cs: ChannelSystemRun<'def>,
    ports: &'def [Channel],
    vals: Vec<Vec<Val>>,
    predicates: &'def [BooleanExpr<Atom>],
    last_event: Option<Event>,
}

impl<'def> TransitionSystem for CsModelRun<'def> {
    type Event = Event;

    fn transition(&mut self) {
        self.last_event = self.cs.montecarlo_execution();
        if let Some(ref event) = self.last_event
            && let EventType::Send(ref vals) = event.event_type
            && let Ok(index) = self.ports.binary_search(&event.channel)
        {
            // Since we have to update old values,
            // the vectors are already allocated and their is always the same.
            // Copying from slice should be faster than cloning.
            self.vals[index].copy_from_slice(vals);
        }
    }

    #[inline]
    fn last_event(&self) -> Option<&Self::Event> {
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
                        Val::Boolean(self.last_event.as_ref().is_some_and(|e| {
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
}
