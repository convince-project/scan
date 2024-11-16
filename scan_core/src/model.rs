use std::collections::HashMap;
use std::sync::Arc;

use crate::channel_system::{Channel, ChannelSystem, Event, EventType};
use crate::transition_system::TransitionSystem;
use crate::{Expression, FnExpression, Time, Val};

type FnMdExpression = FnExpression<Channel>;

#[derive(Debug)]
pub struct CsModelBuilder {
    cs: ChannelSystem,
    vals: HashMap<Channel, Val>,
    predicates: Vec<FnMdExpression>,
}

impl CsModelBuilder {
    pub fn new(initial_state: ChannelSystem) -> Self {
        // TODO: Check predicates are Boolean expressions and that conversion does not fail
        Self {
            cs: initial_state,
            vals: HashMap::new(),
            predicates: Vec::new(),
        }
    }

    pub fn add_port(&mut self, channel: Channel, default: Val) {
        if let std::collections::hash_map::Entry::Vacant(e) = self.vals.entry(channel) {
            e.insert(default);
        } else {
            panic!("entry is already taken");
        }
    }

    pub fn add_predicate(&mut self, predicate: Expression<Channel>) -> usize {
        let predicate = FnExpression::<Channel>::from(predicate);
        let _ = predicate.eval(&|port| self.vals.get(&port).unwrap().clone());
        self.predicates.push(predicate);
        self.predicates.len() - 1
    }

    /// Creates a new [`CsModel`] with the given underlying [`ChannelSystem`] and set of predicates.
    ///
    /// Predicates have to be passed all at once,
    /// as it is not possible to add any further ones after the [`CsModel`] has been initialized.
    pub fn build(self) -> CsModel {
        CsModel {
            cs: self.cs,
            vals: self.vals,
            last_event: None,
            predicates: Arc::new(self.predicates),
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
    vals: HashMap<Channel, Val>,
    predicates: Arc<Vec<FnMdExpression>>,
    last_event: Option<Event>,
}

impl CsModel {
    /// Gets the underlying [`ChannelSystem`].
    #[inline(always)]
    pub fn channel_system(&self) -> &ChannelSystem {
        &self.cs
    }
}

impl TransitionSystem for CsModel {
    type Action = Event;

    fn labels(&self) -> Vec<bool> {
        self.predicates
            .iter()
            .map(|prop| {
                if let Val::Boolean(b) = prop.eval(&|port| self.vals.get(&port).unwrap().clone()) {
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

    fn monaco_transition<R: rand::Rng>(
        &mut self,
        rng: &mut R,
        duration: Time,
    ) -> Option<Self::Action> {
        self.last_event = self.cs.monaco_execution(rng, duration);
        if let Some(event) = self.last_event.as_ref() {
            if let EventType::Send(ref val) = event.event_type {
                self.vals.insert(event.channel, val.to_owned());
            }
        }
        self.last_event.to_owned()
    }
}
