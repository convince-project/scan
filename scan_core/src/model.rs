use std::collections::HashMap;
use std::sync::Arc;

use crate::channel_system::{Channel, ChannelSystem, Event, EventType};
use crate::transition_system::TransitionSystem;
use crate::{Expression, FnExpression, Val};

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

    pub fn add_port(&mut self, channel: Channel, default: Val) -> Result<(), ()> {
        if let std::collections::hash_map::Entry::Vacant(e) = self.vals.entry(channel) {
            e.insert(default);
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn add_predicate(&mut self, predicate: Expression<Channel>) -> Result<usize, ()> {
        let predicate = FnExpression::<Channel>::from(predicate);
        let _ = predicate.eval(&|port| self.vals.get(&port).unwrap().clone());
        // let _ = predicate.eval(&|port| self.vals.get(&port).cloned());
        self.predicates.push(predicate);
        Ok(self.predicates.len() - 1)
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
/// (i.e., of the [`Message`]s sent to and from [`Channel`]s)
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
                if let Val::Boolean(b) = prop.eval(&|port| self.vals.get(&port).unwrap().clone())
                // .eval(&|port| self.vals.get(&port).cloned())
                // .expect("boolean value")
                {
                    Some(b)
                } else {
                    None
                }
            })
            .collect::<Option<Vec<_>>>()
            // FIXME: handle error or guarantee it won't happen
            .unwrap()
    }

    fn transitions(mut self) -> Vec<(Event, CsModel)> {
        // IntoIterator::into_iter(self.clone().list_transitions())
        // Perform all transitions that are deterministic and do not interact with channels.
        // The order in which these are performed does not matter.
        self.cs.resolve_deterministic_transitions();
        self.cs
            .possible_transitions()
            .flat_map(|(pg_id, action, post)| {
                let mut model = self.clone();
                let event = model
                    .cs
                    .transition(pg_id, action, post)
                    .expect("transition is possible");
                model.last_event = event.clone();
                if let Some(event) = event {
                    if let EventType::Send(ref val) = event.event_type {
                        model.vals.insert(event.channel, val.to_owned());
                    }
                    vec![(event, model)]
                } else {
                    model.transitions()
                }
            })
            .collect::<Vec<_>>()
    }
}
