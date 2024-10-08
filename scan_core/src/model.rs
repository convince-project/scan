use std::collections::HashMap;
use std::sync::Arc;

use crate::channel_system::{Channel, ChannelSystem, Event, EventType};
use crate::transition_system::TransitionSystem;
use crate::{Expression, FnExpression, Integer, Val};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Port {
    Message(Channel),
    LastMessage,
}

type FnMdExpression = FnExpression<HashMap<Port, Val>>;

// Constant corresponding to no event index.
const NO_EVENT: Integer = -1;

#[derive(Debug)]
pub struct CsModelBuilder {
    cs: ChannelSystem,
    vals: HashMap<Port, Val>,
    predicates: Vec<FnMdExpression>,
}

impl CsModelBuilder {
    pub fn new(initial_state: ChannelSystem) -> Self {
        let mut vals = HashMap::new();
        // As default value for LastMessage, take the constant NO_EVENT.
        let _ = vals.insert(Port::LastMessage, Val::Integer(NO_EVENT));
        // TODO: Check predicates are Boolean expressions and that conversion does not fail
        Self {
            cs: initial_state,
            vals,
            predicates: Vec::new(),
        }
    }

    pub fn add_port(&mut self, channel: Channel, default: Val) -> Result<(), ()> {
        if let std::collections::hash_map::Entry::Vacant(e) =
            self.vals.entry(Port::Message(channel))
        {
            e.insert(default);
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn add_predicate(&mut self, predicate: Expression<Port>) -> Result<usize, ()> {
        let predicate = FnMdExpression::try_from(predicate)?;
        if predicate.eval(&self.vals).is_some() {
            self.predicates.push(predicate);
            Ok(self.predicates.len() - 1)
        } else {
            Err(())
        }
    }

    /// Creates a new [`CsModel`] with the given underlying [`ChannelSystem`] and set of predicates.
    ///
    /// Predicates have to be passed all at once,
    /// as it is not possible to add any further ones after the [`CsModel`] has been initialized.
    pub fn build(self) -> CsModel {
        CsModel {
            cs: self.cs,
            vals: self.vals,
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
    vals: HashMap<Port, Val>,
    predicates: Arc<Vec<FnMdExpression>>,
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
                if let Some(Val::Boolean(b)) = prop.eval(&self.vals) {
                    // Some(b)
                    b
                } else {
                    // None
                    // FIXME
                    panic!("I don't know how to handle this");
                }
            })
            .collect()
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
                if let Some(event) = event {
                    if let EventType::Send(ref val) = event.event_type {
                        model
                            .vals
                            .insert(Port::Message(event.channel), val.to_owned());
                        model
                            .vals
                            .insert(Port::LastMessage, Val::Integer(event.channel.0 as Integer));
                    } else {
                        let _ = model.vals.insert(Port::LastMessage, Val::Integer(NO_EVENT));
                    }
                    // match event.event_type {
                    //     EventType::Send(ref val) => {
                    //         model.vals.insert(
                    //             (event.pg_id, event.channel, Message::Send),
                    //             val.to_owned(),
                    //         );
                    //     }
                    //     EventType::Receive(ref val) => {
                    //         model.vals.insert(
                    //             (event.pg_id, event.channel, Message::Receive),
                    //             val.to_owned(),
                    //         );
                    //     }
                    //     // No meaningful value can be associated to these events.
                    //     EventType::ProbeEmptyQueue | EventType::ProbeFullQueue => {}
                    // };
                    vec![(event, model)]
                } else {
                    let _ = model.vals.insert(Port::LastMessage, Val::Integer(NO_EVENT));
                    model.transitions()
                }
            })
            .collect::<Vec<_>>()
    }
}
