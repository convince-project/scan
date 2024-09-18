use std::collections::HashMap;
use std::sync::Arc;

use crate::channel_system::{Channel, ChannelSystem, Event, EventType};
use crate::transition_system::TransitionSystem;
use crate::{Expression, FnExpression, Val};

// pub type MdVar = (PgId, Channel, Message);
pub type Port = Channel;

type FnMdExpression = FnExpression<HashMap<Port, Val>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PredicateId(usize);

impl From<PredicateId> for usize {
    fn from(val: PredicateId) -> Self {
        val.0
    }
}

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

    pub fn add_port(&mut self, channel: Channel, val: Val) -> Result<(), ()> {
        if let std::collections::hash_map::Entry::Vacant(e) = self.vals.entry(channel) {
            e.insert(val);
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
    vals: HashMap<Channel, Val>,
    predicates: Arc<Vec<FnMdExpression>>,
}

impl CsModel {
    // /// Creates a new [`CsModel`] with the given underlying [`ChannelSystem`] and set of predicates.
    // ///
    // /// Predicates have to be passed all at once,
    // /// as it is not possible to add any further ones after the [`CsModel`] has been initialized.
    // pub fn new(
    //     current_state: ChannelSystem,
    //     predicates: Vec<Expression<Port>>,
    //     initial: HashMap<Port, Val>,
    // ) -> Self {
    //     // TODO: Check predicates are Boolean expressions and that conversion does not fail
    //     Self {
    //         cs: current_state,
    //         vals: initial,
    //         predicates: Arc::new(
    //             predicates
    //                 .into_iter()
    //                 .map(FnMdExpression::try_from)
    //                 .collect::<Result<_, _>>()
    //                 .unwrap(),
    //         ),
    //     }
    // }

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
                    if let EventType::Receive(ref val) = event.event_type {
                        model.vals.insert(event.channel, val.to_owned());
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
                    model.transitions()
                }
            })
            .collect::<Vec<_>>()
    }
}
