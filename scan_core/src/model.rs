use std::collections::HashMap;
use std::sync::Arc;

use crate::channel_system::{Channel, ChannelSystem, Event, EventType, Message, PgId};
use crate::transition_system::TransitionSystem;
use crate::{Expression, FnExpression, Val};

pub type MdVar = (PgId, Channel, Message);

type FnMdExpression = FnExpression<HashMap<MdVar, Val>>;

#[derive(Debug, Clone)]
pub struct CsModel {
    current_state: ChannelSystem,
    vals: HashMap<MdVar, Val>,
    propositions: Arc<Vec<FnMdExpression>>,
}

impl CsModel {
    pub fn new(current_state: ChannelSystem, propositions: Vec<Expression<MdVar>>) -> Self {
        Self {
            current_state,
            vals: HashMap::new(),
            propositions: Arc::new(
                propositions
                    .into_iter()
                    .map(|prop| FnMdExpression::try_from(prop))
                    .collect::<Result<_, _>>()
                    .unwrap(),
            ),
        }
    }

    pub fn channel_system(&self) -> &ChannelSystem {
        &self.current_state
    }
}

impl TransitionSystem for CsModel {
    type Action = Event;

    fn labels(&self) -> Vec<Option<bool>> {
        self.propositions
            .iter()
            .map(|prop| {
                if let Some(Val::Boolean(b)) = prop.eval(&self.vals) {
                    Some(b)
                } else {
                    None
                }
            })
            .collect()
    }

    fn transitions(mut self) -> Vec<(Event, CsModel)> {
        // IntoIterator::into_iter(self.clone().list_transitions())
        // Perform all transitions that are deterministic and do not interact with channels.
        // The order in which these are performed does not matter.
        self.current_state.resolve_deterministic_transitions();
        self.current_state
            .possible_transitions()
            .map(|(pg_id, action, post)| {
                let mut model = self.clone();
                let event = model
                    .current_state
                    .transition(pg_id, action, post)
                    .expect("transition is possible");
                if let Some(event) = event {
                    match event.event_type {
                        EventType::Send(ref val) => {
                            model.vals.insert(
                                (event.pg_id, event.channel, Message::Send),
                                val.to_owned(),
                            );
                        }
                        EventType::Receive(ref val) => {
                            model.vals.insert(
                                (event.pg_id, event.channel, Message::Receive),
                                val.to_owned(),
                            );
                        }
                        // No meaningful value can be associated to these events.
                        EventType::ProbeEmptyQueue | EventType::ProbeFullQueue => {}
                    };
                    vec![(event, model)]
                } else {
                    model.transitions()
                }
            })
            .flatten()
            .collect::<Vec<_>>()
    }
}
