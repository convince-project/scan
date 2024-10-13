//! Implementation of the CS model of computation.
//!
//! Channel systems comprises multiple program graphs executing asynchronously
//! while sending and retreiving messages from channels.
//!
//! A channel system is given by:
//!
//! - A finite set of PGs.
//! - A finite set of channels, each of which has:
//!     - a given type;
//!     - a FIFO queue that can contain values of the channel's type;
//!     - a queue capacity limit: from zero (handshake communication) to infinite.
//! - Some PG actions are communication actions:
//!     - `send` actions push the computed value of an expression to the rear of the channel queue;
//!     - `receive` actions pop the value in front of the channel queue and write it onto a given PG variable;
//!     - `probe_empty_queue` actions can only be executed if the given channel has an empty queue;
//!     - `probe_full_queue` actions can only be executed if the given channel has a full queue;
//!
//! Analogously to PGs, a CS is defined through a [`ChannelSystemBuilder`],
//! by adding new PGs and channels.
//! Each PG in the CS can be given new locations, actions, effects, guards and transitions.
//! Then, a [`ChannelSystem`] is built from the [`ChannelSystemBuilder`]
//! and can be executed by performing transitions,
//! though the definition of the CS itself can no longer be altered.
//!
//! ```
//! # use scan_core::*;
//! # use scan_core::channel_system::*;
//! // Create a new CS builder
//! let mut cs_builder = ChannelSystemBuilder::new();
//!
//! // Add a new PG to the CS
//! let pg_1 = cs_builder.new_program_graph();
//!
//! // Get initial location of pg_1
//! let initial_1 = cs_builder
//!     .initial_location(pg_1)
//!     .expect("every PG has an initial location");
//!
//! // Create new channel
//! let chn = cs_builder.new_channel(Type::Integer, Some(1));
//!
//! // Create new send communication action
//! let send = cs_builder
//!     .new_send(pg_1, chn, CsExpression::from(1))
//!     .expect("always possible to add new actions");
//!
//! // Add transition sending a message to the channel
//! cs_builder.add_transition(pg_1, initial_1, send, initial_1, None)
//!     .expect("transition is well-defined");
//!
//! // Add a new PG to the CS
//! let pg_2 = cs_builder.new_program_graph();
//!
//! // Get initial location of pg_2
//! let initial_2 = cs_builder
//!     .initial_location(pg_2)
//!     .expect("every PG has an initial location");
//!
//! // Add new variable to pg_2
//! let var = cs_builder
//!     .new_var(pg_2, Expression::from(0))
//!     .expect("always possible to add new variable");
//!
//! // Create new receive communication action
//! let receive = cs_builder
//!     .new_receive(pg_2, chn, var)
//!     .expect("always possible to add new actions");
//!
//! // Add transition sending a message to the channel
//! cs_builder.add_transition(pg_2, initial_2, receive, initial_2, None)
//!     .expect("transition is well-defined");
//!
//! // Build the CS from its builder
//! // The builder is always guaranteed to build a well-defined CS and building cannot fail
//! let mut cs = cs_builder.build();
//!
//! // Since the channel is empty, only pg_1 can transition (with send)
//! assert_eq!(Vec::from_iter(cs.possible_transitions()), vec![(pg_1, send, initial_1)]);
//!
//! // Perform the transition, which sends a value to the channel queue
//! // After this, the channel is full
//! cs.transition(pg_1, send, initial_1)
//!     .expect("transition is possible");
//!
//! // Since the channel is now full, only pg_2 can transition (with receive)
//! assert_eq!(Vec::from_iter(cs.possible_transitions()), vec![(pg_2, receive, initial_2)]);
//!
//! // Perform the transition, which receives a value to the channel queue
//! // After this, the channel is empty
//! cs.transition(pg_2, receive, initial_2)
//!     .expect("transition is possible");
//! ```

mod builder;
pub use builder::*;

use crate::grammar::*;
use crate::program_graph::{Action as PgAction, Location as PgLocation, Var as PgVar, *};
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use thiserror::Error;

/// An indexing object for PGs in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct PgId(pub usize);

impl From<PgId> for usize {
    fn from(val: PgId) -> Self {
        val.0
    }
}

/// An indexing object for channels in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Channel(pub usize);

/// An indexing object for locations in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Location(PgId, PgLocation);

/// An indexing object for actions in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Action(PgId, PgAction);

/// An indexing object for typed variables in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Var(PgId, PgVar);

/// A message to be sent through a CS's channel.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Message {
    /// Sending the computed value of an expression to a channel.
    Send,
    /// Retrieving a value out of a channel and associating it to a variable.
    Receive,
    /// Checking whether a channel is empty.
    ProbeEmptyQueue,
    /// Checking whether a channel is full.
    ProbeFullQueue,
}

/// The error type for operations with [`ChannelSystemBuilder`]s and [`ChannelSystem`]s.
#[derive(Debug, Clone, Error)]
pub enum CsError {
    /// A PG within the CS returned an error of its own.
    #[error("error from program graph {0:?}")]
    ProgramGraph(PgId, #[source] PgError),
    /// There is no such PG in the CS.
    #[error("program graph {0:?} does not belong to the channel system")]
    MissingPg(PgId),
    /// The channel is at full capacity and can accept no more incoming messages.
    #[error("channel {0:?} is at full capacity")]
    OutOfCapacity(Channel),
    /// Channel is not full
    #[error("the channel still has free space {0:?}")]
    NotFull(Channel),
    /// The channel is empty and there is no message to be retrieved.
    #[error("channel {0:?} is empty")]
    Empty(Channel),
    /// The channel is not empty.
    #[error("channel {0:?} is not empty")]
    NotEmpty(Channel),
    /// There is no such communication action in the CS.
    #[error("communication {0:?} has not been defined")]
    NoCommunication(Action),
    /// The action does not belong to the PG.
    #[error("action {0:?} does not belong to program graph {1:?}")]
    ActionNotInPg(Action, PgId),
    /// The variable does not belong to the PG.
    #[error("variable {0:?} does not belong to program graph {1:?}")]
    VarNotInPg(Var, PgId),
    /// The location does not belong to the PG.
    #[error("location {0:?} does not belong to program graph {1:?}")]
    LocationNotInPg(Location, PgId),
    /// The given PGs do not match.
    #[error("program graphs {0:?} and {1:?} do not match")]
    DifferentPgs(PgId, PgId),
    /// Action is a communication.
    ///
    /// Is returned when trying to associate an effect to a communication action.
    #[error("action {0:?} is a communication")]
    ActionIsCommunication(Action),
    /// There is no such channel in the CS.
    #[error("channel {0:?} does not exists")]
    MissingChannel(Channel),
    /// Cannot probe an handshake channel
    #[error("cannot probe handshake {0:?}")]
    ProbingHandshakeQueue(Channel),
    /// Cannot probe for fullness an infinite capacity channel
    #[error("cannot probe for fullness the infinite capacity {0:?}")]
    ProbingInfiniteQueue(Channel),
    /// A type error
    #[error("type error")]
    Type(#[source] TypeError),
}

/// A Channel System event related to a channel.
#[derive(Debug, Clone)]
pub struct Event {
    /// The PG producing the event in the course of a transition.
    pub pg_id: PgId,
    /// The channel involved in the event.
    pub channel: Channel,
    /// The type of event produced.
    pub event_type: EventType,
}

/// A Channel System event type related to a channel.
#[derive(Debug, Clone)]
pub enum EventType {
    /// Sending a value to a channel.
    Send(Val),
    /// Retrieving a value out of a channel.
    Receive(Val),
    /// Checking whether a channel is empty.
    ProbeEmptyQueue,
    /// Checking whether a channel is full.
    ProbeFullQueue,
}

/// Representation of a CS that can be executed transition-by-transition.
///
/// The structure of the CS cannot be changed,
/// meaning that it is not possible to introduce new PGs or modifying them, or add new channels.
/// Though, this restriction makes it so that cloning the [`ChannelSystem`] is cheap,
/// because only the internal state needs to be duplicated.
///
/// The only way to produce a [`ChannelSystem`] is through a [`ChannelSystemBuilder`].
/// This guarantees that there are no type errors involved in the definition of its PGs,
/// and thus the CS will always be in a consistent state.
#[derive(Debug, Clone)]
pub struct ChannelSystem {
    program_graphs: Vec<ProgramGraph>,
    channels: Arc<Vec<(Type, Option<usize>)>>,
    communications: Arc<HashMap<Action, (Channel, Message)>>,
    message_queue: Vec<VecDeque<Val>>,
}

impl ChannelSystem {
    /// Iterates over all transitions that can be admitted in the current state.
    ///
    /// An admittable transition is characterized by the PG it executes on, the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The (eventual) guard is guaranteed to be satisfied.
    ///
    /// See also [`ProgramGraph::possible_transitions`].
    pub fn possible_transitions(&self) -> impl Iterator<Item = (PgId, Action, Location)> + '_ {
        self.program_graphs
            .iter()
            .enumerate()
            .flat_map(move |(id, pg)| {
                let pg_id = PgId(id);
                pg.possible_transitions().filter_map(move |(action, post)| {
                    let action = Action(pg_id, action);
                    let post = Location(pg_id, post);
                    if self.communications.contains_key(&action)
                        && self.check_communication(pg_id, action).is_err()
                    {
                        None
                    } else {
                        Some((pg_id, action, post))
                    }
                })
            })
    }

    pub(crate) fn resolve_deterministic_transitions(&mut self) {
        for (pg_id, pg) in self.program_graphs.iter_mut().enumerate() {
            let pg_id = PgId(pg_id);
            loop {
                // `take(2)` because we need no more to establish the transition is non-deterministic
                let trans: Vec<_> = pg.possible_transitions().take(2).collect();
                if trans.len() == 1 {
                    let (action, post) = trans[0];
                    if self.communications.contains_key(&Action(pg_id, action)) {
                        break;
                    } else {
                        pg.transition(action, post).expect("transition is possible");
                    }
                } else {
                    break;
                }
            }
        }
    }

    fn check_communication(&self, pg_id: PgId, action: Action) -> Result<(), CsError> {
        if action.0 != pg_id {
            Err(CsError::ActionNotInPg(action, pg_id))
        } else if let Some((channel, message)) = self.communications.get(&action) {
            let (_, capacity) = self.channels[channel.0];
            let queue = &self.message_queue[channel.0];
            // Channel capacity must never be exeeded!
            assert!(capacity.is_none() || capacity.is_some_and(|cap| queue.len() <= cap));
            match message {
                Message::Send if capacity.is_some_and(|cap| queue.len() == cap) => {
                    Err(CsError::OutOfCapacity(*channel))
                }
                Message::Receive if queue.is_empty() => Err(CsError::Empty(*channel)),
                Message::ProbeEmptyQueue | Message::ProbeFullQueue
                    if matches!(capacity, Some(0)) =>
                {
                    Err(CsError::ProbingHandshakeQueue(*channel))
                }
                Message::ProbeFullQueue if capacity.is_none() => {
                    Err(CsError::ProbingInfiniteQueue(*channel))
                }
                Message::ProbeEmptyQueue if !queue.is_empty() => Err(CsError::NotEmpty(*channel)),
                Message::ProbeFullQueue if capacity.is_some_and(|cap| queue.len() < cap) => {
                    Err(CsError::NotFull(*channel))
                }
                _ => Ok(()),
            }
        } else {
            Err(CsError::NoCommunication(action))
        }
    }

    /// Executes a transition on the given PG characterized by the argument action and post-state.
    ///
    /// Fails if the requested transition is not admissible.
    ///
    /// See also [`ProgramGraph::transition`].
    pub fn transition(
        &mut self,
        pg_id: PgId,
        action: Action,
        post: Location,
    ) -> Result<Option<Event>, CsError> {
        // If action is a communication, check it is legal
        if action.0 != pg_id {
            return Err(CsError::ActionNotInPg(action, pg_id));
        } else if post.0 != pg_id {
            return Err(CsError::LocationNotInPg(post, pg_id));
        } else if self.communications.contains_key(&action) {
            self.check_communication(pg_id, action)?;
        }
        // Transition the program graph
        let pg = self
            .program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))?;
        // If the action is a communication, send/receive the message
        if let Some(&(channel, message)) = self.communications.get(&action) {
            // communication has been verified before so there is a queue for channel.0
            let queue = &mut self.message_queue[channel.0];
            let event_type = match message {
                Message::Send => {
                    let val = pg
                        .send(action.1, post.1)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
                    queue.push_back(val.clone());
                    EventType::Send(val)
                }
                Message::Receive => {
                    let val = queue
                        .pop_front()
                        .expect("communication has been verified before");
                    pg.receive(action.1, post.1, val.clone())
                        .expect("communication has been verified before");
                    EventType::Receive(val)
                }
                Message::ProbeEmptyQueue => {
                    assert!(
                        queue.is_empty(),
                        "by definition, ProbeEmptyQueue is only possible if the queue is empty"
                    );
                    pg.transition(action.1, post.1)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
                    EventType::ProbeEmptyQueue
                }
                Message::ProbeFullQueue => {
                    pg.transition(action.1, post.1)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
                    EventType::ProbeFullQueue
                }
            };
            Ok(Some(Event {
                pg_id,
                channel,
                event_type,
            }))
        } else {
            pg.transition(action.1, post.1)
                .map_err(|err| CsError::ProgramGraph(pg_id, err))
                .map(|()| None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builder() {
        let _cs = ChannelSystemBuilder::new();
    }

    #[test]
    fn new_pg() {
        let mut cs = ChannelSystemBuilder::new();
        let _ = cs.new_program_graph();
    }

    #[test]
    fn new_action() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let pg = cs.new_program_graph();
        let _action = cs.new_action(pg)?;
        Ok(())
    }

    #[test]
    fn new_var() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let pg = cs.new_program_graph();
        let _var1 = cs.new_var(pg, Expression::Const(Val::Boolean(false)))?;
        let _var2 = cs.new_var(pg, Expression::Const(Val::Integer(0)))?;
        Ok(())
    }

    #[test]
    fn add_effect() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let pg = cs.new_program_graph();
        let action = cs.new_action(pg)?;
        let var1 = cs.new_var(pg, Expression::Const(Val::Boolean(false)))?;
        let var2 = cs.new_var(pg, Expression::Const(Val::Integer(0)))?;
        let effect_1 = CsExpression::Const(Val::Integer(2));
        cs.add_effect(pg, action, var1, effect_1.clone())
            .expect_err("type mismatch");
        let effect_2 = CsExpression::Const(Val::Boolean(true));
        cs.add_effect(pg, action, var1, effect_2.clone())?;
        cs.add_effect(pg, action, var2, effect_2)
            .expect_err("type mismatch");
        cs.add_effect(pg, action, var2, effect_1)?;
        Ok(())
    }

    #[test]
    fn new_location() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let pg = cs.new_program_graph();
        let initial = cs.initial_location(pg)?;
        let location = cs.new_location(pg)?;
        assert_ne!(initial, location);
        Ok(())
    }

    #[test]
    fn add_transition() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let pg = cs.new_program_graph();
        let initial = cs.initial_location(pg)?;
        let action = cs.new_action(pg)?;
        let var1 = cs.new_var(pg, Expression::Const(Val::Boolean(false)))?;
        let var2 = cs.new_var(pg, Expression::Const(Val::Integer(0)))?;
        let effect_1 = CsExpression::Const(Val::Integer(0));
        let effect_2 = CsExpression::Const(Val::Boolean(true));
        cs.add_effect(pg, action, var1, effect_2)?;
        cs.add_effect(pg, action, var2, effect_1)?;
        let post = cs.new_location(pg)?;
        cs.add_transition(pg, initial, action, post, None)?;
        Ok(())
    }

    #[test]
    fn add_communication() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let ch = cs.new_channel(Type::Boolean, Some(1));

        let pg1 = cs.new_program_graph();
        let initial1 = cs.initial_location(pg1)?;
        let post1 = cs.new_location(pg1)?;
        let effect = CsExpression::Const(Val::Boolean(true));
        let send = cs.new_send(pg1, ch, effect)?;
        cs.add_transition(pg1, initial1, send, post1, None)?;

        let var1 = cs.new_var(pg1, Expression::Const(Val::Integer(0)))?;
        let effect = CsExpression::Const(Val::Integer(0));
        cs.add_effect(pg1, send, var1, effect)
            .expect_err("send is a message so it cannot have effects");

        let pg2 = cs.new_program_graph();
        let initial2 = cs.initial_location(pg2)?;
        let post2 = cs.new_location(pg2)?;
        let var2 = cs.new_var(pg2, Expression::Const(Val::Boolean(false)))?;
        let receive = cs.new_receive(pg2, ch, var2)?;
        cs.add_transition(pg2, initial2, receive, post2, None)?;

        let mut cs = cs.build();
        assert_eq!(cs.possible_transitions().count(), 1);

        cs.transition(pg1, send, post1)?;
        cs.transition(pg2, receive, post2)?;
        assert_eq!(cs.possible_transitions().count(), 0);
        Ok(())
    }
}
