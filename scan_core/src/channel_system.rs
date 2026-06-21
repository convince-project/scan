//! Implementation of the CS model of computation.
//!
//! Channel systems comprises multiple program graphs executing asynchronously
//! while sending and retrieving messages from channels.
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
//!     .new_initial_location(pg_1)
//!     .expect("every PG has an initial location");
//!
//! // Create new channel
//! let chn = cs_builder.new_channel(vec![Type::Integer], Some(1));
//!
//! // Create new send communication action
//! let send = cs_builder
//!     .new_send(pg_1, chn, vec![CsExpression::from(1i64)])
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
//!     .new_initial_location(pg_2)
//!     .expect("every PG has an initial location");
//!
//! // Add new variable to pg_2
//! let var = cs_builder
//!     .new_var(pg_2, Val::from(0i64))
//!     .expect("always possible to add new variable");
//!
//! // Create new receive communication action
//! let receive = cs_builder
//!     .new_receive(pg_2, chn, vec![var])
//!     .expect("always possible to add new actions");
//!
//! // Add transition sending a message to the channel
//! cs_builder.add_transition(pg_2, initial_2, receive, initial_2, None)
//!     .expect("transition is well-defined");
//!
//! // Build the CS from its builder
//! // The builder is always guaranteed to build a well-defined CS and building cannot fail
//! let cs = cs_builder.build();
//! let mut instance = cs.new_instance();
//!
//! // Since the channel is empty, only pg_1 can transition (with send)
//! {
//! let mut iter = instance.possible_transitions();
//! let (pg, action, mut trans) = iter.next().unwrap();
//! assert_eq!(pg, pg_1);
//! assert_eq!(action, send);
//! let post_locs: Vec<Location> = trans.next().unwrap().collect();
//! assert_eq!(post_locs, vec![initial_1]);
//! assert!(iter.next().is_none());
//! }
//!
//! // Perform the transition, which sends a value to the channel queue
//! // After this, the channel is full
//! instance.transition(pg_1, send, &[initial_1])
//!     .expect("transition is possible");
//!
//! // Since the channel is now full, only pg_2 can transition (with receive)
//! {
//! let mut iter = instance.possible_transitions();
//! let (pg, action, mut trans) = iter.next().unwrap();
//! assert_eq!(pg, pg_2);
//! assert_eq!(action, receive);
//! let post_locs: Vec<Location> = trans.next().unwrap().collect();
//! assert_eq!(post_locs, vec![initial_2]);
//! assert!(iter.next().is_none());
//! }
//!
//! // Perform the transition, which receives a value to the channel queue
//! // After this, the channel is empty
//! instance.transition(pg_2, receive, &[initial_2])
//!     .expect("transition is possible");
//! ```

mod builder;
mod run;

use crate::grammar::*;
use crate::program_graph::{
    Action as PgAction, Clock as PgClock, Location as PgLocation, Var as PgVar, *,
};
pub use builder::*;
pub use run::ChannelSystemRun;
use thiserror::Error;

type PgIndex = u16;

/// An indexing object for PGs in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PgId(PgIndex);

impl From<PgId> for PgIndex {
    #[inline]
    fn from(val: PgId) -> Self {
        val.0
    }
}

/// An indexing object for channels in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Channel(u16);

impl From<Channel> for u16 {
    #[inline]
    fn from(val: Channel) -> Self {
        val.0
    }
}

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
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Action(PgId, PgAction);

/// An indexing object for typed variables in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Var(PgId, PgVar);

/// An indexing object for clocks in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
///
/// See also [`PgClock`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Clock(PgId, PgClock);

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
#[derive(Debug, Clone, Copy, Error)]
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
    /// The clock does not belong to the PG.
    #[error("clock {0:?} does not belong to program graph {1:?}")]
    ClockNotInPg(Clock, PgId),
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
    ProbingHandshakeChannel(Channel),
    /// Cannot probe for fullness an infinite capacity channel
    #[error("cannot probe for fullness the infinite capacity {0:?}")]
    ProbingInfiniteQueue(Channel),
    /// A type error
    #[error("type error")]
    Type(#[source] TypeError),
}

/// A Channel System event related to a channel.
#[derive(Debug, Clone, PartialEq)]
pub struct Event {
    /// The PG producing the event in the course of a transition.
    pub pg_id: PgId,
    /// The channel involved in the event.
    pub channel: Channel,
    /// The type of event produced.
    pub event_type: EventType,
}

/// A Channel System event type related to a channel.
#[derive(Debug, Clone, PartialEq)]
pub enum EventType {
    /// Sending a value to a channel.
    Send(Vec<Val>),
    /// Retrieving a value out of a channel.
    Receive(Vec<Val>),
    /// Checking whether a channel is empty.
    ProbeEmptyQueue,
    /// Checking whether a channel is full.
    ProbeFullQueue,
}

/// The capacity type of a channel:
#[derive(Debug, Clone, Copy)]
pub enum ChannelCapacity {
    /// A (in)finite-capacity FIFO queue.
    Queue(Option<usize>),
    /// A channel that receives messages but never returns them.
    Sink,
}

/// A definition object for a CS.
/// It represents the abstract definition of a CS.
///
/// The only way to produce a [`ChannelSystem`] is through a [`ChannelSystemBuilder`].
/// This guarantees that there are no type errors involved in the definition of its PGs,
/// and thus the CS will always be in a consistent state.
///
/// The only way to execute the [`ChannelSystem`] is to generate a new [`ChannelSystemRun`] through [`ChannelSystem::new_instance`].
/// The [`ChannelSystemRun`] cannot outlive its [`ChannelSystem`], as it holds references to it.
/// This allows to cheaply generate multiple [`ChannelSystemRun`]s from the same [`ChannelSystem`].
///
/// Example:
///
/// ```
/// # use scan_core::channel_system::ChannelSystemBuilder;
/// // Create and populate a CS builder object
/// let mut cs_builder = ChannelSystemBuilder::new();
/// let pg_id = cs_builder.new_program_graph();
/// let initial = cs_builder.new_initial_location(pg_id).expect("create new location");
/// cs_builder.add_autonomous_transition(pg_id, initial, initial, None).expect("add transition");
///
/// // Build the builder object to get a CS definition object.
/// let cs_def = cs_builder.build();
///
/// // Instantiate a CS with the previously built definition.
/// let mut cs = cs_def.new_instance();
///
/// // Perform the (unique) active transition available.
/// let (pg_id_trans, e, mut post_locs) = cs.possible_transitions().last().expect("autonomous transition");
/// assert_eq!(pg_id_trans, pg_id);
/// let post_loc = post_locs.last().expect("post location").last().expect("post location");
/// assert_eq!(post_loc, initial);
/// cs.transition(pg_id, e, &[initial]).expect("transition is active");
/// ```
#[derive(Debug, Clone)]
pub struct ChannelSystem {
    channels: Vec<(Vec<Type>, ChannelCapacity)>,
    communications: Vec<Option<(Channel, Message)>>,
    communications_pg_idxs: Vec<usize>,
    program_graphs: Vec<ProgramGraph>,
}

impl ChannelSystem {
    /// Creates a new [`ChannelSystemRun`] which allows to execute the CS as defined.
    ///
    /// The new instance borrows the caller to refer to the CS definition without copying its data,
    /// so that spawning instances is (relatively) inexpensive.
    ///
    /// See also [`ProgramGraph::new_instance`].
    pub fn new_instance<'def>(&'def self) -> ChannelSystemRun<'def> {
        ChannelSystemRun::new(self)
    }

    #[inline]
    fn communication(&self, pg_id: PgId, pg_action: PgAction) -> Option<(Channel, Message)> {
        if pg_action == EPSILON {
            None
        } else {
            let start = self.communications_pg_idxs[pg_id.0 as usize];
            self.communications[start + ActionIdx::from(pg_action) as usize]
        }
    }

    /// Returns an immutable reference to the list of [`ProgramGraph`]s of the Channel System.
    #[inline]
    pub fn program_graphs(&self) -> &[ProgramGraph] {
        &self.program_graphs
    }

    /// Returns the list of [`PgId`]s associated to the Program Graphs of the Channel System.
    #[inline]
    pub fn program_graph_ids(&self) -> impl Iterator<Item = PgId> {
        (0..self.program_graphs.len()).map(|idx| PgId(idx as PgIndex))
    }

    /// Returns the list of defined channels, given as the pair of their type and capacity
    /// (where `None` denotes channels with infinite capacity, and `Some` denotes channels with finite capacity).
    #[inline]
    pub fn channels(&self) -> &[(Vec<Type>, ChannelCapacity)] {
        &self.channels
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builder() {
        let _cs: ChannelSystemBuilder = ChannelSystemBuilder::new();
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
        let _var1 = cs.new_var(pg, Val::from(false))?;
        let _var2 = cs.new_var(pg, Val::from(0i64))?;
        Ok(())
    }

    #[test]
    fn add_effect() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let pg = cs.new_program_graph();
        let action = cs.new_action(pg)?;
        let var1 = cs.new_var(pg, Val::from(false))?;
        let var2 = cs.new_var(pg, Val::from(0i64))?;
        let effect_1 = CsExpression::from(2i64);
        cs.add_effect(pg, action, var1, effect_1.clone())
            .expect_err("type mismatch");
        let effect_2 = CsExpression::from(true);
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
        let initial = cs.new_initial_location(pg)?;
        let location = cs.new_location(pg)?;
        assert_ne!(initial, location);
        Ok(())
    }

    #[test]
    fn add_transition() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let pg = cs.new_program_graph();
        let initial = cs.new_initial_location(pg)?;
        let action = cs.new_action(pg)?;
        let var1 = cs.new_var(pg, Val::from(false))?;
        let var2 = cs.new_var(pg, Val::from(0i64))?;
        let effect_1 = CsExpression::from(0i64);
        let effect_2 = CsExpression::from(true);
        cs.add_effect(pg, action, var1, effect_2)?;
        cs.add_effect(pg, action, var2, effect_1)?;
        let post = cs.new_location(pg)?;
        cs.add_transition(pg, initial, action, post, None)?;
        Ok(())
    }

    #[test]
    fn add_communication() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let ch = cs.new_channel(vec![Type::Boolean], Some(1));

        let pg1 = cs.new_program_graph();
        let initial1 = cs.new_initial_location(pg1)?;
        let post1 = cs.new_location(pg1)?;
        let effect = CsExpression::from(true);
        let send = cs.new_send(pg1, ch, vec![effect.clone()])?;
        let _ = cs.new_send(pg1, ch, vec![effect])?;
        cs.add_transition(pg1, initial1, send, post1, None)?;

        let var1 = cs.new_var(pg1, Val::from(0i64))?;
        let effect = CsExpression::from(0i64);
        cs.add_effect(pg1, send, var1, effect)
            .expect_err("send is a message so it cannot have effects");

        let pg2 = cs.new_program_graph();
        let initial2 = cs.new_initial_location(pg2)?;
        let post2 = cs.new_location(pg2)?;
        let var2 = cs.new_var(pg2, Val::from(false))?;
        let receive = cs.new_receive(pg2, ch, vec![var2])?;
        let _ = cs.new_receive(pg2, ch, vec![var2])?;
        let _ = cs.new_receive(pg2, ch, vec![var2])?;
        cs.add_transition(pg2, initial2, receive, post2, None)?;

        let cs_def = cs.build();
        let mut cs = cs_def.new_instance();
        // assert_eq!(cs.possible_transitions().count(), 1);
        assert_eq!(cs.def().communications_pg_idxs, vec![0, 2, 5]);

        cs.transition(pg1, send, &[post1])?;
        cs.transition(pg2, receive, &[post2])?;
        // assert_eq!(cs.possible_transitions().count(), 0);
        Ok(())
    }
}
