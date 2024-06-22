//! Implementation of the CS model of computation.
//!
//! Channel systems comprises multiple program graphs executing asynchronously
//! while sending and retreiving messages from channels.
//!
//! Analogously to PGs, a CS is defined through a [`ChannelSystemBuilder`],
//! by adding new PGs and channels.
//! Each PG in the CS can be given new locations, actions, effects, guards and transitions.
//! Then, a [`ChannelSystem`] is built from the [`ChannelSystemBuilder`]
//! and can be executed by performing transitions,
//! though the definition of the CS itself can no longer be altered.

use log::info;
use thiserror::Error;

use crate::grammar::*;
use crate::program_graph::{Action as PgAction, Location as PgLocation, Var as PgVar, *};
use std::{collections::HashMap, rc::Rc};

/// An indexing object for PGs in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct PgId(usize);

/// An indexing object for channels in a CS.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ChannelSystemBuilder`] or [`ChannelSystem`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Channel(usize);

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

/// An expression using CS's [`CsVar`] as variables.
pub type CsExpression = super::grammar::Expression<Var>;

impl TryFrom<(PgId, CsExpression)> for PgExpression {
    type Error = CsError;

    fn try_from((pg_id, expr): (PgId, CsExpression)) -> Result<Self, Self::Error> {
        match expr {
            Expression::Boolean(b) => Ok(Expression::Boolean(b)),
            Expression::Integer(i) => Ok(Expression::Integer(i)),
            Expression::Var(cs_var) if cs_var.0 == pg_id => Ok(Expression::Var(cs_var.1)),
            Expression::Var(cs_var) => Err(CsError::VarNotInPg(cs_var, pg_id)),
            Expression::Tuple(comps) => Ok(Expression::Tuple(
                comps
                    .into_iter()
                    .map(|comp| (pg_id, comp).try_into())
                    .collect::<Result<Vec<PgExpression>, CsError>>()?,
            )),
            Expression::Component(index, expr) => (pg_id, *expr)
                .try_into()
                .map(|expr| Expression::Component(index, Box::new(expr))),
            Expression::And(comps) => Ok(Expression::And(
                comps
                    .into_iter()
                    .map(|comp| (pg_id, comp).try_into())
                    .collect::<Result<Vec<PgExpression>, CsError>>()?,
            )),
            Expression::Or(comps) => Ok(Expression::Or(
                comps
                    .into_iter()
                    .map(|comp| (pg_id, comp).try_into())
                    .collect::<Result<Vec<PgExpression>, CsError>>()?,
            )),
            Expression::Implies(comps) => Ok(Expression::Implies(Box::new((
                (pg_id, comps.0).try_into()?,
                (pg_id, comps.1).try_into()?,
            )))),
            Expression::Not(expr) => (pg_id, *expr).try_into().map(Box::new).map(Expression::Not),
            Expression::Opposite(expr) => (pg_id, *expr)
                .try_into()
                .map(Box::new)
                .map(Expression::Opposite),
            Expression::Sum(comps) => Ok(Expression::Sum(
                comps
                    .into_iter()
                    .map(|comp| (pg_id, comp).try_into())
                    .collect::<Result<Vec<PgExpression>, CsError>>()?,
            )),
            Expression::Mult(comps) => Ok(Expression::Mult(
                comps
                    .into_iter()
                    .map(|comp| (pg_id, comp).try_into())
                    .collect::<Result<Vec<PgExpression>, CsError>>()?,
            )),
            Expression::Equal(comps) => Ok(Expression::Equal(Box::new((
                (pg_id, comps.0).try_into()?,
                (pg_id, comps.1).try_into()?,
            )))),
            Expression::Greater(comps) => Ok(Expression::Greater(Box::new((
                (pg_id, comps.0).try_into()?,
                (pg_id, comps.1).try_into()?,
            )))),
            Expression::GreaterEq(comps) => Ok(Expression::GreaterEq(Box::new((
                (pg_id, comps.0).try_into()?,
                (pg_id, comps.1).try_into()?,
            )))),
            Expression::Less(comps) => Ok(Expression::Less(Box::new((
                (pg_id, comps.0).try_into()?,
                (pg_id, comps.1).try_into()?,
            )))),
            Expression::LessEq(comps) => Ok(Expression::LessEq(Box::new((
                (pg_id, comps.0).try_into()?,
                (pg_id, comps.1).try_into()?,
            )))),
        }
    }
}

/// A message to be sent through a CS's channel.
#[derive(Debug, Clone)]
pub enum Message {
    /// Sending the computed value of an expression to a channel.
    Send(CsExpression),
    /// Retrieving a value out of a channel and associating it to a variable.
    Receive(Var),
    /// Checking whether a channel is empty.
    ProbeEmptyQueue,
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
    /// The channel is empty and there is no message to be retrieved.
    #[error("channel {0:?} is empty")]
    Empty(Channel),
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
}

/// The object used to define and build a CS.
#[derive(Debug, Default, Clone)]
pub struct ChannelSystemBuilder {
    program_graphs: Vec<ProgramGraphBuilder>,
    channels: Vec<(Type, Option<usize>)>,
    communications: HashMap<Action, (Channel, Message)>,
}

impl ChannelSystemBuilder {
    /// Creates a new [`ProgramGraphBuilder`].
    /// At creation, this will be completely empty.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new PG to the CS.
    pub fn new_program_graph(&mut self) -> PgId {
        let pg_id = PgId(self.program_graphs.len());
        let pg = ProgramGraphBuilder::new();
        self.program_graphs.push(pg);
        pg_id
    }

    /// Gets the initial location of the given PG.
    ///
    /// Fails if the CS contains no such PG.
    pub fn initial_location(&mut self, pg_id: PgId) -> Result<Location, CsError> {
        let pg = self
            .program_graphs
            .get(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))?;
        let initial = Location(pg_id, pg.initial_location());
        Ok(initial)
    }

    /// Adds a new variable of the given type to the given PG.
    ///
    /// Fails if the CS contains no such PG.
    pub fn new_var(&mut self, pg_id: PgId, var_type: Type) -> Result<Var, CsError> {
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| Var(pg_id, pg.new_var(var_type)))
    }

    /// Adds a new action to the given PG.
    ///
    /// Fails if the CS contains no such PG.
    pub fn new_action(&mut self, pg_id: PgId) -> Result<Action, CsError> {
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| Action(pg_id, pg.new_action()))
    }

    /// Adds an effect to the given action of the given PG.
    ///
    /// Fails if the CS contains no such PG, or if the given action or variable do not belong to it.
    pub fn add_effect(
        &mut self,
        pg_id: PgId,
        action: Action,
        var: Var,
        effect: CsExpression,
    ) -> Result<(), CsError> {
        if action.0 != pg_id {
            Err(CsError::ActionNotInPg(action, pg_id))
        } else if var.0 != pg_id {
            Err(CsError::VarNotInPg(var, pg_id))
        } else if self.communications.contains_key(&action) {
            // Communications cannot have effects
            Err(CsError::ActionIsCommunication(action))
        } else {
            let effect = PgExpression::try_from((pg_id, effect))?;
            self.program_graphs
                .get_mut(pg_id.0)
                .ok_or(CsError::MissingPg(pg_id))
                .and_then(|pg| {
                    pg.add_effect(action.1, var.1, effect)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))
                })
        }
    }

    /// Adds a new location to the given PG.
    pub fn new_location(&mut self, pg_id: PgId) -> Result<Location, CsError> {
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| Location(pg_id, pg.new_location()))
    }

    /// Adds a transition to the PG.
    ///
    /// Fails if the CS contains no such PG, or if the given action, variable or locations do not belong to it.
    pub fn add_transition(
        &mut self,
        pg_id: PgId,
        pre: Location,
        action: Action,
        post: Location,
        guard: Option<CsExpression>,
    ) -> Result<(), CsError> {
        if action.0 != pg_id {
            Err(CsError::ActionNotInPg(action, pg_id))
        } else if pre.0 != pg_id {
            Err(CsError::LocationNotInPg(pre, pg_id))
        } else if post.0 != pg_id {
            Err(CsError::LocationNotInPg(post, pg_id))
        } else {
            // Turn CsExpression into a PgExpression for Program Graph pg_id
            let guard = guard
                .map(|guard| PgExpression::try_from((pg_id, guard)))
                .transpose()?;
            self.program_graphs
                .get_mut(pg_id.0)
                .ok_or(CsError::MissingPg(pg_id))
                .and_then(|pg| {
                    pg.add_transition(pre.1, action.1, post.1, guard)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))
                })
        }
    }

    /// Adds a new channel of the given type and capacity to the CS.
    ///
    /// - [`None`] capacity means that the channel's capacity is unlimited.
    /// - [`Some(0)`] capacity means the channel uses the handshake protocol (NOT YET IMPLEMENTED!)
    pub fn new_channel(&mut self, var_type: Type, capacity: Option<usize>) -> Channel {
        let channel = Channel(self.channels.len());
        self.channels.push((var_type, capacity));
        channel
    }

    /// Adds a new communication action to the given PG.
    ///
    /// Fails if the channel and message types do not match.
    pub fn new_communication(
        &mut self,
        pg_id: PgId,
        channel: Channel,
        message: Message,
    ) -> Result<Action, CsError> {
        let channel_type = self
            .channels
            .get(channel.0)
            .ok_or(CsError::MissingChannel(channel))?
            .0
            .to_owned();
        let message_type = match &message {
            Message::Send(expr) => self
                .program_graphs
                .get(pg_id.0)
                .ok_or(CsError::MissingPg(pg_id))?
                .r#type(&(pg_id, expr.to_owned()).try_into()?)
                .map_err(|err| CsError::ProgramGraph(pg_id, err))?,
            Message::Receive(var) => {
                if pg_id != var.0 {
                    return Err(CsError::VarNotInPg(*var, pg_id));
                } else {
                    self.program_graphs
                        .get((var.0).0)
                        .ok_or(CsError::MissingPg(var.0))?
                        .var_type(var.1)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))?
                        .to_owned()
                }
            }
            Message::ProbeEmptyQueue => {
                // There is no type to check so the message is always the right type
                channel_type.to_owned()
            }
        };
        if channel_type != message_type {
            return Err(CsError::ProgramGraph(pg_id, PgError::TypeMismatch));
        }
        let action = self.new_action(pg_id)?;
        self.communications.insert(action, (channel, message));
        Ok(action)
    }

    /// Produces a [`Channel System`] defined by the [`ChannelSystemBuilder`]'s data and consuming it.
    pub fn build(mut self) -> ChannelSystem {
        info!(
            "create Channel System with:\n{} Program Graphs\n{} channels",
            self.program_graphs.len(),
            self.channels.len(),
        );
        let mut program_graphs: Vec<ProgramGraph> = self
            .program_graphs
            .into_iter()
            .map(|builder| builder.build())
            .collect();
        let mut communications = HashMap::new();
        for (act, (chn, msg)) in self.communications.into_iter() {
            let msg = match msg {
                Message::Send(expr) => FnMessage::Send(
                    TryInto::<PgExpression>::try_into((act.0, expr))
                        .expect("")
                        .into(),
                ),
                Message::Receive(val) => FnMessage::Receive(val),
                Message::ProbeEmptyQueue => FnMessage::ProbeEmptyQueue,
            };
            communications.insert(act, (chn, msg));
        }

        program_graphs.shrink_to_fit();
        self.channels.shrink_to_fit();
        communications.shrink_to_fit();
        ChannelSystem {
            program_graphs,
            communications: Rc::new(communications),
            message_queue: vec![Vec::default(); self.channels.len()],
            channels: Rc::new(self.channels),
        }
    }
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
}

/// A message to be sent through a CS's channel.
#[derive(Debug)]
enum FnMessage {
    /// Sending the computed value of an expression to a channel.
    Send(FnExpression),
    /// Retrieving a value out of a channel and associating it to a variable.
    Receive(Var),
    /// Checking whether a channel is empty.
    ProbeEmptyQueue,
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
    channels: Rc<Vec<(Type, Option<usize>)>>,
    communications: Rc<HashMap<Action, (Channel, FnMessage)>>,
    message_queue: Vec<Vec<Val>>,
}

impl ChannelSystem {
    /// Iterates over all transitions that can be admitted in the current state.
    ///
    /// An admittable transition is characterized by the PG it executes on, the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The (eventual) guard is guaranteed to be satisfied.
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

    fn check_communication(&self, pg_id: PgId, action: Action) -> Result<(), CsError> {
        if action.0 != pg_id {
            Err(CsError::ActionNotInPg(action, pg_id))
        } else if let Some((channel, message)) = self.communications.get(&action) {
            let (_, capacity) = self.channels[channel.0];
            let queue = &self.message_queue[channel.0];
            // Channel capacity must never be exeeded!
            assert!(capacity.is_none() || capacity.is_some_and(|cap| queue.len() <= cap));
            match message {
                FnMessage::Send(_) if capacity.is_some_and(|cap| queue.len() == cap) => {
                    Err(CsError::OutOfCapacity(*channel))
                }
                FnMessage::Receive(_) if queue.is_empty() => Err(CsError::Empty(*channel)),
                FnMessage::ProbeEmptyQueue if !queue.is_empty() => Err(CsError::Empty(*channel)),
                _ => Ok(()),
            }
        } else {
            Err(CsError::NoCommunication(action))
        }
    }

    /// Executes a transition on the given PG characterized by the argument action and post-state.
    ///
    /// Fails if the requested transition is not admissible.
    pub fn transition(
        &mut self,
        pg_id: PgId,
        action: Action,
        post: Location,
    ) -> Result<Option<Event>, CsError> {
        // If action is a communication, check it is legal
        if self.communications.contains_key(&action) {
            self.check_communication(pg_id, action)?;
        } else if action.0 != pg_id {
            return Err(CsError::ActionNotInPg(action, pg_id));
        }
        if post.0 != pg_id {
            return Err(CsError::LocationNotInPg(post, pg_id));
        }
        // Transition the program graph
        let pg = self
            .program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))?;
        pg.transition(action.1, post.1)
            .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
        // If the action is a communication, send/receive the message
        if let Some((channel, message)) = self.communications.get(&action) {
            // communication has been verified before so there is a queue for channel.0
            let queue = &mut self.message_queue[channel.0];
            let event_type = match message {
                FnMessage::Send(effect) => {
                    // let effect = (pg_id, effect.to_owned()).try_into()?;
                    let val = pg.eval(effect);
                    queue.push(val.clone());
                    EventType::Send(val)
                }
                FnMessage::Receive(var) => {
                    let val = queue.pop().expect("communication has been verified before");
                    pg.assign(var.1, val.clone())
                        .expect("communication has been verified before");
                    EventType::Receive(val)
                }
                FnMessage::ProbeEmptyQueue => {
                    assert!(
                        queue.is_empty(),
                        "by definition, ProbeEmptyQueue is only possible if the queue is empty"
                    );
                    EventType::ProbeEmptyQueue
                }
            };
            Ok(Some(Event {
                pg_id,
                channel: *channel,
                event_type,
            }))
        } else {
            Ok(None)
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
        let _var1 = cs.new_var(pg, Type::Boolean)?;
        let _var2 = cs.new_var(pg, Type::Integer)?;
        Ok(())
    }

    #[test]
    fn add_effect() -> Result<(), CsError> {
        let mut cs = ChannelSystemBuilder::new();
        let pg = cs.new_program_graph();
        let action = cs.new_action(pg)?;
        let var1 = cs.new_var(pg, Type::Boolean)?;
        let var2 = cs.new_var(pg, Type::Integer)?;
        let effect_1 = CsExpression::Integer(2);
        cs.add_effect(pg, action, var1, effect_1.clone())
            .expect_err("type mismatch");
        let effect_2 = CsExpression::Boolean(true);
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
        let var1 = cs.new_var(pg, Type::Boolean)?;
        let var2 = cs.new_var(pg, Type::Integer)?;
        let effect_1 = CsExpression::Integer(0);
        let effect_2 = CsExpression::Boolean(true);
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
        let effect = CsExpression::Boolean(true);
        let msg = Message::Send(effect);
        let send = cs.new_communication(pg1, ch, msg)?;
        cs.add_transition(pg1, initial1, send, post1, None)?;

        let var1 = cs.new_var(pg1, Type::Integer)?;
        let effect = CsExpression::Integer(0);
        cs.add_effect(pg1, send, var1, effect)
            .expect_err("send is a message so it cannot have effects");

        let pg2 = cs.new_program_graph();
        let initial2 = cs.initial_location(pg2)?;
        let post2 = cs.new_location(pg2)?;
        let var2 = cs.new_var(pg2, Type::Boolean)?;
        let msg = Message::Receive(var2);
        let receive = cs.new_communication(pg2, ch, msg)?;
        cs.add_transition(pg2, initial2, receive, post2, None)?;

        let mut cs = cs.build();
        assert_eq!(cs.possible_transitions().count(), 1);

        cs.transition(pg1, send, post1)?;
        cs.transition(pg2, receive, post2)?;
        assert_eq!(cs.possible_transitions().count(), 0);
        Ok(())
    }
}
