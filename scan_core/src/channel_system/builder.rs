use super::{
    Action, Channel, ChannelSystem, Clock, CsError, Location, Message, PgError, PgExpression, PgId,
    ProgramGraph, ProgramGraphBuilder, TimeConstraint, Var,
};
use crate::grammar::Type;
use crate::Expression;
use ahash::AHashMap;
use log::info;
use std::collections::VecDeque;
use std::sync::Arc;

/// An expression using CS's [`Var`] as variables.
pub type CsExpression = Expression<Var>;

// WARN: This method should probably not be exposed to the public API.
// TODO: Turn into a private method.
impl TryFrom<(PgId, CsExpression)> for PgExpression {
    type Error = CsError;

    fn try_from((pg_id, expr): (PgId, CsExpression)) -> Result<Self, Self::Error> {
        match expr {
            Expression::Const(val) => Ok(Expression::Const(val)),
            Expression::Var(cs_var, t) if cs_var.0 == pg_id => Ok(Expression::Var(cs_var.1, t)),
            Expression::Var(cs_var, _t) => Err(CsError::VarNotInPg(cs_var, pg_id)),
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
            Expression::Append(comps) => Ok(Expression::Append(Box::new((
                (pg_id, comps.0).try_into()?,
                (pg_id, comps.1).try_into()?,
            )))),
            Expression::Truncate(comp) => {
                Ok(Expression::Truncate(Box::new((pg_id, *comp).try_into()?)))
            }
            Expression::Len(comp) => Ok(Expression::Len(Box::new((pg_id, *comp).try_into()?))),
            Expression::Mod(comps) => Ok(Expression::Mod(Box::new((
                (pg_id, comps.0).try_into()?,
                (pg_id, comps.1).try_into()?,
            )))),
        }
    }
}

/// The object used to define and build a CS.
#[derive(Debug, Default, Clone)]
pub struct ChannelSystemBuilder {
    program_graphs: Vec<ProgramGraphBuilder>,
    channels: Vec<(Type, Option<usize>)>,
    communications: AHashMap<Action, (Channel, Message)>,
}

impl ChannelSystemBuilder {
    /// Create a new [`ProgramGraphBuilder`].
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

    /// Get the initial location of the given PG.
    ///
    /// It fails if the CS contains no such PG.
    pub fn initial_location(&mut self, pg_id: PgId) -> Result<Location, CsError> {
        let pg = self
            .program_graphs
            .get(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))?;
        let initial = Location(pg_id, pg.initial_location());
        Ok(initial)
    }

    /// Add a new variable of the given type to the given PG.
    ///
    /// It fails if the CS contains no such PG, or if the expression is badly-typed.
    ///
    /// See [`ProgramGraphBuilder::new_var`] for more info.
    pub fn new_var(&mut self, pg_id: PgId, init: CsExpression) -> Result<Var, CsError> {
        let pg = self
            .program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))?;
        let init = PgExpression::try_from((pg_id, init))?;
        let var = pg
            .new_var(init)
            .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
        Ok(Var(pg_id, var))
    }

    pub fn new_clock(&mut self, pg_id: PgId) -> Result<Clock, CsError> {
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| Clock(pg_id, pg.new_clock()))
    }

    /// Adds a new action to the given PG.
    ///
    /// It fails if the CS contains no such PG.
    ///
    /// See also [`ProgramGraphBuilder::new_action`].
    pub fn new_action(&mut self, pg_id: PgId) -> Result<Action, CsError> {
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| Action(pg_id, pg.new_action()))
    }

    /// Add an effect to the given action of the given PG.
    /// It fails if:
    ///
    /// - the CS contains no such PG;
    /// - the given action does not belong to it;
    /// - the given variable does not belong to it;
    /// - trying to add an effect to a communication action.
    ///
    /// ```
    /// # use scan_core::*;
    /// # use scan_core::channel_system::*;
    /// // Create a new CS builder
    /// let mut cs_builder = ChannelSystemBuilder::new();
    ///
    /// // Add a new PG to the CS
    /// let pg = cs_builder.new_program_graph();
    ///
    /// // Create new channel
    /// let chn = cs_builder.new_channel(Type::Integer, Some(1));
    ///
    /// // Create new send communication action
    /// let send = cs_builder
    ///     .new_send(pg, chn, CsExpression::from(1))
    ///     .expect("always possible to add new actions");
    ///
    /// // Add new variable to pg
    /// let var = cs_builder
    ///     .new_var(pg, Expression::from(0))
    ///     .expect("always possible to add new variable");
    ///
    /// // It is not allowed to associate effects to communication actions
    /// cs_builder.add_effect(pg, send, var, Expression::from(1))
    ///     .expect_err("cannot add effect to receive, which is a communication");
    /// ```
    ///
    /// See [`ProgramGraphBuilder::add_effect`] for more info.
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

    pub fn reset_clock(
        &mut self,
        pg_id: PgId,
        action: Action,
        clock: Clock,
    ) -> Result<(), CsError> {
        if action.0 != pg_id {
            Err(CsError::ActionNotInPg(action, pg_id))
        } else if clock.0 != pg_id {
            Err(CsError::DifferentPgs(pg_id, clock.0))
        } else {
            self.program_graphs
                .get_mut(pg_id.0)
                .ok_or(CsError::MissingPg(pg_id))
                .and_then(|pg| {
                    pg.reset_clock(action.1, clock.1)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))
                })
        }
    }

    /// Adds a new location to the given PG.
    ///
    /// It fails if the CS contains no such PG.
    ///
    /// See also [`ProgramGraphBuilder::new_location`].
    pub fn new_location(&mut self, pg_id: PgId) -> Result<Location, CsError> {
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| Location(pg_id, pg.new_location()))
    }

    /// TODO
    pub fn new_timed_location(
        &mut self,
        pg_id: PgId,
        invariants: &[TimeConstraint],
    ) -> Result<Location, CsError> {
        let invariants = invariants
            .iter()
            .map(|(c, l, u)| {
                if c.0 == pg_id {
                    Ok((c.1, *l, *u))
                } else {
                    Err(CsError::DifferentPgs(pg_id, c.0))
                }
            })
            .collect::<Result<Vec<_>, CsError>>()?;
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| Location(pg_id, pg.new_timed_location(&invariants)))
    }

    /// Adds a transition to the PG.
    ///
    /// Fails if the CS contains no such PG, or if the given action, variable or locations do not belong to it.
    ///
    /// See also [`ProgramGraphBuilder::add_transition`].
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

    pub fn add_timed_transition(
        &mut self,
        pg_id: PgId,
        pre: Location,
        action: Action,
        post: Location,
        guard: Option<CsExpression>,
        constraints: &[TimeConstraint],
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
            let constraints = constraints
                .iter()
                .map(|(c, l, u)| {
                    if c.0 == pg_id {
                        Ok((c.1, *l, *u))
                    } else {
                        Err(CsError::DifferentPgs(pg_id, c.0))
                    }
                })
                .collect::<Result<Vec<_>, CsError>>()?;
            self.program_graphs
                .get_mut(pg_id.0)
                .ok_or(CsError::MissingPg(pg_id))
                .and_then(|pg| {
                    pg.add_timed_transition(pre.1, action.1, post.1, guard, &constraints)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))
                })
        }
    }

    /// Adds an autonomous transition to the PG.
    ///
    /// Fails if the CS contains no such PG, or if the given variable or locations do not belong to it.
    ///
    /// See also [`ProgramGraphBuilder::add_transition`].
    pub fn add_autonomous_transition(
        &mut self,
        pg_id: PgId,
        pre: Location,
        post: Location,
        guard: Option<CsExpression>,
    ) -> Result<(), CsError> {
        if pre.0 != pg_id {
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
                    pg.add_autonomous_transition(pre.1, post.1, guard)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))
                })
        }
    }

    pub fn add_autonomous_timed_transition(
        &mut self,
        pg_id: PgId,
        pre: Location,
        post: Location,
        guard: Option<CsExpression>,
        constraints: &[TimeConstraint],
    ) -> Result<(), CsError> {
        if pre.0 != pg_id {
            Err(CsError::LocationNotInPg(pre, pg_id))
        } else if post.0 != pg_id {
            Err(CsError::LocationNotInPg(post, pg_id))
        } else {
            // Turn CsExpression into a PgExpression for Program Graph pg_id
            let guard = guard
                .map(|guard| PgExpression::try_from((pg_id, guard)))
                .transpose()?;
            let constraints = constraints
                .iter()
                .map(|(c, l, u)| {
                    if c.0 == pg_id {
                        Ok((c.1, *l, *u))
                    } else {
                        Err(CsError::DifferentPgs(pg_id, c.0))
                    }
                })
                .collect::<Result<Vec<_>, CsError>>()?;
            self.program_graphs
                .get_mut(pg_id.0)
                .ok_or(CsError::MissingPg(pg_id))
                .and_then(|pg| {
                    pg.add_autonomous_timed_transition(pre.1, post.1, guard, &constraints)
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

    /// Adds a new Send communication action to the given PG.
    ///
    /// Fails if the channel and message types do not match.
    pub fn new_send(
        &mut self,
        pg_id: PgId,
        channel: Channel,
        msg: CsExpression,
    ) -> Result<Action, CsError> {
        let channel_type = self
            .channels
            .get(channel.0)
            .ok_or(CsError::MissingChannel(channel))?
            .0
            .to_owned();
        let msg = PgExpression::try_from((pg_id, msg))?;
        let message_type = msg.r#type().map_err(CsError::Type)?;
        if channel_type != message_type {
            Err(CsError::ProgramGraph(pg_id, PgError::TypeMismatch))
        } else {
            let action = self.program_graphs[pg_id.0]
                .new_send(msg)
                .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
            let action = Action(pg_id, action);
            self.communications.insert(action, (channel, Message::Send));
            Ok(action)
        }
    }

    /// Adds a new Receive communication action to the given PG.
    ///
    /// Fails if the channel and message types do not match.
    pub fn new_receive(
        &mut self,
        pg_id: PgId,
        channel: Channel,
        var: Var,
    ) -> Result<Action, CsError> {
        if pg_id != var.0 {
            Err(CsError::VarNotInPg(var, pg_id))
        } else {
            let channel_type = self
                .channels
                .get(channel.0)
                .ok_or(CsError::MissingChannel(channel))?
                .0
                .to_owned();
            let message_type = self
                .program_graphs
                .get(pg_id.0)
                .ok_or(CsError::MissingPg(pg_id))?
                .var_type(var.1)
                .map_err(|err| CsError::ProgramGraph(pg_id, err))?
                .to_owned();
            if channel_type != message_type {
                Err(CsError::ProgramGraph(pg_id, PgError::TypeMismatch))
            } else {
                let action = self.program_graphs[pg_id.0]
                    .new_receive(var.1)
                    .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
                let action = Action(pg_id, action);
                self.communications
                    .insert(action, (channel, Message::Receive));
                Ok(action)
            }
        }
    }

    /// Adds a new ProbeEmptyQueue communication action to the given PG.
    ///
    /// Fails if the queue uses the handshake protocol.
    pub fn new_probe_empty_queue(
        &mut self,
        pg_id: PgId,
        channel: Channel,
    ) -> Result<Action, CsError> {
        let (_, cap) = self
            .channels
            .get(channel.0)
            .ok_or(CsError::MissingChannel(channel))?;
        if matches!(cap, Some(0)) {
            // it makes no sense to probe an handshake channel
            Err(CsError::ProbingHandshakeQueue(channel))
        } else {
            let action = self
                .program_graphs
                .get_mut(pg_id.0)
                .ok_or(CsError::MissingPg(pg_id))?
                .new_action();
            let action = Action(pg_id, action);
            self.communications
                .insert(action, (channel, Message::ProbeEmptyQueue));
            Ok(action)
        }
    }

    /// Adds a new ProbeFullQueue communication action to the given PG.
    ///
    /// Fails if the queue uses the handshake protocol or it has infinite capacity.
    pub fn new_probe_full_queue(
        &mut self,
        pg_id: PgId,
        channel: Channel,
    ) -> Result<Action, CsError> {
        let (_, cap) = self
            .channels
            .get(channel.0)
            .ok_or(CsError::MissingChannel(channel))?;
        if matches!(cap, Some(0)) {
            // it makes no sense to probe an handshake channel
            Err(CsError::ProbingHandshakeQueue(channel))
        } else if cap.is_none() {
            // it makes no sense to probe for fullness an handshake channel
            Err(CsError::ProbingInfiniteQueue(channel))
        } else {
            let action = self
                .program_graphs
                .get_mut(pg_id.0)
                .ok_or(CsError::MissingPg(pg_id))?
                .new_action();
            let action = Action(pg_id, action);
            self.communications
                .insert(action, (channel, Message::ProbeFullQueue));
            Ok(action)
        }
    }

    /// Produces a [`ChannelSystem`] defined by the [`ChannelSystemBuilder`]'s data and consuming it.
    pub fn build(mut self) -> ChannelSystem {
        info!(
            "create Channel System with:\n{} Program Graphs\n{} channels",
            self.program_graphs.len(),
            self.channels.len(),
        );
        self.program_graphs.shrink_to_fit();
        let mut program_graphs: Vec<ProgramGraph> = self
            .program_graphs
            .into_iter()
            .map(|builder| builder.build())
            .collect();

        program_graphs.shrink_to_fit();
        self.channels.shrink_to_fit();
        self.communications.shrink_to_fit();
        let message_queue = self
            .channels
            .iter()
            .map(|(_, cap)| {
                if let Some(cap) = cap {
                    VecDeque::with_capacity(*cap)
                } else {
                    VecDeque::default()
                }
            })
            .collect();

        ChannelSystem {
            time: 0,
            program_graphs,
            communications: Arc::new(self.communications),
            message_queue,
            channels: Arc::new(self.channels),
        }
    }
}
