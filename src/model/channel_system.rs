use super::grammar::*;

use std::{collections::HashMap, error::Error, fmt, rc::Rc};

use crate::{
    Action, Location, PgError, PgExpression, ProgramGraph, ProgramGraphBuilder, Type, Val, Var,
};

// Use of "Newtype" pattern to define different types of indexes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct PgId(usize);

// Use of "Newtype" pattern to define different types of indexes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Channel(usize);

// Use of "Newtype" pattern to define different types of indexes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct CsLocation(PgId, Location);

// Use of "Newtype" pattern to define different types of indexes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct CsAction(PgId, Action);

// Use of "Newtype" pattern to define different types of indexes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct CsVar(PgId, Var);

pub type CsExpression = super::grammar::Expression<CsVar>;

impl TryFrom<(PgId, CsExpression)> for PgExpression {
    type Error = CsError;

    fn try_from((pg_id, expr): (PgId, CsExpression)) -> Result<Self, Self::Error> {
        match expr {
            Expression::Const(val) => Ok(Expression::Const(val)),
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

#[derive(Debug, Clone)]
pub enum Message {
    Send(CsExpression),
    Receive(CsVar),
    ProbeEmptyQueue,
}

#[derive(Debug, Clone)]
pub enum CsError {
    ProgramGraph(PgId, PgError),
    MissingPg(PgId),
    OutOfCapacity(Channel),
    Empty(Channel),
    NoCommunication(CsAction),
    ActionNotInPg(CsAction, PgId),
    VarNotInPg(CsVar, PgId),
    LocationNotInPg(CsLocation, PgId),
    DifferentPgs(PgId, PgId),
    ActionIsCommunication(CsAction),
    MissingChannel(Channel),
    NotATuple,
    BadIndex,
}

// TODO: switch to `thiserror` crate
impl fmt::Display for CsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CsError::ProgramGraph(pg_id, _) => write!(f, "error from program graph {:?}", pg_id),
            CsError::MissingPg(pg_id) => {
                write!(
                    f,
                    "program graph {:?} does not belong to the channel system",
                    pg_id
                )
            }
            CsError::OutOfCapacity(channel) => {
                write!(f, "channel {:?} is at full capacity", channel)
            }
            CsError::Empty(channel) => write!(f, "channel {:?} is empty", channel),
            CsError::NoCommunication(comm) => {
                write!(f, "communication {:?} has not been defined", comm)
            }
            CsError::ActionNotInPg(action, pg_id) => write!(
                f,
                "action {:?} does not belong to program graph {:?}",
                action, pg_id
            ),
            CsError::VarNotInPg(var, pg_id) => write!(
                f,
                "variable {:?} does not belong to program graph {:?}",
                var, pg_id
            ),
            CsError::LocationNotInPg(location, pg_id) => write!(
                f,
                "location {:?} does not belong to program graph {:?}",
                location, pg_id
            ),
            CsError::DifferentPgs(id1, id2) => {
                write!(f, "program graphs {id1:?} and {id2:?} do not match")
            }
            CsError::ActionIsCommunication(action) => {
                write!(f, "action {action:?} is a communication")
            }
            CsError::MissingChannel(channel) => write!(f, "channel {channel:?} does not exists"),
            CsError::NotATuple => todo!(),
            CsError::BadIndex => todo!(),
        }
    }
}

impl Error for CsError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            CsError::ProgramGraph(_, err) => Some(err),
            _ => None,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ChannelSystemBuilder {
    program_graphs: Vec<ProgramGraphBuilder>,
    channels: Vec<(Type, Option<usize>)>,
    communications: HashMap<CsAction, (Channel, Message)>,
}

impl ChannelSystemBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_program_graph(&mut self) -> PgId {
        let pg_id = PgId(self.program_graphs.len());
        let pg = ProgramGraphBuilder::new();
        self.program_graphs.push(pg);
        pg_id
    }

    pub fn initial_location(&mut self, pg_id: PgId) -> Result<CsLocation, CsError> {
        let pg = self
            .program_graphs
            .get(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))?;
        let initial = CsLocation(pg_id, pg.initial_location());
        Ok(initial)
    }

    pub fn new_var(&mut self, pg_id: PgId, var_type: Type) -> Result<CsVar, CsError> {
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| CsVar(pg_id, pg.new_var(var_type)))
    }

    pub fn new_action(&mut self, pg_id: PgId) -> Result<CsAction, CsError> {
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| CsAction(pg_id, pg.new_action()))
    }

    pub fn add_effect(
        &mut self,
        pg_id: PgId,
        action: CsAction,
        var: CsVar,
        effect: CsExpression,
    ) -> Result<(), CsError> {
        if action.0 != pg_id {
            return Err(CsError::ActionNotInPg(action, pg_id));
        }
        if var.0 != pg_id {
            return Err(CsError::VarNotInPg(var, pg_id));
        }
        let effect = PgExpression::try_from((pg_id, effect))?;
        // Communications cannot have effects
        if self.communications.contains_key(&action) {
            return Err(CsError::ActionIsCommunication(action));
        }
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .and_then(|pg| {
                pg.add_effect(action.1, var.1, effect)
                    .map_err(|err| CsError::ProgramGraph(pg_id, err))
            })
    }

    pub fn new_location(&mut self, pg_id: PgId) -> Result<CsLocation, CsError> {
        self.program_graphs
            .get_mut(pg_id.0)
            .ok_or(CsError::MissingPg(pg_id))
            .map(|pg| CsLocation(pg_id, pg.new_location()))
    }

    pub fn add_transition(
        &mut self,
        pg_id: PgId,
        pre: CsLocation,
        action: CsAction,
        post: CsLocation,
        guard: Option<CsExpression>,
    ) -> Result<(), CsError> {
        if action.0 != pg_id {
            return Err(CsError::ActionNotInPg(action, pg_id));
        }
        if pre.0 != pg_id {
            return Err(CsError::LocationNotInPg(pre, pg_id));
        }
        if post.0 != pg_id {
            return Err(CsError::LocationNotInPg(post, pg_id));
        }
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

    pub fn new_channel(&mut self, var_type: Type, capacity: Option<usize>) -> Channel {
        let channel = Channel(self.channels.len());
        self.channels.push((var_type, capacity));
        channel
    }

    pub fn new_communication(
        &mut self,
        pg_id: PgId,
        channel: Channel,
        message: Message,
    ) -> Result<CsAction, CsError> {
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
                }
                self.program_graphs
                    .get((var.0).0)
                    .ok_or(CsError::MissingPg(var.0))?
                    .var_type(var.1)
                    .map_err(|err| CsError::ProgramGraph(pg_id, err))?
            }
            Message::ProbeEmptyQueue => {
                // There is no type to check so the message is always the right type
                channel_type.to_owned()
            }
        };
        if channel_type != message_type {
            return Err(CsError::ProgramGraph(pg_id, PgError::Mismatched));
        }
        let action = self.new_action(pg_id)?;
        self.communications.insert(action, (channel, message));
        Ok(action)
    }

    pub fn build(mut self) -> ChannelSystem {
        self.program_graphs.shrink_to_fit();
        self.channels.shrink_to_fit();
        self.communications.shrink_to_fit();
        ChannelSystem {
            program_graphs: self
                .program_graphs
                .into_iter()
                .map(|builder| builder.build())
                .collect(),
            communications: Rc::new(self.communications),
            message_queue: vec![Vec::default(); self.channels.len()],
            channels: Rc::new(self.channels),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ChannelSystem {
    program_graphs: Vec<ProgramGraph>,
    channels: Rc<Vec<(Type, Option<usize>)>>,
    communications: Rc<HashMap<CsAction, (Channel, Message)>>,
    message_queue: Vec<Vec<Val>>,
}

impl ChannelSystem {
    // Is this function optimized? Does it unnecessarily copy data?
    pub fn possible_transitions(&self) -> Vec<(PgId, CsAction, CsLocation)> {
        self.program_graphs
            .iter()
            .enumerate()
            .flat_map(|(id, pg)| {
                let pg_id = PgId(id);
                pg.possible_transitions()
                    .iter()
                    .filter_map(|(action, post)| {
                        let action = CsAction(pg_id, *action);
                        let post = CsLocation(pg_id, *post);
                        if self.communications.contains_key(&action)
                            && self.check_communication(pg_id, action).is_err()
                        {
                            None
                        } else {
                            Some((pg_id, action, post))
                        }
                    })
                    .collect::<Vec<(PgId, CsAction, CsLocation)>>()
            })
            .collect::<Vec<(PgId, CsAction, CsLocation)>>()
    }

    fn check_communication(&self, pg_id: PgId, action: CsAction) -> Result<(), CsError> {
        if action.0 != pg_id {
            return Err(CsError::ActionNotInPg(action, pg_id));
        }
        if let Some((channel, message)) = self.communications.get(&action) {
            let (_, capacity) = self
                .channels
                .get(channel.0)
                .expect("communication has been verified before");
            let queue = self
                .message_queue
                .get(channel.0)
                .expect("communication has been verified before");
            match message {
                Message::Send(_) => {
                    let len = queue.len();
                    // Channel capacity must never be exeeded!
                    assert!(capacity.is_none() || capacity.is_some_and(|c| len <= c));
                    if capacity.is_some_and(|c| c == len) {
                        Err(CsError::OutOfCapacity(*channel))
                    } else {
                        Ok(())
                    }
                }
                Message::Receive(_) => {
                    if queue.is_empty() {
                        Err(CsError::Empty(*channel))
                    } else {
                        Ok(())
                    }
                }
                Message::ProbeEmptyQueue => {
                    if queue.is_empty() {
                        Ok(())
                    } else {
                        Err(CsError::Empty(*channel))
                    }
                }
            }
        } else {
            Err(CsError::NoCommunication(action))
        }
    }

    pub fn transition(
        &mut self,
        pg_id: PgId,
        action: CsAction,
        post: CsLocation,
    ) -> Result<(), CsError> {
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
            let queue = self
                .message_queue
                .get_mut(channel.0)
                .expect("communication has been verified before");
            match message {
                Message::Send(effect) => {
                    let effect = (pg_id, effect.to_owned()).try_into()?;
                    let val = pg
                        .eval(&effect)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
                    queue.push(val);
                }
                Message::Receive(var) => {
                    let val = queue.pop().expect("communication has been verified before");
                    pg.assign(var.1, val)
                        .expect("communication has been verified before");
                }
                Message::ProbeEmptyQueue => {
                    assert!(
                        queue.is_empty(),
                        "by definition, ProbeEmptyQueue is only possible if the queue is empty"
                    );
                }
            }
        }
        Ok(())
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
        let var1 = cs.new_var(pg, Type::Boolean)?;
        let var2 = cs.new_var(pg, Type::Integer)?;
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
        let msg = Message::Send(effect);
        let send = cs.new_communication(pg1, ch, msg)?;
        cs.add_transition(pg1, initial1, send, post1, None)?;

        let var1 = cs.new_var(pg1, Type::Integer)?;
        let effect = CsExpression::Const(Val::Integer(0));
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
        assert_eq!(cs.possible_transitions().len(), 1);

        cs.transition(pg1, send, post1)?;
        cs.transition(pg2, receive, post2)?;
        assert!(cs.possible_transitions().is_empty());
        Ok(())
    }
}
