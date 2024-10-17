//! Model builder for SCAN's XML specification format.

use crate::parser::{
    Bt, BtNode, Executable, Fsm, If, MoC, OmgType, OmgTypes, Param, Parser, Scxml, Send, Target,
    ACTION_RESPONSE, CONDITION_RESPONSE, FAILURE, HALT_CALL, HALT_RETURN, RESULT, RUNNING, SUCCESS,
    TICK_CALL, TICK_RETURN,
};
use anyhow::anyhow;
use boa_interner::ToInternedString;
use log::{info, trace};
use scan_core::{channel_system::*, *};
use std::collections::{HashMap, HashSet};

// TODO:
//
// -[ ] WARN FIXME System is fragile if name/id/path do not coincide

#[derive(Debug)]
pub struct ScxmlModel {
    pub model: CsModel,
    pub predicates: Vec<String>,
    pub guarantees: Vec<Mtl<usize>>,
    pub assumes: Vec<Mtl<usize>>,
    pub fsm_names: HashMap<PgId, String>,
    pub fsm_indexes: HashMap<usize, String>,
    pub parameters: HashMap<Channel, (PgId, PgId, usize, String)>,
    pub int_queues: HashSet<Channel>,
    pub ext_queues: HashMap<Channel, PgId>,
    pub events: HashMap<usize, String>,
    // TODO: ...other stuff needed to backtrack scxml's ids
}

#[derive(Debug, Clone)]
struct FsmBuilder {
    pg_id: PgId,
    ext_queue: Channel,
}

#[derive(Debug, Clone)]
struct EventBuilder {
    // Associates parameter's name with the id of its type.
    params: HashMap<String, String>,
    senders: HashSet<PgId>,
    receivers: HashSet<PgId>,
    index: usize,
}

#[derive(Debug, Clone)]
enum EcmaObj {
    PrimitiveData(CsExpression, String),
    // Associates property name with content, which can be another object.
    Properties(HashMap<String, EcmaObj>),
}

/// Builder turning a [`Parser`] into a [`ChannelSystem`].
#[derive(Debug)]
pub struct ModelBuilder {
    cs: ChannelSystemBuilder,
    // Associates a type's id with both its OMG type and SCAN type.
    // NOTE: This is necessary because, at the moment, it is not possible to derive one from the other.
    // QUESTION: is there a better way?
    types: HashMap<String, (OmgType, Type)>,
    // Associates an enum's label with a **globally unique** index.
    // The same label can belong to multiple enums,
    // and given a label it is not possible to recover the originating enum.
    // WARN FIXME TODO: simplistic implementation of enums
    enums: HashMap<String, Integer>,
    // Associates a struct's id and field id with the index it is assigned in the struct's representation as a product.
    // NOTE: This is decided arbitrarily and not imposed by the OMG type definition.
    // QUESTION: Is there a better way?
    structs: HashMap<(String, String), usize>,
    // Each State Chart has an associated Program Graph,
    // and an arbitrary, progressive index
    fsm_names: HashMap<PgId, String>,
    fsm_builders: HashMap<String, FsmBuilder>,
    // Each event is associated to a unique global index and parameter(s).
    // WARN FIXME TODO: name clashes
    events: Vec<EventBuilder>,
    event_indexes: HashMap<String, usize>,
    // Events carrying parameters have dedicated channels for them,
    // one for each:
    // - senderStateChart
    // - receiverStateChart
    // - sentEvent (index)
    // - paramName
    // that is needed
    parameters: HashMap<(PgId, PgId, usize, String), Channel>,
    // Properties
    guarantees: HashMap<String, Mtl<String>>,
    assumes: HashMap<String, Mtl<String>>,
    predicates: HashMap<String, Expression<Port>>,
    ports: HashMap<String, (Port, Val)>,
    // extra data
    int_queues: HashSet<Channel>,
}

impl ModelBuilder {
    /// Turns the [`Parser`] into a [`ChannelSystem`].
    ///
    /// Can fail if the model specification contains semantic errors
    /// (particularly type mismatches)
    /// or references to non-existing items.
    pub fn build(parser: Parser) -> anyhow::Result<ScxmlModel> {
        let mut model_builder = ModelBuilder {
            cs: ChannelSystemBuilder::new(),
            types: HashMap::new(),
            enums: HashMap::new(),
            structs: HashMap::new(),
            fsm_names: HashMap::new(),
            fsm_builders: HashMap::new(),
            events: Vec::new(),
            event_indexes: HashMap::new(),
            parameters: HashMap::new(),
            guarantees: HashMap::new(),
            assumes: HashMap::new(),
            predicates: HashMap::new(),
            ports: HashMap::new(),
            int_queues: HashSet::new(),
        };

        info!("Building types");
        model_builder.build_types(&parser.types)?;

        model_builder.prebuild_processes(&parser)?;

        info!("Visit process list");
        for (_id, declaration) in parser.process_list.iter() {
            match &declaration.moc {
                MoC::Fsm(fsm) => model_builder.build_fsm(fsm)?,
                MoC::Bt(bt) => model_builder.build_bt(bt)?,
            }
        }

        model_builder.build_predicates(&parser)?;

        let model = model_builder.build_model();

        Ok(model)
    }

    fn build_types(&mut self, omg_types: &OmgTypes) -> anyhow::Result<()> {
        for (name, omg_type) in omg_types.types.iter() {
            let scan_type = match omg_type {
                OmgType::Boolean => Type::Boolean,
                OmgType::Int32 => Type::Integer,
                OmgType::F64 => Type::Float,
                OmgType::Uri => Type::Integer,
                OmgType::Structure(fields) => {
                    let mut fields_type: Vec<Type> = Vec::new();
                    for (index, (field_id, field_type)) in fields.iter().enumerate() {
                        self.structs
                            .insert((name.to_owned(), field_id.to_owned()), index);
                        // NOTE: fields must have an already known type, to aviod recursion.
                        let (_, field_type) = self.types.get(field_type).ok_or(anyhow!(
                            "unknown type {} of field {} in struct {}",
                            field_type,
                            field_id,
                            name
                        ))?;
                        // NOTE: fields have to be inserted in this order or they will not correspond to their index.
                        fields_type.push(field_type.clone());
                    }
                    Type::Product(fields_type)
                }
                OmgType::Enumeration(labels) => {
                    // NOTE: enum labels are assigned a **globally unique** index,
                    // and the same label can appear in different enums.
                    // This makes it so that SUCCESS and FAILURE from ActionResponse are the same as those in ConditionResponse.
                    for label in labels.iter() {
                        if !self.enums.contains_key(label) {
                            let idx = self.enums.len();
                            self.enums.insert(label.to_owned(), idx as Integer);
                        }
                    }
                    Type::Integer
                }
            };
            self.types
                .insert(name.to_owned(), (omg_type.to_owned(), scan_type));
        }
        Ok(())
    }

    fn event_index(&mut self, id: &str) -> usize {
        self.event_indexes.get(id).cloned().unwrap_or_else(|| {
            let index = self.events.len();
            self.events.push(EventBuilder {
                params: HashMap::new(),
                index,
                senders: HashSet::new(),
                receivers: HashSet::new(),
            });
            self.event_indexes.insert(id.to_owned(), index);
            index
        })
    }

    fn fsm_builder(&mut self, id: &str) -> &FsmBuilder {
        if !self.fsm_builders.contains_key(id) {
            let pg_id = self.cs.new_program_graph();
            let ext_queue = self
                .cs
                .new_channel(Type::Product(vec![Type::Integer, Type::Integer]), None);
            let fsm = FsmBuilder { pg_id, ext_queue };
            self.fsm_builders.insert(id.to_string(), fsm);
            self.fsm_names.insert(pg_id, id.to_string());
        }
        self.fsm_builders.get(id).expect("just inserted")
    }

    fn prebuild_processes(&mut self, parser: &Parser) -> anyhow::Result<()> {
        for (id, declaration) in parser.process_list.iter() {
            let pg_id = self.fsm_builder(id).pg_id;
            match &declaration.moc {
                MoC::Fsm(fsm) => self.prebuild_fsms(pg_id, &fsm.scxml)?,
                MoC::Bt(bt) => self.prebuild_bt(pg_id, bt)?,
            }
        }
        Ok(())
    }

    fn prebuild_fsms(&mut self, pg_id: PgId, fmt: &Scxml) -> anyhow::Result<()> {
        for (_, state) in fmt.states.iter() {
            for exec in state.on_entry.iter() {
                self.prebuild_exec(pg_id, exec)?;
            }
            for transition in state.transitions.iter() {
                if let Some(ref event) = transition.event {
                    // Event may or may not have been processed before
                    let event_index = self.event_index(event);
                    let builder = self.events.get_mut(event_index).expect("index must exist");
                    builder.receivers.insert(pg_id);
                }
                for exec in transition.effects.iter() {
                    self.prebuild_exec(pg_id, exec)?;
                }
            }
            for exec in state.on_exit.iter() {
                self.prebuild_exec(pg_id, exec)?;
            }
        }
        Ok(())
    }

    fn prebuild_exec(&mut self, pg_id: PgId, executable: &Executable) -> anyhow::Result<()> {
        match executable {
            Executable::Assign {
                location: _,
                expr: _,
            } => Ok(()),
            Executable::Raise { event: _ } => Ok(()),
            Executable::Send(Send {
                event,
                target: _,
                params,
            }) => {
                let event_index = self.event_index(event);
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.senders.insert(pg_id);
                for param in params {
                    let prev_type = builder
                        .params
                        .insert(param.name.to_owned(), param.omg_type.to_owned());
                    // Type parameters should not change type
                    if let Some(prev_type) = prev_type {
                        if prev_type != param.omg_type {
                            return Err(anyhow!("type parameter mismatch"));
                        }
                    }
                }
                Ok(())
            }
            Executable::If(If {
                r#elif: elifs,
                r#else,
                ..
            }) => {
                // preprocess all executables
                for (_, executables) in elifs {
                    for executable in executables {
                        self.prebuild_exec(pg_id, executable)?;
                    }
                }
                for executable in r#else {
                    self.prebuild_exec(pg_id, executable)?;
                }
                Ok(())
            }
        }
    }

    fn prebuild_bt(&mut self, pg_id: PgId, bt: &Bt) -> anyhow::Result<()> {
        let event_index = self.event_index(TICK_CALL);
        let builder = self.events.get_mut(event_index).expect("index must exist");
        builder.receivers.insert(pg_id);
        let event_index = self.event_index(TICK_RETURN);
        let builder = self.events.get_mut(event_index).expect("index must exist");
        builder.senders.insert(pg_id);
        // WARN: This (and similar event/parameter names) depends on an arbitrary format convention,
        // could break if format changes!
        builder
            .params
            .insert(RESULT.to_owned(), ACTION_RESPONSE.to_owned());
        let event_index = self.event_index(HALT_CALL);
        let builder = self.events.get_mut(event_index).expect("index must exist");
        builder.receivers.insert(pg_id);
        let event_index = self.event_index(HALT_RETURN);
        let builder = self.events.get_mut(event_index).expect("index must exist");
        builder.senders.insert(pg_id);

        self.prebuild_node(pg_id, &bt.root)
    }

    fn prebuild_node(&mut self, pg_id: PgId, node: &BtNode) -> anyhow::Result<()> {
        match node {
            // | BtNode::MSeq(children)
            // | BtNode::MFbk(children) => {
            BtNode::RSeq(children) | BtNode::RFbk(children) => {
                for child in children {
                    self.prebuild_node(pg_id, child)?;
                }
                Ok(())
            }
            // BtNode::Invr(child) => self.prebuild_node(pg_id, child),
            BtNode::LAct(action) => {
                let event_index = self.event_index(&(action.to_owned() + "_" + TICK_CALL));
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.senders.insert(pg_id);
                let event_index = self.event_index(&(action.to_owned() + "_" + TICK_RETURN));
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.receivers.insert(pg_id);
                builder
                    .params
                    .insert(RESULT.to_owned(), ACTION_RESPONSE.to_owned());
                let event_index = self.event_index(&(action.to_owned() + "_" + HALT_CALL));
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.senders.insert(pg_id);
                let event_index = self.event_index(&(action.to_owned() + "_" + HALT_RETURN));
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.receivers.insert(pg_id);
                Ok(())
            }
            BtNode::LCnd(condition) => {
                let event_index = self.event_index(&(condition.to_owned() + "_" + TICK_CALL));
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.senders.insert(pg_id);
                let event_index = self.event_index(&(condition.to_owned() + "_" + TICK_RETURN));
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.receivers.insert(pg_id);
                builder
                    .params
                    .insert(RESULT.to_owned(), CONDITION_RESPONSE.to_owned());
                Ok(())
            }
        }
    }

    fn build_bt(&mut self, bt: &Bt) -> anyhow::Result<()> {
        trace!("build bt {}", bt.id);
        // Initialize bt.
        let pg_builder = self.fsm_builder(&bt.id);
        let pg_id = pg_builder.pg_id;
        let ext_queue = pg_builder.ext_queue;
        // Locations are relative to what the node receives
        let loc_idle = self.cs.initial_location(pg_id).unwrap();
        let loc_tick = self.cs.new_location(pg_id).unwrap();
        let loc_success = self.cs.new_location(pg_id).unwrap();
        let loc_running = self.cs.new_location(pg_id).unwrap();
        let loc_failure = self.cs.new_location(pg_id).unwrap();
        let loc_halt = self.cs.new_location(pg_id).unwrap();
        let loc_ack = self.cs.new_location(pg_id).unwrap();
        self.build_bt_node(
            pg_id,
            loc_tick,
            loc_success,
            loc_running,
            loc_failure,
            loc_halt,
            loc_ack,
            &bt.root,
        )?;

        let ext_event_var = self
            .cs
            .new_var(
                pg_id,
                CsExpression::Tuple(vec![CsExpression::from(0), CsExpression::from(0)]),
            )
            .expect("{pg_id:?} exists");
        let receive_event = self
            .cs
            .new_receive(pg_id, ext_queue, ext_event_var)
            .unwrap();
        let process_event = self.cs.new_action(pg_id).unwrap();
        let ext_event_index = self.cs.new_var(pg_id, CsExpression::from(0)).unwrap();
        let ext_origin_var = self.cs.new_var(pg_id, CsExpression::from(0)).unwrap();
        self.cs
            .add_effect(
                pg_id,
                process_event,
                ext_event_index,
                CsExpression::Component(
                    0,
                    Box::new(CsExpression::Var(
                        ext_event_var,
                        Type::Product(vec![Type::Integer, Type::Integer]),
                    )),
                ),
            )
            .unwrap();
        self.cs
            .add_effect(
                pg_id,
                process_event,
                ext_origin_var,
                CsExpression::Component(
                    1,
                    Box::new(CsExpression::Var(
                        ext_event_var,
                        Type::Product(vec![Type::Integer, Type::Integer]),
                    )),
                ),
            )
            .unwrap();
        let event_received = self.cs.new_location(pg_id).unwrap();
        self.cs
            .add_transition(pg_id, loc_idle, receive_event, event_received, None)
            .unwrap();
        let event_processed = self.cs.new_location(pg_id).unwrap();
        self.cs
            .add_transition(pg_id, event_received, process_event, event_processed, None)
            .unwrap();

        // TICK
        // Create event.
        let tick_idx = *self.event_indexes.get(TICK_CALL).unwrap() as Integer;
        let tick_return_idx = *self.event_indexes.get(TICK_RETURN).unwrap() as Integer;
        let halt_idx = *self.event_indexes.get(HALT_CALL).unwrap() as Integer;
        let halt_return_idx = *self.event_indexes.get(HALT_RETURN).unwrap() as Integer;
        self.cs
            .add_autonomous_transition(
                pg_id,
                event_processed,
                loc_tick,
                Some(CsExpression::Equal(Box::new((
                    CsExpression::Var(ext_event_index, Type::Integer),
                    CsExpression::from(tick_idx),
                )))),
            )
            .expect("hope this works");
        // HALT
        self.cs
            .add_autonomous_transition(
                pg_id,
                event_processed,
                loc_halt,
                Some(CsExpression::Equal(Box::new((
                    CsExpression::Var(ext_event_index, Type::Integer),
                    CsExpression::from(halt_idx),
                )))),
            )
            .expect("hope this works");

        // Send tick return value
        let result = self
            .cs
            .new_var(pg_id, CsExpression::from(0))
            .expect("must work");
        let send_result_loc = self.cs.new_location(pg_id).unwrap();
        let success = self.cs.new_action(pg_id).unwrap();
        let success_val = *self.enums.get(&(String::from("SUCCESS"))).unwrap();
        self.cs
            .add_effect(pg_id, success, result, Expression::from(success_val))
            .unwrap();
        self.cs
            .add_transition(pg_id, loc_success, success, send_result_loc, None)
            .unwrap();
        let running = self.cs.new_action(pg_id).unwrap();
        let running_val = *self.enums.get(&(String::from("RUNNING"))).unwrap();
        self.cs
            .add_effect(pg_id, running, result, Expression::from(running_val))
            .unwrap();
        self.cs
            .add_transition(pg_id, loc_running, running, send_result_loc, None)
            .unwrap();
        let failure = self.cs.new_action(pg_id).unwrap();
        let failure_val = *self.enums.get(&(String::from("FAILURE"))).unwrap();
        self.cs
            .add_effect(pg_id, failure, result, Expression::from(failure_val))
            .unwrap();
        self.cs
            .add_transition(pg_id, loc_failure, failure, send_result_loc, None)
            .unwrap();

        // Send tickReturn event with result param
        let callers = &self.events.get(tick_idx as usize).unwrap().senders;
        for &caller in callers.iter() {
            let caller_name = self.fsm_names.get(&caller).unwrap();
            let caller_builder = self.fsm_builders.get(caller_name).expect("it must exist");
            let caller_ext_queue = caller_builder.ext_queue;
            let send_event_loc = self.cs.new_location(pg_id).unwrap();
            let param_channel = self
                .parameters
                .entry((pg_id, caller, tick_return_idx as usize, RESULT.to_owned()))
                .or_insert_with(|| self.cs.new_channel(Type::Integer, None));
            let send_result = self
                .cs
                .new_send(
                    pg_id,
                    *param_channel,
                    Expression::Var(result, Type::Integer),
                )
                .unwrap();
            self.cs
                .add_transition(
                    pg_id,
                    send_result_loc,
                    send_result,
                    send_event_loc,
                    Some(Expression::Equal(Box::new((
                        Expression::Var(ext_origin_var, Type::Integer),
                        Expression::from(usize::from(caller) as Integer),
                    )))),
                )
                .unwrap();
            let send_event = self
                .cs
                .new_send(
                    pg_id,
                    caller_ext_queue,
                    Expression::Tuple(vec![
                        Expression::from(tick_return_idx),
                        Expression::from(usize::from(pg_id) as Integer),
                    ]),
                )
                .unwrap();
            self.cs
                .add_transition(pg_id, send_event_loc, send_event, loc_idle, None)
                .unwrap();
        }

        // Send halt acknowledgement
        let callers = &self.events.get(halt_idx as usize).unwrap().senders;
        for &caller in callers.iter() {
            let caller_name = self.fsm_names.get(&caller).unwrap();
            let caller_builder = self.fsm_builders.get(caller_name).expect("it must exist");
            let caller_ext_queue = caller_builder.ext_queue;
            let send_event = self
                .cs
                .new_send(
                    pg_id,
                    caller_ext_queue,
                    Expression::Tuple(vec![
                        Expression::from(halt_return_idx),
                        Expression::from(usize::from(pg_id) as Integer),
                    ]),
                )
                .unwrap();
            self.cs
                .add_transition(
                    pg_id,
                    loc_ack,
                    send_event,
                    loc_idle,
                    Some(Expression::Equal(Box::new((
                        Expression::Var(ext_origin_var, Type::Integer),
                        Expression::from(usize::from(caller) as Integer),
                    )))),
                )
                .unwrap();
        }

        Ok(())
    }

    /// Recursively build a BT node by associating each possible state of the node to a location:
    /// - pt_tick: the parent node has sent a tick
    /// - pt_success: the parent node receives a tick return with state success
    /// - pt_running: the parent node receives a tick return with state running
    /// - pt_failure: the parent node receives a tick return with state failure
    /// - pt_halt: the parent node sends an halt signal
    /// - pt_ack: the parent node receives an ack signal
    ///
    /// Moreover, we consider the following nodes:
    /// - pt_*: parent node
    /// - loc_*: current node (loc=location)
    /// - branch_*: branch/child node
    fn build_bt_node(
        &mut self,
        pg_id: PgId,
        pt_tick: Location,
        pt_success: Location,
        pt_running: Location,
        pt_failure: Location,
        pt_halt: Location,
        pt_ack: Location,
        node: &BtNode,
    ) -> anyhow::Result<()> {
        match node {
            BtNode::RSeq(branches) => {
                let halt_after_failure = self.cs.new_action(pg_id).expect("{pg_id:?} exists");
                let halting_after_failure = self
                    .cs
                    .new_var(pg_id, CsExpression::from(false))
                    .expect("{pg_id:?} exists");
                self.cs
                    .add_effect(
                        pg_id,
                        halt_after_failure,
                        halting_after_failure,
                        CsExpression::from(true),
                    )
                    .expect("hand-picked arguments");
                let failure_after_halting = self.cs.new_action(pg_id).expect("{pg_id:?} exists");
                self.cs
                    .add_effect(
                        pg_id,
                        failure_after_halting,
                        halting_after_failure,
                        CsExpression::from(false),
                    )
                    .expect("hand-picked arguments");

                let mut prev_ack = pt_halt;
                let mut prev_success = pt_tick;
                // this value is irrelevant
                let mut prev_failure = self.cs.new_location(pg_id).unwrap();

                for branch in branches.iter() {
                    let loc_tick = prev_success;
                    let loc_success = self.cs.new_location(pg_id).unwrap();
                    let loc_running = pt_running;
                    let loc_failure = self.cs.new_location(pg_id).unwrap();
                    let loc_halt = prev_ack;
                    let loc_ack = self.cs.new_location(pg_id).unwrap();
                    self.cs
                        .add_transition(pg_id, prev_failure, halt_after_failure, loc_halt, None)
                        .unwrap();
                    self.build_bt_node(
                        pg_id,
                        loc_tick,
                        loc_success,
                        loc_running,
                        loc_failure,
                        loc_halt,
                        loc_ack,
                        branch,
                    )?;
                    prev_success = loc_success;
                    prev_failure = loc_failure;
                    prev_ack = loc_ack;
                }
                // If all children are successful, return success to father node.
                self.cs
                    .add_autonomous_transition(pg_id, prev_success, pt_success, None)
                    .unwrap();
                // If last child fails, return failure to father node.
                self.cs
                    .add_autonomous_transition(pg_id, prev_failure, pt_failure, None)
                    .unwrap();
                // If all children acknowledge halting, return ack to father node.
                self.cs
                    .add_autonomous_transition(
                        pg_id,
                        prev_ack,
                        pt_ack,
                        Some(CsExpression::Not(Box::new(CsExpression::Var(
                            halting_after_failure,
                            Type::Boolean,
                        )))),
                    )
                    .expect("hand-made args");
                // If all children acknowledge halting after a failure, return failure to father node.
                self.cs
                    .add_transition(
                        pg_id,
                        prev_ack,
                        failure_after_halting,
                        pt_failure,
                        Some(CsExpression::Var(halting_after_failure, Type::Boolean)),
                    )
                    .expect("hand-made args");
            }
            BtNode::RFbk(branches) => {
                let halt_after_success = self.cs.new_action(pg_id).expect("{pg_id:?} exists");
                let halting_after_success = self
                    .cs
                    .new_var(pg_id, CsExpression::from(false))
                    .expect("{pg_id:?} exists");
                self.cs
                    .add_effect(
                        pg_id,
                        halt_after_success,
                        halting_after_success,
                        CsExpression::from(true),
                    )
                    .expect("hand-picked arguments");
                let success_after_halting = self.cs.new_action(pg_id).expect("{pg_id:?} exists");
                self.cs
                    .add_effect(
                        pg_id,
                        success_after_halting,
                        halting_after_success,
                        CsExpression::from(false),
                    )
                    .expect("hand-picked arguments");

                let mut prev_ack = pt_halt;
                let mut prev_failure = pt_tick;
                // this value is irrelevant
                let mut prev_success = self.cs.new_location(pg_id).unwrap();

                for branch in branches.iter() {
                    let loc_tick = prev_failure;
                    let loc_failure = self.cs.new_location(pg_id).unwrap();
                    let loc_running = pt_running;
                    let loc_success = self.cs.new_location(pg_id).unwrap();
                    let loc_halt = prev_ack;
                    let loc_ack = self.cs.new_location(pg_id).unwrap();
                    self.cs
                        .add_transition(pg_id, prev_success, halt_after_success, loc_halt, None)
                        .unwrap();
                    self.build_bt_node(
                        pg_id,
                        loc_tick,
                        loc_success,
                        loc_running,
                        loc_failure,
                        loc_halt,
                        loc_ack,
                        branch,
                    )?;
                    prev_success = loc_success;
                    prev_failure = loc_failure;
                    prev_ack = loc_ack;
                }
                self.cs
                    .add_autonomous_transition(pg_id, prev_success, pt_success, None)
                    .unwrap();
                self.cs
                    .add_autonomous_transition(pg_id, prev_failure, pt_failure, None)
                    .unwrap();
                // If all children acknowledge halting, return ack to father node.
                self.cs
                    .add_autonomous_transition(
                        pg_id,
                        prev_ack,
                        pt_ack,
                        Some(CsExpression::Not(Box::new(CsExpression::Var(
                            halting_after_success,
                            Type::Boolean,
                        )))),
                    )
                    .expect("hand-made args");
                // If all children acknowledge halting after a failure, return failure to father node.
                self.cs
                    .add_transition(
                        pg_id,
                        prev_ack,
                        success_after_halting,
                        pt_success,
                        Some(CsExpression::Var(halting_after_success, Type::Boolean)),
                    )
                    .expect("hand-made args");
            }
            // BtNode::MSeq(_branches) => todo!(),
            // BtNode::MFbk(_branches) => todo!(),
            // BtNode::Invr(branch) => {
            //     // Swap success and failure.
            //     self.build_bt_node(
            //         pg_id, pt_tick, pt_failure, pt_running, pt_success, pt_halt, pt_ack, step,
            //         branch,
            //     )?;
            // }
            BtNode::LAct(action) => {
                trace!("building bt leaf {action}");
                let caller_name = self.fsm_names.get(&pg_id).unwrap();
                let caller_builder = self.fsm_builders.get(caller_name).expect("it must exist");
                let ext_queue = caller_builder.ext_queue;
                let target = action;
                let target_builder = self
                    .fsm_builders
                    .get(target)
                    .ok_or(anyhow!("Action/condition {action} not found"))?;
                let target_ext_queue = target_builder.ext_queue;

                // TICK
                let tick_call_idx = *self
                    .event_indexes
                    .get(&(action.to_owned() + "_" + TICK_CALL))
                    .unwrap();
                let send_event = self
                    .cs
                    .new_send(
                        pg_id,
                        target_ext_queue,
                        CsExpression::Tuple(vec![
                            CsExpression::from(tick_call_idx as Integer),
                            CsExpression::from(usize::from(pg_id) as Integer),
                        ]),
                    )
                    .unwrap();
                let tick_sent = self.cs.new_location(pg_id).unwrap();
                self.cs
                    .add_transition(pg_id, pt_tick, send_event, tick_sent, None)
                    .unwrap();
                let tick_response = self
                    .cs
                    .new_var(
                        pg_id,
                        CsExpression::Tuple(vec![CsExpression::from(0), CsExpression::from(0)]),
                    )
                    .expect("{pg_id:?} exists");
                let get_tick_response = self
                    .cs
                    .new_receive(pg_id, ext_queue, tick_response)
                    .expect("hand-made args");
                let got_tick_response = self.cs.new_location(pg_id).expect("{pg_id:?} exists");
                self.cs
                    .add_transition(pg_id, tick_sent, get_tick_response, got_tick_response, None)
                    .expect("hand-made args");
                let tick_response_param_chn = *self
                    .parameters
                    .entry((
                        target_builder.pg_id,
                        pg_id,
                        *self
                            .event_indexes
                            .get(&(action.to_owned() + "_" + TICK_RETURN))
                            .unwrap(),
                        RESULT.to_owned(),
                    ))
                    .or_insert(self.cs.new_channel(Type::Integer, None));
                let tick_response_param = self
                    .cs
                    .new_var(pg_id, CsExpression::from(0))
                    .expect("{pg_id:?} exists");
                let get_tick_response_param = self
                    .cs
                    .new_receive(pg_id, tick_response_param_chn, tick_response_param)
                    .expect("hand-made args");
                let got_tick_response_param =
                    self.cs.new_location(pg_id).expect("{pg_id:?} exists");
                self.cs
                    .add_transition(
                        pg_id,
                        got_tick_response,
                        get_tick_response_param,
                        got_tick_response_param,
                        None,
                    )
                    .expect("hand-made args");
                self.cs
                    .add_autonomous_transition(
                        pg_id,
                        got_tick_response_param,
                        pt_success,
                        Some(CsExpression::Equal(Box::new((
                            CsExpression::Var(tick_response_param, Type::Integer),
                            CsExpression::from(*self.enums.get(SUCCESS).unwrap()),
                        )))),
                    )
                    .expect("hope this works");
                self.cs
                    .add_autonomous_transition(
                        pg_id,
                        got_tick_response_param,
                        pt_failure,
                        Some(CsExpression::Equal(Box::new((
                            CsExpression::Var(tick_response_param, Type::Integer),
                            CsExpression::from(*self.enums.get(FAILURE).unwrap()),
                        )))),
                    )
                    .expect("hope this works");
                self.cs
                    .add_autonomous_transition(
                        pg_id,
                        got_tick_response_param,
                        pt_running,
                        Some(CsExpression::Equal(Box::new((
                            CsExpression::Var(tick_response_param, Type::Integer),
                            CsExpression::from(*self.enums.get(RUNNING).unwrap()),
                        )))),
                    )
                    .expect("hope this works");

                // HALT
                let event = action.to_owned() + "_" + HALT_CALL;
                // Create event, if it does not exist already.
                let event_idx = self.event_index(&event);
                let send_event = self.cs.new_send(
                    pg_id,
                    target_ext_queue,
                    CsExpression::Tuple(vec![
                        CsExpression::from(event_idx as Integer),
                        CsExpression::from(usize::from(pg_id) as Integer),
                    ]),
                )?;
                let halt_sent = self.cs.new_location(pg_id)?;
                self.cs
                    .add_transition(pg_id, pt_halt, send_event, halt_sent, None)?;
                let halt_response = self
                    .cs
                    .new_var(
                        pg_id,
                        CsExpression::Tuple(vec![CsExpression::from(0), CsExpression::from(0)]),
                    )
                    .expect("{pg_id:?} exists");
                let get_halt_response = self
                    .cs
                    .new_receive(pg_id, ext_queue, halt_response)
                    .expect("hand-made args");
                let got_halt_response = pt_ack;
                self.cs
                    .add_transition(pg_id, halt_sent, get_halt_response, got_halt_response, None)
                    .expect("hand-made args");
            }
            BtNode::LCnd(condition) => {
                trace!("building bt leaf {condition}");
                let caller_name = self.fsm_names.get(&pg_id).unwrap();
                let caller_builder = self.fsm_builders.get(caller_name).expect("it must exist");
                let caller_ext_queue = caller_builder.ext_queue;
                let target_builder = self
                    .fsm_builders
                    .get(condition)
                    .ok_or_else(|| anyhow!("Condition {condition} not found"))?;
                let target_ext_queue = target_builder.ext_queue;

                // TICK
                let tick_call_idx = *self
                    .event_indexes
                    .get(&(condition.to_owned() + "_" + TICK_CALL))
                    .unwrap();
                let send_event = self
                    .cs
                    .new_send(
                        pg_id,
                        target_ext_queue,
                        CsExpression::Tuple(vec![
                            CsExpression::from(tick_call_idx as Integer),
                            CsExpression::from(usize::from(pg_id) as Integer),
                        ]),
                    )
                    .unwrap();
                let tick_sent = self.cs.new_location(pg_id).unwrap();
                self.cs
                    .add_transition(pg_id, pt_tick, send_event, tick_sent, None)
                    .unwrap();
                let tick_response = self
                    .cs
                    .new_var(
                        pg_id,
                        CsExpression::Tuple(vec![CsExpression::from(0), CsExpression::from(0)]),
                    )
                    .expect("{pg_id:?} exists");
                let get_tick_response = self
                    .cs
                    .new_receive(pg_id, caller_ext_queue, tick_response)
                    .expect("hand-made args");
                let got_tick_response = self.cs.new_location(pg_id).expect("{pg_id:?} exists");
                self.cs
                    .add_transition(pg_id, tick_sent, get_tick_response, got_tick_response, None)
                    .expect("hand-made args");
                let tick_response_param_chn = *self
                    .parameters
                    .entry((
                        target_builder.pg_id,
                        pg_id,
                        *self
                            .event_indexes
                            .get(&(condition.to_owned() + "_" + TICK_RETURN))
                            .unwrap(),
                        RESULT.to_owned(),
                    ))
                    .or_insert(self.cs.new_channel(Type::Integer, None));
                let tick_response_param = self
                    .cs
                    .new_var(pg_id, CsExpression::from(0))
                    .expect("{pg_id:?} exists");
                let get_tick_response_param = self
                    .cs
                    .new_receive(pg_id, tick_response_param_chn, tick_response_param)
                    .expect("hand-made args");
                let got_tick_response_param =
                    self.cs.new_location(pg_id).expect("{pg_id:?} exists");
                self.cs
                    .add_transition(
                        pg_id,
                        got_tick_response,
                        get_tick_response_param,
                        got_tick_response_param,
                        None,
                    )
                    .expect("hand-made args");
                self.cs
                    .add_autonomous_transition(
                        pg_id,
                        got_tick_response_param,
                        pt_success,
                        Some(CsExpression::Equal(Box::new((
                            CsExpression::Var(tick_response_param, Type::Integer),
                            CsExpression::from(*self.enums.get(SUCCESS).unwrap()),
                        )))),
                    )
                    .expect("hope this works");
                self.cs
                    .add_autonomous_transition(
                        pg_id,
                        got_tick_response_param,
                        pt_failure,
                        Some(CsExpression::Equal(Box::new((
                            CsExpression::Var(tick_response_param, Type::Integer),
                            CsExpression::from(*self.enums.get(FAILURE).unwrap()),
                        )))),
                    )
                    .expect("hope this works");

                // HALT
                let halt_sent = pt_halt;
                let got_halt_response = pt_ack;
                self.cs
                    .add_autonomous_transition(pg_id, halt_sent, got_halt_response, None)
                    .expect("hand-made args");
            }
        }

        Ok(())
    }

    fn build_fsm(&mut self, fsm: &Fsm) -> anyhow::Result<()> {
        let scxml = &fsm.scxml;
        trace!("build fsm {}", scxml.id);
        // Initialize fsm.
        let pg_builder = self
            .fsm_builders
            .get(&scxml.id)
            .unwrap_or_else(|| panic!("builder for {} must already exist", scxml.id));
        let pg_id = pg_builder.pg_id;
        let ext_queue = pg_builder.ext_queue;
        // Initial location of Program Graph.
        let initial_loc = self
            .cs
            .initial_location(pg_id)
            .expect("program graph must exist");
        let mut initialize = None;
        // Initialize variables from datamodel
        // NOTE vars cannot be initialized using previously defined vars because datamodel is an HashMap
        let mut vars = HashMap::new();
        for data in scxml.datamodel.iter() {
            let scan_type = self
                .types
                .get(data.omg_type.as_str())
                .ok_or(anyhow!("unknown type"))?
                .1
                .to_owned();
            let var = self
                .cs
                .new_var(pg_id, CsExpression::Const(scan_type.default_value()))
                .expect("program graph exists!");
            vars.insert(data.id.to_owned(), (var, data.omg_type.to_owned()));
            // Initialize variable with `expr`, if any, by adding it as effect of `initialize` action.
            if let Some(ref expr) = data.expression {
                let expr = self.expression(expr, &fsm.interner, &vars, None, &HashMap::new())?;
                // Initialization has at least an effect, so we need to perform it.
                // Create action if there was none.
                let initialize = *initialize.get_or_insert_with(|| {
                    self.cs.new_action(pg_id).expect("program graph must exist")
                });
                // This might fail if `expr` does not typecheck.
                self.cs.add_effect(pg_id, initialize, var, expr)?;
            }
        }
        // Make vars immutable
        let vars = vars;
        // Transition initializing datamodel variables.
        // After initializing datamodel, transition to location representing point-of-entry of initial state of State Chart.
        let initial_state;
        if let Some(initialize) = initialize {
            initial_state = self.cs.new_location(pg_id).expect("program graph exists!");
            self.cs
                .add_transition(pg_id, initial_loc, initialize, initial_state, None)
                .expect("hand-coded args");
        } else {
            initial_state = initial_loc;
        };
        // Map fsm's state ids to corresponding CS's locations.
        let mut states = HashMap::new();
        // Conventionally, the entry-point for a state is a location associated to the id of the state.
        states.insert(scxml.initial.to_owned(), initial_state);
        // Var representing the current event and origin pair
        let current_event_and_origin_var = self
            .cs
            .new_var(
                pg_id,
                CsExpression::Const(
                    Type::Product(vec![Type::Integer, Type::Integer]).default_value(),
                ),
            )
            .expect("program graph exists!");
        // Var representing the current event
        let current_event_var = self
            .cs
            .new_var(pg_id, CsExpression::from(0))
            .expect("program graph exists!");
        // Variable that will store origin of last processed event.
        let origin_var = self
            .cs
            .new_var(pg_id, CsExpression::from(0))
            .expect("program graph exists!");
        // Implement internal queue
        let int_queue = self.cs.new_channel(Type::Integer, None);
        // This we only need for backtracking.
        let _ = self.int_queues.insert(int_queue);
        let dequeue_int = self
            .cs
            .new_receive(pg_id, int_queue, current_event_var)
            .expect("hand-coded args");
        // For events from the internal queue, origin is self
        let set_int_origin = self.cs.new_action(pg_id).expect("program graph exists!");
        self.cs
            .add_effect(
                pg_id,
                set_int_origin,
                origin_var,
                CsExpression::from(usize::from(pg_id) as Integer),
            )
            .expect("hand-coded args");
        // Implement external queue
        let dequeue_ext = self
            .cs
            .new_receive(pg_id, ext_queue, current_event_and_origin_var)
            .expect("hand-coded args");
        // Process external event to assign event and origin values to respective vars
        let process_ext_event = self.cs.new_action(pg_id)?;
        self.cs
            .add_effect(
                pg_id,
                process_ext_event,
                current_event_var,
                CsExpression::Component(
                    0,
                    Box::new(CsExpression::Var(
                        current_event_and_origin_var,
                        Type::Product(vec![Type::Integer, Type::Integer]),
                    )),
                ),
            )
            .expect("hand-coded args");
        self.cs
            .add_effect(
                pg_id,
                process_ext_event,
                origin_var,
                CsExpression::Component(
                    1,
                    Box::new(CsExpression::Var(
                        current_event_and_origin_var,
                        Type::Product(vec![Type::Integer, Type::Integer]),
                    )),
                ),
            )
            .expect("hand-coded args");

        // Create variables and channels for the storage of the parameters sent by external events.
        let mut param_vars: HashMap<(usize, String), (Var, String)> = HashMap::new();
        let mut param_actions: HashMap<(PgId, usize, String), Action> = HashMap::new();
        for event_builder in self
            .events
            .iter()
            // only consider events that can activate some transition and that some other process is sending.
            .filter(|eb| eb.receivers.contains(&pg_id) && !eb.senders.is_empty())
        {
            let event_index = event_builder.index;
            for (param_name, param_type_name) in event_builder.params.iter() {
                let param_type = self
                    .types
                    .get(param_type_name)
                    .ok_or(anyhow!("type {} not found", param_type_name))?
                    .1
                    .to_owned();
                // Variable where to store parameter.
                let param_var = self
                    .cs
                    .new_var(pg_id, CsExpression::Const(param_type.default_value()))
                    .expect("hand-made input");
                let old = param_vars.insert(
                    (event_index, param_name.to_owned()),
                    (param_var, param_type_name.to_owned()),
                );
                assert!(old.is_none());
                for &sender_id in event_builder.senders.iter() {
                    let chn = self
                        .parameters
                        .entry((sender_id, pg_id, event_index, param_name.to_owned()))
                        // entry may be present if the sender fsm has been built already,
                        // and it might be missing otherwise.
                        .or_insert_with(|| self.cs.new_channel(param_type.to_owned(), None));
                    let read = self
                        .cs
                        .new_receive(pg_id, *chn, param_var)
                        .expect("must work");
                    let old =
                        param_actions.insert((sender_id, event_index, param_name.to_owned()), read);
                    assert!(old.is_none());
                }
            }
        }
        // Make non-mut
        let param_vars = param_vars;
        let param_actions = param_actions;

        // Consider each of the fsm's states
        for (state_id, state) in scxml.states.iter() {
            trace!("build state {}", state_id);
            // Each state is modeled by multiple locations connected by transitions
            // A starting location is used as a point-of-entry to the execution of the state.
            let start_loc = *states
                .entry(state_id.to_owned())
                .or_insert_with(|| self.cs.new_location(pg_id).expect("program graph exists!"));
            // Execute the state's `onentry` executable content
            let mut onentry_loc = start_loc;
            for executable in state.on_entry.iter() {
                // Each executable content attaches suitable transitions to the point-of-entry location
                // and returns the target of such transitions as updated point-of-entry location.
                onentry_loc = self.add_executable(
                    executable,
                    pg_id,
                    int_queue,
                    onentry_loc,
                    &vars,
                    None,
                    &HashMap::new(),
                    &fsm.interner,
                )?;
            }
            // Make immutable
            let onentry_loc = onentry_loc;

            // Location where autonomous/eventless/NULL transitions activate
            let mut null_trans = onentry_loc;
            // Location where internal events are dequeued
            let int_queue_loc = self.cs.new_location(pg_id).expect("program graph exists!");
            // Location where external events are dequeued
            let ext_queue_loc = self.cs.new_location(pg_id).expect("program graph exists!");
            // Location where eventful transitions activate
            let mut eventful_trans = self.cs.new_location(pg_id).expect("program graph exists!");
            // int_origin_loc will not be needed outside of this scope
            {
                // Location where the origin of internal events is set as own.
                let int_origin_loc = self.cs.new_location(pg_id).expect("program graph exists!");
                // Transition dequeueing a new internal event and searching for first active eventful transition
                self.cs
                    .add_transition(pg_id, int_queue_loc, dequeue_int, int_origin_loc, None)
                    .expect("hand-coded args");
                // Transition dequeueing a new internal event and searching for first active eventful transition
                self.cs
                    .add_transition(pg_id, int_origin_loc, set_int_origin, eventful_trans, None)
                    .expect("hand-coded args");
            }
            // Action denoting checking if internal queue is empty;
            // if so, move to external queue.
            // Notice that one and only one of `int_dequeue` and `empty_int_queue` can be executed at a given time.
            // empty_int_queue will not be needed outside of this scope
            {
                let empty_int_queue = self
                    .cs
                    .new_probe_empty_queue(pg_id, int_queue)
                    .expect("hand-coded args");
                self.cs
                    .add_transition(pg_id, int_queue_loc, empty_int_queue, ext_queue_loc, None)
                    .expect("hand-coded args");
            }
            // Location where parameters of events are read into suitable variables.
            let ext_event_processing_param =
                self.cs.new_location(pg_id).expect("program graph exists!");
            // Process external events by reading the (event, origin) pair and writing the components to the designated variables.
            // ext_event_processing_loc will not be needed outside of this scope.
            {
                // Location where the index/origin of external events are dequeued
                let ext_event_processing_loc =
                    self.cs.new_location(pg_id).expect("program graph exists!");
                // Dequeue a new external event and search for first active named transition.
                self.cs
                    .add_transition(
                        pg_id,
                        ext_queue_loc,
                        dequeue_ext,
                        ext_event_processing_loc,
                        None,
                    )
                    .expect("hand-coded args");
                self.cs
                    .add_transition(
                        pg_id,
                        ext_event_processing_loc,
                        process_ext_event,
                        ext_event_processing_param,
                        None,
                    )
                    .expect("hand-coded args");
            }
            // Keep track of all known events.
            let mut known_events = Vec::new();
            // Retreive external event's parameters
            // We need to set up the parameter-passing channel for every possible event that could be sent,
            // from any possible other fsm,
            // and for any parameter of the event.
            for event_builder in self
                .events
                .iter()
                .filter(|eb| eb.receivers.contains(&pg_id) && !eb.senders.is_empty())
            {
                let event_index = event_builder.index;
                for &sender_id in &event_builder.senders {
                    // Expression checking event and sender correspond to the given ones.
                    let is_event_sender = CsExpression::And(vec![
                        CsExpression::Equal(Box::new((
                            CsExpression::from(event_index as Integer),
                            CsExpression::Var(current_event_var, Type::Integer),
                        ))),
                        CsExpression::Equal(Box::new((
                            CsExpression::from(usize::from(sender_id) as Integer),
                            CsExpression::Var(origin_var, Type::Integer),
                        ))),
                    ]);
                    // Add event (and sender) to list of known events.
                    known_events.push(is_event_sender.to_owned());
                    // We need to use this as guard only once, so we wrap it in an Option.
                    let mut is_event_sender = Some(is_event_sender);
                    let mut current_loc = ext_event_processing_param;
                    for (param_name, _) in event_builder.params.iter() {
                        let read_param = *param_actions
                            .get(&(sender_id, event_index, param_name.to_owned()))
                            .expect("has to be there");
                        let next_loc = self.cs.new_location(pg_id).expect("program graph exists!");
                        self.cs
                            .add_transition(
                                pg_id,
                                current_loc,
                                read_param,
                                next_loc,
                                // Need to check only once, so `take` Option
                                is_event_sender.take(),
                            )
                            .expect("hand-coded args");
                        current_loc = next_loc;
                    }
                    // Check if event and sender are the correct ones in case of event with no parameter.
                    self.cs
                        .add_autonomous_transition(
                            pg_id,
                            current_loc,
                            eventful_trans,
                            is_event_sender,
                        )
                        .expect("has to work");
                }
            }
            // Proceed if event is unknown (without retreiving parameters).
            let unknown_event = if known_events.is_empty() {
                None
            } else {
                Some(Expression::Not(Box::new(Expression::Or(known_events))))
            };
            self.cs
                .add_autonomous_transition(
                    pg_id,
                    ext_event_processing_param,
                    eventful_trans,
                    unknown_event,
                )
                .expect("has to work");

            // Consider each of the state's transitions.
            for transition in state.transitions.iter() {
                trace!(
                    "build {} transition to {}",
                    transition
                        .event
                        .as_ref()
                        .unwrap_or(&"eventless".to_string()),
                    transition.target
                );
                // Get or create the location corresponding to the target state.
                let target_loc = *states
                    .entry(transition.target.to_owned())
                    .or_insert_with(|| self.cs.new_location(pg_id).expect("pg_id should exist"));

                // Set up origin and parameters for conditional/executable content.
                let exec_origin;
                let exec_params;
                if let Some(event_name) = transition.event.as_ref() {
                    let event_index = *self
                        .event_indexes
                        .get(event_name)
                        .expect("event must be registered");
                    exec_origin = Some(origin_var);
                    exec_params = param_vars
                        .iter()
                        .filter(|((ev_ix, _), _)| *ev_ix == event_index)
                        .map(|((_, name), (var, tp))| (name.to_owned(), (*var, tp.to_owned())))
                        .collect::<HashMap<String, (Var, String)>>();
                } else {
                    exec_origin = None;
                    exec_params = HashMap::new();
                }
                // Condition activating the transition.
                // It has to be parsed/built as a Boolean expression.
                // Could fail if `expr` is invalid.
                let cond: Option<CsExpression> = transition
                    .cond
                    .as_ref()
                    .map(|cond| {
                        self.expression(cond, &fsm.interner, &vars, exec_origin, &exec_params)
                    })
                    .transpose()?;

                // Location corresponding to checking if the transition is active.
                // Has to be defined depending on the type of transition.
                let check_trans_loc;
                // Location corresponding to verifying the transition is not active and moving to next one.
                let next_trans_loc = self.cs.new_location(pg_id).expect("{pg_id:?} exists");

                // Guard for transition.
                // Has to be defined depending on the type of transition, etc...
                let guard;
                // Proceed on whether the transition is eventless or activated by event.
                if let Some(event_name) = transition.event.as_ref() {
                    let event_index = *self
                        .event_indexes
                        .get(event_name)
                        .expect("event must be registered");
                    // Check if the current event (internal or external) corresponds to the event activating the transition.
                    let event_match = CsExpression::Equal(Box::new((
                        CsExpression::Var(current_event_var, Type::Integer),
                        CsExpression::from(event_index as Integer),
                    )));
                    // TODO FIXME: optimize And/Or expressions
                    guard = cond
                        .map(|cond| CsExpression::And(vec![event_match.clone(), cond]))
                        .or(Some(event_match));
                    // Check this transition after the other eventful transitions.
                    check_trans_loc = eventful_trans;
                    // Move location of next eventful transitions to a new location.
                    eventful_trans = next_trans_loc;
                } else {
                    // NULL (autonomous/eventless) transition
                    // No event needs to happen in order to trigger this transition.
                    guard = cond;
                    // Check this transition after the other eventless transitions.
                    check_trans_loc = null_trans;
                    // Move location of next eventless transitions to a new location.
                    null_trans = next_trans_loc;
                }

                // If transition is active, execute the relevant executable content and then the transition to the target.
                // Could fail if 'cond' expression was not acceptable as guard.
                let mut exec_trans_loc = self.cs.new_location(pg_id)?;
                self.cs.add_autonomous_transition(
                    pg_id,
                    check_trans_loc,
                    exec_trans_loc,
                    guard.to_owned(),
                )?;
                // First execute the executable content of the state's `on_exit` tag,
                // then that of the `transition` tag, following the specs.
                for exec in state.on_exit.iter().chain(transition.effects.iter()) {
                    exec_trans_loc = self.add_executable(
                        exec,
                        pg_id,
                        int_queue,
                        exec_trans_loc,
                        &vars,
                        exec_origin,
                        &exec_params,
                        &fsm.interner,
                    )?;
                }
                // Transitioning to the target state/location.
                // At this point, the transition cannot be stopped so there can be no guard.
                self.cs
                    .add_autonomous_transition(pg_id, exec_trans_loc, target_loc, None)
                    .expect("has to work");
                // If the current transition is not active, move on to check the next one.
                // NOTE: an autonomous transition without cond is always active so there is no point processing further transitions.
                // This happens in State Charts already, so we model it faithfully without optimizations.
                let not_guard = guard
                    .map(|guard| CsExpression::Not(Box::new(guard)))
                    .unwrap_or(CsExpression::from(false));
                self.cs
                    .add_autonomous_transition(
                        pg_id,
                        check_trans_loc,
                        next_trans_loc,
                        Some(not_guard),
                    )
                    .expect("cannot fail because guard was already checked");
            }

            // Connect NULL events with named events
            // by transitioning from last "NUll" location to dequeuing event location.
            self.cs
                .add_autonomous_transition(pg_id, null_trans, int_queue_loc, None)?;
            // Return to dequeue a new (internal or external) event.
            self.cs
                .add_autonomous_transition(pg_id, eventful_trans, int_queue_loc, None)?;
        }
        Ok(())
    }

    // WARN: vars and params have the same type so they could be easily swapped by mistake when calling the function.
    fn add_executable(
        &mut self,
        executable: &Executable,
        pg_id: PgId,
        int_queue: Channel,
        loc: Location,
        vars: &HashMap<String, (Var, String)>,
        origin: Option<Var>,
        params: &HashMap<String, (Var, String)>,
        interner: &boa_interner::Interner,
    ) -> Result<Location, anyhow::Error> {
        match executable {
            Executable::Raise { event } => {
                // Create event, if it does not exist already.
                let event_idx = self.event_index(event);
                let raise =
                    self.cs
                        .new_send(pg_id, int_queue, CsExpression::from(event_idx as Integer))?;
                let next_loc = self.cs.new_location(pg_id)?;
                // queue the internal event
                self.cs.add_transition(pg_id, loc, raise, next_loc, None)?;
                Ok(next_loc)
            }
            Executable::Send(Send {
                event,
                target,
                params: send_params,
            }) => match target {
                Target::Id(target) => {
                    let target_builder = self
                        .fsm_builders
                        .get(target)
                        .ok_or(anyhow!(format!("target {target} not found")))?;
                    let target_id = target_builder.pg_id;
                    let target_ext_queue = target_builder.ext_queue;
                    let event_idx = *self
                        .event_indexes
                        .get(event)
                        .expect("builder for {event} already exists");
                    let send_event = self
                        .cs
                        .new_send(
                            pg_id,
                            target_ext_queue,
                            CsExpression::Tuple(vec![
                                CsExpression::from(event_idx as Integer),
                                CsExpression::from(usize::from(pg_id) as Integer),
                            ]),
                        )
                        .expect("must work");

                    // Send event and event origin before moving on to next location.
                    let mut next_loc = self.cs.new_location(pg_id)?;
                    self.cs
                        .add_transition(pg_id, loc, send_event, next_loc, None)?;

                    // Pass parameters.
                    for param in send_params {
                        // Updates next location.
                        next_loc = self.send_param(
                            pg_id, target_id, param, event_idx, next_loc, vars, origin, params,
                            interner,
                        )?;
                    }

                    Ok(next_loc)
                }
                Target::Expr(targetexpr) => {
                    let targetexpr = self.expression(targetexpr, interner, vars, origin, params)?;
                    let event_idx = *self
                        .event_indexes
                        .get(event)
                        .ok_or(anyhow!("event not found"))?;
                    // Location representing having sent the event to the correct target after evaluating expression.
                    let done_loc = self.cs.new_location(pg_id).expect("PG exists");
                    for target_id in self.events[event_idx].receivers.clone() {
                        let target_name = self.fsm_names.get(&target_id).unwrap();
                        let target_builder =
                            self.fsm_builders.get(target_name).expect("it must exist");
                        let target_ext_queue = target_builder.ext_queue;
                        let send_event = self
                            .cs
                            .new_send(
                                pg_id,
                                target_ext_queue,
                                CsExpression::Tuple(vec![
                                    CsExpression::from(event_idx as Integer),
                                    CsExpression::from(usize::from(pg_id) as Integer),
                                ]),
                            )
                            .expect("params are hard-coded");

                        // Send event and event origin before moving on to next location.
                        let mut next_loc = self.cs.new_location(pg_id).expect("PG exists");
                        self.cs
                            .add_transition(
                                pg_id,
                                loc,
                                send_event,
                                next_loc,
                                Some(CsExpression::Equal(Box::new((
                                    CsExpression::from(usize::from(target_id) as Integer),
                                    targetexpr.to_owned(),
                                )))),
                            )
                            .expect("params are right");

                        // Pass parameters. This could fail due to param content.
                        for param in send_params {
                            // Updates next location.
                            next_loc = self.send_param(
                                pg_id, target_id, param, event_idx, next_loc, vars, origin, params,
                                interner,
                            )?;
                        }
                        // Once sending event and args done, get to exit-point
                        self.cs
                            .add_autonomous_transition(pg_id, next_loc, done_loc, None)
                            .expect("hand-made args");
                    }

                    // Return exit point
                    Ok(done_loc)
                }
            },
            Executable::Assign { location, expr } => {
                // Add a transition that perform the assignment via the effect of the `assign` action.
                let expr = self.expression(expr, interner, vars, origin, params)?;
                let (var, _scan_type) = vars.get(location).ok_or(anyhow!("undefined variable"))?;
                let assign = self.cs.new_action(pg_id).expect("PG exists");
                self.cs.add_effect(pg_id, assign, *var, expr)?;
                let next_loc = self.cs.new_location(pg_id).unwrap();
                self.cs.add_transition(pg_id, loc, assign, next_loc, None)?;
                Ok(next_loc)
            }
            Executable::If(If { r#elif, r#else, .. }) => {
                // We go to this location after the if/elif/else block
                let end_loc = self.cs.new_location(pg_id).unwrap();
                let mut curr_loc = loc;
                for (cond, execs) in r#elif {
                    let mut next_loc = self.cs.new_location(pg_id).unwrap();
                    let cond = self.expression(cond, interner, vars, origin, params)?;
                    self.cs.add_autonomous_transition(
                        pg_id,
                        curr_loc,
                        next_loc,
                        Some(cond.to_owned()),
                    )?;
                    for exec in execs {
                        next_loc = self.add_executable(
                            exec, pg_id, int_queue, next_loc, vars, origin, params, interner,
                        )?;
                    }
                    // end of `if` branch, go to end_loc
                    self.cs
                        .add_autonomous_transition(pg_id, next_loc, end_loc, None)?;
                    // `elif/else` branch
                    let old_loc = curr_loc;
                    curr_loc = self.cs.new_location(pg_id).unwrap();
                    self.cs
                        .add_autonomous_transition(
                            pg_id,
                            old_loc,
                            curr_loc,
                            Some(Expression::Not(Box::new(cond))),
                        )
                        .unwrap();
                }
                // Add executables for `else` (if any)
                for exec in r#else {
                    curr_loc = self.add_executable(
                        exec, pg_id, int_queue, curr_loc, vars, origin, params, interner,
                    )?;
                }
                self.cs
                    .add_autonomous_transition(pg_id, curr_loc, end_loc, None)?;
                Ok(end_loc)
            }
        }
    }

    // WARN: vars and params have the same type so they could be easily swapped by mistake when calling the function.
    fn send_param(
        &mut self,
        pg_id: PgId,
        target_id: PgId,
        param: &Param,
        event_idx: usize,
        param_loc: Location,
        vars: &HashMap<String, (Var, String)>,
        origin: Option<Var>,
        params: &HashMap<String, (Var, String)>,
        interner: &boa_interner::Interner,
    ) -> Result<Location, anyhow::Error> {
        // Get param type.
        let scan_type = self
            .types
            .get(param.omg_type.as_str())
            .cloned()
            .ok_or(anyhow!("undefined type"))?
            .1;
        // Build expression from ECMAScript expression.
        let expr = self.expression(&param.expr, interner, vars, origin, params)?;
        // Retreive or create channel for parameter passing.
        let param_chn = *self
            .parameters
            .entry((pg_id, target_id, event_idx, param.name.to_owned()))
            .or_insert(self.cs.new_channel(scan_type, None));
        // Can return error if expr is badly typed
        let pass_param = self.cs.new_send(pg_id, param_chn, expr)?;
        let next_loc = self.cs.new_location(pg_id).expect("PG exists");
        self.cs
            .add_transition(pg_id, param_loc, pass_param, next_loc, None)
            .expect("hand-made params are correct");
        Ok(next_loc)
    }

    // WARN: vars and params have the same type so they could be easily swapped by mistake when calling the function.
    fn expression(
        &mut self,
        expr: &boa_ast::Expression,
        interner: &boa_interner::Interner,
        vars: &HashMap<String, (Var, String)>,
        origin: Option<Var>,
        params: &HashMap<String, (Var, String)>,
    ) -> anyhow::Result<CsExpression> {
        let expr = match expr {
            boa_ast::Expression::This => todo!(),
            boa_ast::Expression::Identifier(ident) => {
                let ident = ident.to_interned_string(interner);
                self.enums
                    .get(&ident)
                    .map(|i| CsExpression::from(*i))
                    .or_else(|| {
                        vars.get(&ident).and_then(|(var, t)| {
                            self.types
                                .get(t)
                                .map(|(_, t)| CsExpression::Var(*var, t.to_owned()))
                            // .ok_or(anyhow!("missing type {t}"))
                        })
                    })
                    .ok_or(anyhow!("unknown identifier: {ident}"))?
            }
            boa_ast::Expression::Literal(lit) => {
                use boa_ast::expression::literal::Literal;
                match lit {
                    Literal::String(_) => todo!(),
                    Literal::Num(f) => CsExpression::from(*f),
                    Literal::Int(i) => CsExpression::from(*i),
                    Literal::BigInt(_) => todo!(),
                    Literal::Bool(b) => CsExpression::from(*b),
                    Literal::Null => todo!(),
                    Literal::Undefined => todo!(),
                }
            }
            boa_ast::Expression::RegExpLiteral(_) => todo!(),
            boa_ast::Expression::ArrayLiteral(arr) => {
                todo!()
                // arr
                //             .to_pattern(true)
                //             .ok_or(anyhow!("array syntax error"))?
                //             .into_iter()
                //             .map(|element| match element {})
            }
            boa_ast::Expression::ObjectLiteral(_) => todo!(),
            boa_ast::Expression::Spread(_) => todo!(),
            boa_ast::Expression::Function(_) => todo!(),
            boa_ast::Expression::ArrowFunction(_) => todo!(),
            boa_ast::Expression::AsyncArrowFunction(_) => todo!(),
            boa_ast::Expression::Generator(_) => todo!(),
            boa_ast::Expression::AsyncFunction(_) => todo!(),
            boa_ast::Expression::AsyncGenerator(_) => todo!(),
            boa_ast::Expression::Class(_) => todo!(),
            boa_ast::Expression::TemplateLiteral(_) => todo!(),
            boa_ast::Expression::PropertyAccess(prop_acc) => {
                let expr = &boa_ast::Expression::PropertyAccess(prop_acc.to_owned());
                let ecma_obj = self.expression_prop_access(expr, interner, vars, origin, params)?;
                // WARN: If the EcmaObj is a primitive SCAN data, we return that.
                // If it is a dictionary of properties, instead, we have no way to represent it properly as a SCAN type.
                match ecma_obj {
                    EcmaObj::PrimitiveData(expr, _) => expr,
                    EcmaObj::Properties(_) => todo!(),
                }
            }
            boa_ast::Expression::New(_) => todo!(),
            boa_ast::Expression::Call(_) => todo!(),
            boa_ast::Expression::SuperCall(_) => todo!(),
            boa_ast::Expression::ImportCall(_) => todo!(),
            boa_ast::Expression::Optional(_) => todo!(),
            boa_ast::Expression::TaggedTemplate(_) => todo!(),
            boa_ast::Expression::NewTarget => todo!(),
            boa_ast::Expression::ImportMeta => todo!(),
            boa_ast::Expression::Assign(_) => todo!(),
            boa_ast::Expression::Unary(unary) => {
                use boa_ast::expression::operator::unary::UnaryOp;
                match unary.op() {
                    UnaryOp::Minus => todo!(),
                    UnaryOp::Plus => todo!(),
                    UnaryOp::Not => self
                        .expression(unary.target(), interner, vars, origin, params)
                        .map(|expr| Expression::Not(Box::new(expr)))?,
                    UnaryOp::Tilde => todo!(),
                    UnaryOp::TypeOf => todo!(),
                    UnaryOp::Delete => todo!(),
                    UnaryOp::Void => todo!(),
                }
            }
            boa_ast::Expression::Update(_) => todo!(),
            boa_ast::Expression::Binary(bin) => {
                use boa_ast::expression::operator::binary::{
                    ArithmeticOp, BinaryOp, LogicalOp, RelationalOp,
                };
                let lhs = self.expression(bin.lhs(), interner, vars, origin, params)?;
                let rhs = self.expression(bin.rhs(), interner, vars, origin, params)?;
                match bin.op() {
                    BinaryOp::Arithmetic(ar_bin) => match ar_bin {
                        ArithmeticOp::Add => CsExpression::Sum(vec![lhs, rhs]),
                        ArithmeticOp::Sub => {
                            CsExpression::Sum(vec![lhs, CsExpression::Opposite(Box::new(rhs))])
                        }
                        ArithmeticOp::Div => todo!(),
                        ArithmeticOp::Mul => todo!(),
                        ArithmeticOp::Exp => todo!(),
                        ArithmeticOp::Mod => CsExpression::Mod(Box::new((lhs, rhs))),
                    },
                    BinaryOp::Bitwise(_) => todo!(),
                    BinaryOp::Relational(rel_bin) => match rel_bin {
                        RelationalOp::Equal => CsExpression::Equal(Box::new((lhs, rhs))),
                        RelationalOp::NotEqual => todo!(),
                        RelationalOp::StrictEqual => todo!(),
                        RelationalOp::StrictNotEqual => todo!(),
                        RelationalOp::GreaterThan => CsExpression::Greater(Box::new((lhs, rhs))),
                        RelationalOp::GreaterThanOrEqual => {
                            CsExpression::GreaterEq(Box::new((lhs, rhs)))
                        }
                        RelationalOp::LessThan => CsExpression::Less(Box::new((lhs, rhs))),
                        RelationalOp::LessThanOrEqual => CsExpression::LessEq(Box::new((lhs, rhs))),
                        RelationalOp::In => todo!(),
                        RelationalOp::InstanceOf => todo!(),
                    },
                    BinaryOp::Logical(op) => match op {
                        LogicalOp::And => CsExpression::And(vec![lhs, rhs]),
                        LogicalOp::Or => CsExpression::Or(vec![lhs, rhs]),
                        LogicalOp::Coalesce => todo!(),
                    },
                    BinaryOp::Comma => todo!(),
                }
            }
            boa_ast::Expression::BinaryInPrivate(_) => todo!(),
            boa_ast::Expression::Conditional(_) => todo!(),
            boa_ast::Expression::Await(_) => todo!(),
            boa_ast::Expression::Yield(_) => todo!(),
            boa_ast::Expression::Parenthesized(par) => {
                self.expression(par.expression(), interner, vars, origin, params)?
            }
            _ => todo!(),
        };
        Ok(expr)
    }

    fn value(
        &self,
        expr: &boa_ast::Expression,
        interner: &boa_interner::Interner,
    ) -> anyhow::Result<Val> {
        let expr = match expr {
            boa_ast::Expression::This => todo!(),
            boa_ast::Expression::Identifier(ident) => {
                let ident = ident.to_interned_string(interner);
                self.enums
                    .get(&ident)
                    .map(|i| Val::Integer(*i))
                    .ok_or(anyhow!("unknown identifier: {ident}"))?
            }
            boa_ast::Expression::Literal(lit) => {
                use boa_ast::expression::literal::Literal;
                match lit {
                    Literal::String(_) => todo!(),
                    Literal::Num(_) => todo!(),
                    Literal::Int(i) => Val::Integer(*i),
                    Literal::BigInt(_) => todo!(),
                    Literal::Bool(b) => Val::Boolean(*b),
                    Literal::Null => todo!(),
                    Literal::Undefined => todo!(),
                }
            }
            boa_ast::Expression::PropertyAccess(_prop_acc) => {
                todo!()
            }
            boa_ast::Expression::Unary(unary) => {
                use boa_ast::expression::operator::unary::UnaryOp;
                match unary.op() {
                    UnaryOp::Minus => todo!(),
                    UnaryOp::Plus => todo!(),
                    UnaryOp::Not => self.value(unary.target(), interner).map(|val| {
                        if let Val::Boolean(b) = val {
                            Val::Boolean(!b)
                        } else {
                            todo!()
                        }
                    })?,
                    UnaryOp::Tilde => todo!(),
                    UnaryOp::TypeOf => todo!(),
                    UnaryOp::Delete => todo!(),
                    UnaryOp::Void => todo!(),
                }
            }
            boa_ast::Expression::Binary(bin) => {
                use boa_ast::expression::operator::binary::{ArithmeticOp, BinaryOp};
                match bin.op() {
                    BinaryOp::Arithmetic(ar_bin) => {
                        let lhs = self.value(bin.lhs(), interner)?;
                        let rhs = self.value(bin.rhs(), interner)?;
                        let lhs = if let Val::Integer(i) = lhs {
                            i
                        } else {
                            todo!()
                        };
                        let rhs = if let Val::Integer(i) = rhs {
                            i
                        } else {
                            todo!()
                        };
                        match ar_bin {
                            ArithmeticOp::Add => Val::Integer(lhs + rhs),
                            ArithmeticOp::Sub => Val::Integer(lhs - rhs),
                            ArithmeticOp::Div => todo!(),
                            ArithmeticOp::Mul => todo!(),
                            ArithmeticOp::Exp => todo!(),
                            ArithmeticOp::Mod => todo!(),
                        }
                    }
                    BinaryOp::Relational(_rel_bin) => {
                        todo!()
                    }
                    BinaryOp::Logical(_) => todo!(),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        };
        Ok(expr)
    }

    fn expression_prop_access(
        &mut self,
        expr: &boa_ast::Expression,
        interner: &boa_interner::Interner,
        vars: &HashMap<String, (Var, String)>,
        origin: Option<Var>,
        params: &HashMap<String, (Var, String)>,
    ) -> anyhow::Result<EcmaObj> {
        match expr {
            boa_ast::Expression::This => todo!(),
            boa_ast::Expression::Identifier(ident) => {
                let ident = ident.to_interned_string(interner);
                match ident.as_str() {
                    "_event" => Ok(EcmaObj::Properties(HashMap::from([
                        (
                            String::from("origin"),
                            EcmaObj::PrimitiveData(
                                Expression::Var(
                                    origin.ok_or(anyhow!("missing origin of _event"))?,
                                    Type::Integer,
                                ),
                                String::from("int32"),
                            ),
                        ),
                        (
                            String::from("data"),
                            EcmaObj::Properties(HashMap::from_iter(params.iter().map(
                                |(n, (v, t))| {
                                    (
                                        n.to_owned(),
                                        EcmaObj::PrimitiveData(
                                            CsExpression::Var(
                                                *v,
                                                self.types
                                                    .get(t)
                                                    .map(|(_, t)| t.to_owned())
                                                    .expect("type of data parameter"),
                                            ),
                                            t.to_owned(),
                                        ),
                                    )
                                },
                            ))),
                        ),
                    ]))),
                    ident => {
                        let (var, type_name) = vars
                            .get(ident)
                            .ok_or(anyhow!("location {} not found", ident))?
                            .to_owned();
                        let (_, t) = self.types.get(&type_name).expect("var type");
                        Ok(EcmaObj::PrimitiveData(
                            Expression::Var(var, t.to_owned()),
                            type_name,
                        ))
                    }
                }
            }
            boa_ast::Expression::PropertyAccess(prop_acc) => {
                use boa_ast::expression::access::{PropertyAccess, PropertyAccessField};
                match prop_acc {
                    PropertyAccess::Simple(simp_prop_acc) => {
                        let prop_target = self.expression_prop_access(
                            simp_prop_acc.target(),
                            interner,
                            vars,
                            origin,
                            params,
                        )?;
                        match simp_prop_acc.field() {
                            PropertyAccessField::Const(sym) => {
                                let ident: &str = interner
                                    .resolve(*sym)
                                    .ok_or(anyhow!("unknown symbol {:?}", sym))?
                                    .utf8()
                                    .ok_or(anyhow!("not utf8"))?;
                                match prop_target {
                                    EcmaObj::PrimitiveData(expr, type_name) => {
                                        match &self
                                            .types
                                            .get(&type_name)
                                            .ok_or(anyhow!("unknown type {}", type_name))?
                                            .0
                                        {
                                            OmgType::Boolean => todo!(),
                                            OmgType::Int32 => todo!(),
                                            OmgType::F64 => todo!(),
                                            OmgType::Uri => todo!(),
                                            OmgType::Structure(fields) => {
                                                let index = *self
                                                    .structs
                                                    .get(&(type_name, ident.to_owned()))
                                                    .ok_or(anyhow!("field {} not found", ident))?;
                                                let field_type_name = fields
                                                    .get(ident)
                                                    .ok_or(anyhow!("field {} not found", ident))?;
                                                Ok(EcmaObj::PrimitiveData(
                                                    Expression::Component(index, Box::new(expr)),
                                                    field_type_name.to_owned(),
                                                ))
                                            }
                                            OmgType::Enumeration(_) => todo!(),
                                        }
                                    }
                                    EcmaObj::Properties(fields) => fields
                                        .get(ident)
                                        .ok_or(anyhow!("property {} not found", ident))
                                        .cloned(),
                                }
                            }
                            PropertyAccessField::Expr(_) => todo!(),
                        }
                    }
                    PropertyAccess::Private(_) => todo!(),
                    PropertyAccess::Super(_) => todo!(),
                }
            }
            _ => todo!(),
        }
    }

    fn build_predicates(&mut self, parser: &Parser) -> anyhow::Result<()> {
        for (port_id, port) in parser.properties.ports.iter() {
            let origin_builder = self
                .fsm_builders
                .get(&port.origin)
                .ok_or(anyhow!("missing origin fsm {}", port.origin))?;
            let origin = origin_builder.pg_id;
            let target_builder = self
                .fsm_builders
                .get(&port.target)
                .ok_or(anyhow!("missing target fsm {}", port.target))?;
            let target = target_builder.pg_id;
            let event_id = *self
                .event_indexes
                .get(&port.event)
                .ok_or(anyhow!("missing event {}", port.event))?;
            if let Some((param, init)) = &port.param {
                let init = self.value(init, &boa_interner::Interner::new())?;
                let channel = *self
                    .parameters
                    .get(&(origin, target, event_id, param.to_owned()))
                    .ok_or(anyhow!("param {param} not found"))?;
                self.ports
                    .insert(port_id.to_owned(), (Port::Message(channel), init));
            } else {
                let channel = target_builder.ext_queue;
                self.ports.insert(
                    format!("{target:?}"),
                    (
                        Port::Message(channel),
                        Val::Tuple(vec![Val::Integer(-1), Val::Integer(-1)]),
                    ),
                );
                let expr = Expression::And(vec![
                    Expression::Equal(Box::new((
                        Expression::from(channel.0 as Integer),
                        Expression::Var(Port::LastMessage, Type::Integer),
                    ))),
                    Expression::Equal(Box::new((
                        Expression::from(event_id as Integer),
                        Expression::Component(
                            0,
                            Box::new(Expression::Var(
                                Port::Message(channel),
                                Type::Product(vec![Type::Integer, Type::Integer]),
                            )),
                        ),
                    ))),
                    Expression::Equal(Box::new((
                        Expression::from(Into::<usize>::into(origin) as Integer),
                        Expression::Component(
                            1,
                            Box::new(Expression::Var(
                                Port::Message(channel),
                                Type::Product(vec![Type::Integer, Type::Integer]),
                            )),
                        ),
                    ))),
                ]);
                // A port for an event becomes a predicate.
                self.predicates.insert(port_id.to_owned(), expr);
            }
        }
        for (predicate_id, predicate) in parser.properties.predicates.iter() {
            let predicate = self.build_predicate(predicate)?;
            self.predicates.insert(predicate_id.to_owned(), predicate);
        }
        for (property_id, property) in parser.properties.guarantees.iter() {
            self.guarantees
                .insert(property_id.to_owned(), property.to_owned());
        }
        for (property_id, property) in parser.properties.assumes.iter() {
            self.assumes
                .insert(property_id.to_owned(), property.to_owned());
        }
        Ok(())
    }

    fn build_predicate(&self, predicate: &Expression<String>) -> anyhow::Result<Expression<Port>> {
        match predicate {
            Expression::Const(val) => Ok(Expression::Const(val.to_owned())),
            // WARN: The variable type is wrong, it cannot be used!
            Expression::Var(port, _wrong_type_do_not_use) => self
                .ports
                .get(port)
                .cloned()
                .map(|(port, val)| Expression::Var(port, val.r#type()))
                .or_else(|| self.predicates.get(port).map(|pred| pred.to_owned()))
                .ok_or(anyhow!("missing port {port}")),
            Expression::Tuple(_) => todo!(),
            Expression::Component(_, _) => todo!(),
            Expression::And(exprs) => exprs
                .iter()
                .map(|expr| self.build_predicate(expr))
                .collect::<Result<_, _>>()
                .map(Expression::And),
            Expression::Or(exprs) => exprs
                .iter()
                .map(|expr| self.build_predicate(expr))
                .collect::<Result<_, _>>()
                .map(Expression::Or),
            Expression::Implies(exprs) => Ok(Expression::Implies(Box::new((
                self.build_predicate(&exprs.0)?,
                self.build_predicate(&exprs.1)?,
            )))),
            Expression::Not(expr) => self
                .build_predicate(expr)
                .map(|expr| Expression::Not(Box::new(expr))),
            Expression::Opposite(expr) => self
                .build_predicate(expr)
                .map(|expr| Expression::Opposite(Box::new(expr))),
            Expression::Sum(exprs) => exprs
                .iter()
                .map(|expr| self.build_predicate(expr))
                .collect::<Result<_, _>>()
                .map(Expression::Sum),
            Expression::Mult(exprs) => exprs
                .iter()
                .map(|expr| self.build_predicate(expr))
                .collect::<Result<_, _>>()
                .map(Expression::Mult),
            Expression::Equal(exprs) => Ok(Expression::Equal(Box::new((
                self.build_predicate(&exprs.0)?,
                self.build_predicate(&exprs.1)?,
            )))),
            Expression::Greater(exprs) => Ok(Expression::Greater(Box::new((
                self.build_predicate(&exprs.0)?,
                self.build_predicate(&exprs.1)?,
            )))),
            Expression::GreaterEq(exprs) => Ok(Expression::GreaterEq(Box::new((
                self.build_predicate(&exprs.0)?,
                self.build_predicate(&exprs.1)?,
            )))),
            Expression::Less(exprs) => Ok(Expression::Less(Box::new((
                self.build_predicate(&exprs.0)?,
                self.build_predicate(&exprs.1)?,
            )))),
            Expression::LessEq(exprs) => Ok(Expression::LessEq(Box::new((
                self.build_predicate(&exprs.0)?,
                self.build_predicate(&exprs.1)?,
            )))),
            Expression::Append(exprs) => Ok(Expression::Append(Box::new((
                self.build_predicate(&exprs.0)?,
                self.build_predicate(&exprs.1)?,
            )))),
            Expression::Truncate(expr) => Ok(Expression::Truncate(Box::new(
                self.build_predicate(expr.as_ref())?,
            ))),
            Expression::Len(expr) => Ok(Expression::Len(Box::new(
                self.build_predicate(expr.as_ref())?,
            ))),
            Expression::Mod(exprs) => Ok(Expression::Mod(Box::new((
                self.build_predicate(&exprs.0)?,
                self.build_predicate(&exprs.1)?,
            )))),
        }
    }

    fn build_model(self) -> ScxmlModel {
        let mut model = CsModelBuilder::new(self.cs.build());
        let mut pred_names: HashMap<String, usize> = HashMap::new();
        let mut predicates = Vec::new();
        for (_port_name, (port, init)) in self.ports {
            // TODO FIXME handle error.
            if let Port::Message(channel) = port {
                model.add_port(channel, init).unwrap();
            }
        }
        for (pred_name, pred_expr) in self.predicates {
            // TODO FIXME handle error.
            let id = model.add_predicate(pred_expr).unwrap();
            pred_names.insert(pred_name.to_owned(), id);
            assert_eq!(id, predicates.len());
            predicates.push(pred_name);
        }
        ScxmlModel {
            model: model.build(),
            guarantees: self
                .guarantees
                .into_values()
                .map(|prop| map_predicates_in_property(prop, &pred_names))
                .collect(),
            assumes: self
                .assumes
                .into_values()
                .map(|prop| map_predicates_in_property(prop, &pred_names))
                .collect(),
            fsm_names: self.fsm_names,
            parameters: self
                .parameters
                .into_iter()
                .map(|((src, trg, event, name), chn)| (chn, (src, trg, event, name)))
                .collect(),
            ext_queues: self
                .fsm_builders
                .values()
                .map(|b| (b.ext_queue, b.pg_id))
                .collect(),
            int_queues: self.int_queues,
            events: self
                .event_indexes
                .into_iter()
                .map(|(name, idx)| (idx, name))
                .collect(),
            fsm_indexes: self
                .fsm_builders
                .into_iter()
                .map(|(name, b)| (usize::from(b.pg_id), name))
                .collect(),
            predicates,
        }
    }
}

fn map_predicates_in_property(
    property: Mtl<String>,
    predicates: &HashMap<String, usize>,
) -> Mtl<usize> {
    match property {
        Mtl::True => Mtl::True,
        // FIXME TODO handle error
        Mtl::Atom(pred) => Mtl::Atom(*predicates.get(&pred).unwrap()),
        Mtl::And(formulae) => Mtl::And(
            formulae
                .into_iter()
                .map(|f| map_predicates_in_property(f, predicates))
                .collect(),
        ),
        // Mtl::Or(formulae) => Mtl::Or(
        //     formulae
        //         .into_iter()
        //         .map(|f| map_predicates_in_property(f, predicates))
        //         .collect(),
        // ),
        Mtl::Not(formula) => Mtl::Not(Box::new(map_predicates_in_property(*formula, predicates))),
        // Mtl::Implies(_) => todo!(),
        Mtl::Next(_) => todo!(),
        Mtl::Until(formulae, range) => {
            let (lhs, rhs) = *formulae;
            Mtl::Until(
                Box::new((
                    map_predicates_in_property(lhs, predicates),
                    map_predicates_in_property(rhs, predicates),
                )),
                range,
            )
        }
    }
}
