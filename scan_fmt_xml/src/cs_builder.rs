use crate::parser::*;
use anyhow::anyhow;
use log::{info, trace};
use scan_core::{channel_system::*, *};
use std::collections::{HashMap, HashSet};

// TODO:
//
// -[ ] WARN FIXME System is fragile if name/id/path do not coincide

#[derive(Debug)]
pub struct CsModel {
    pub cs: ChannelSystem,
    pub fsm_names: HashMap<PgId, String>,
}

#[derive(Debug, Clone)]
struct FsmBuilder {
    pg_id: PgId,
    ext_queue: Channel,
    index: usize,
}

#[derive(Debug, Clone)]
struct EventBuilder {
    params: HashMap<String, Type>,
    senders: HashSet<PgId>,
    receivers: HashSet<PgId>,
    index: usize,
}

#[derive(Debug)]
pub struct Sc2CsVisitor {
    cs: ChannelSystemBuilder,
    // Represent OMG types
    scan_types: HashMap<String, Type>,
    // WARN FIXME TODO: simplistic implementation of enums
    enums: HashMap<String, Integer>,
    // Each State Chart has an associated Program Graph,
    // and an arbitrary, progressive index
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
}

impl Sc2CsVisitor {
    pub fn visit(parser: Parser) -> anyhow::Result<CsModel> {
        // Add base types
        // FIXME: Is there a better way? Const object?
        let base_types: [(String, Type); 3] = [
            (String::from("Boolean"), Type::Boolean),
            (String::from("int32"), Type::Integer),
            (String::from("URI"), Type::Integer),
        ];

        let mut model = Sc2CsVisitor {
            cs: ChannelSystemBuilder::new(),
            scan_types: HashMap::from_iter(base_types.into_iter()),
            enums: HashMap::new(),
            fsm_builders: HashMap::new(),
            events: Vec::new(),
            event_indexes: HashMap::new(),
            parameters: HashMap::new(),
        };

        model.build_types(&parser.types)?;

        model.prebuild_processes(&parser)?;

        info!("Visit process list");
        for (id, declaration) in parser.process_list.iter() {
            info!("Visit process {id}");
            match &declaration.moc {
                MoC::Fsm(fsm) => model.build_fsm(fsm)?,
                MoC::Bt(bt) => todo!(), // model.build_bt(bt)?,
            }
        }

        let model = model.build();

        Ok(model)
    }

    fn build_types(&mut self, omg_types: &OmgTypes) -> anyhow::Result<()> {
        for (name, omg_type) in omg_types.types.iter() {
            let scan_type = match omg_type {
                OmgType::Boolean => Type::Boolean,
                OmgType::Int32 => Type::Integer,
                OmgType::Structure() => todo!(),
                OmgType::Enumeration(labels) => {
                    for (idx, label) in labels.iter().enumerate() {
                        self.enums.insert(label.to_owned(), idx as Integer);
                    }
                    Type::Integer
                }
            };
            self.scan_types.insert(name.to_owned(), scan_type);
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
            let index = self.fsm_builders.len();
            let pg_id = self.cs.new_program_graph();
            let ext_queue = self
                .cs
                .new_channel(Type::Product(vec![Type::Integer, Type::Integer]), None);
            let fsm = FsmBuilder {
                pg_id,
                ext_queue,
                index,
            };
            self.fsm_builders.insert(id.to_string(), fsm);
        }
        self.fsm_builders.get(id).expect("just inserted")
    }

    fn prebuild_processes(&mut self, parser: &Parser) -> anyhow::Result<()> {
        for (id, declaration) in parser.process_list.iter() {
            let pg_id = self.fsm_builder(id).pg_id;
            match &declaration.moc {
                MoC::Fsm(fsm) => self.prebuild_fsms(pg_id, fsm)?,
                MoC::Bt(bt) => todo!(), // self.prebuild_bt(pg_id, bt)?,
            }
        }
        Ok(())
    }

    fn prebuild_fsms(&mut self, pg_id: PgId, fmt: &Fsm) -> anyhow::Result<()> {
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
            Executable::Send {
                event,
                target: _,
                params,
            } => {
                let event_index = self.event_index(event);
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.senders.insert(pg_id);
                for param in params {
                    let var_type = self
                        .scan_types
                        .get(&param.omg_type)
                        .ok_or(anyhow!("type not found"))?;
                    let prev_type = builder
                        .params
                        .insert(param.name.to_owned(), var_type.to_owned());
                    // Type parameters should not change type
                    if let Some(prev_type) = prev_type {
                        if prev_type != *var_type {
                            return Err(anyhow!("type parameter mismatch"));
                        }
                    }
                }
                Ok(())
            }
        }
    }

    fn prebuild_bt(&mut self, pg_id: PgId, _bt: &Bt) -> anyhow::Result<()> {
        let event_index = self.event_index(TICK_CALL);
        let builder = self.events.get_mut(event_index).expect("index must exist");
        builder.senders.insert(pg_id);
        builder.receivers.insert(pg_id);
        let event_index = self.event_index(HALT_CALL);
        let builder = self.events.get_mut(event_index).expect("index must exist");
        builder.senders.insert(pg_id);
        builder.receivers.insert(pg_id);
        let event_index = self.event_index(TICK_RETURN);
        let builder = self.events.get_mut(event_index).expect("index must exist");
        builder.senders.insert(pg_id);
        builder.receivers.insert(pg_id);
        builder.params.insert(RESULT.to_owned(), Type::Integer);
        let event_index = self.event_index(HALT_RETURN);
        let builder = self.events.get_mut(event_index).expect("index must exist");
        builder.senders.insert(pg_id);
        builder.receivers.insert(pg_id);
        Ok(())
    }

    fn build_bt(&mut self, bt: &Bt) -> anyhow::Result<()> {
        trace!("build bt {}", bt.id);
        // Initialize bt.
        let pg_builder = self.fsm_builder(&bt.id);
        let pg_id = pg_builder.pg_id;
        let ext_queue = pg_builder.ext_queue;
        // Locations are relative to what the node receives
        let loc_idle = self.cs.initial_location(pg_id)?;
        let loc_tick = self.cs.new_location(pg_id)?;
        let loc_success = self.cs.new_location(pg_id)?;
        let loc_running = self.cs.new_location(pg_id)?;
        let loc_failure = self.cs.new_location(pg_id)?;
        let loc_halt = self.cs.new_location(pg_id)?;
        let loc_ack = self.cs.new_location(pg_id)?;
        let step = self.cs.new_action(pg_id)?;
        let ext_event_var = self
            .cs
            .new_var(pg_id, Type::Product(vec![Type::Integer, Type::Integer]))
            .expect("{pg_id:?} exists");
        self.build_bt_node(
            pg_id,
            loc_tick,
            loc_success,
            loc_running,
            loc_failure,
            loc_halt,
            loc_ack,
            step,
            &bt.root,
        )?;

        let receive_event =
            self.cs
                .new_communication(pg_id, ext_queue, Message::Receive(ext_event_var))?;
        let event_received = self.cs.new_location(pg_id)?;
        self.cs
            .add_transition(pg_id, loc_idle, receive_event, event_received, None)?;

        // TICK
        // Create event, if it does not exist already.
        let tick_idx = *self.event_indexes.get(TICK_CALL).unwrap() as Integer;
        let halt_idx = *self.event_indexes.get(HALT_CALL).unwrap() as Integer;
        self.cs
            .add_transition(
                pg_id,
                event_received,
                step,
                loc_tick,
                Some(Expression::Equal(Box::new((
                    Expression::Component(0, Box::new(Expression::Var(ext_event_var))),
                    Expression::Integer(tick_idx),
                )))),
            )
            .expect("hope this works");
        self.cs
            .add_transition(
                pg_id,
                event_received,
                step,
                loc_halt,
                Some(Expression::Equal(Box::new((
                    Expression::Component(0, Box::new(Expression::Var(ext_event_var))),
                    Expression::Integer(halt_idx),
                )))),
            )
            .expect("hope this works");
        Ok(())
    }

    /// Recursively build a BT node by associating each possible state of the node to a location:
    /// - pt_tick: the parent node has sent a tick
    /// - pt_success: the parent node receives a tick return with state success
    /// - pt_running: the parent node receives a tick return with state running
    /// - pt_failure: the parent node receives a tick return with state failure
    /// - pt_halt: the parent node sends an halt signal
    /// - pt_ack: the parent node receives an ack signal
    /// Moreover, we consider the following nodes:
    /// - pt_*: parent node
    /// - loc_*: current node (loc=location)
    /// - branch_*: branch/child node
    fn build_bt_node(
        &mut self,
        pg_id: PgId,
        pt_tick: CsLocation,
        pt_success: CsLocation,
        pt_running: CsLocation,
        pt_failure: CsLocation,
        pt_halt: CsLocation,
        pt_ack: CsLocation,
        step: CsAction,
        node: &BtNode,
    ) -> anyhow::Result<()> {
        match node {
            BtNode::RSeq(branches) => {
                if let Some(branch) = branches.first() {
                    let halt_after_failure = self.cs.new_action(pg_id).expect("{pg_id:?} exists");
                    let halting_after_failure = self
                        .cs
                        .new_var(pg_id, Type::Boolean)
                        .expect("{pg_id:?} exists");
                    self.cs
                        .add_effect(
                            pg_id,
                            halt_after_failure,
                            halting_after_failure,
                            Expression::Boolean(true),
                        )
                        .expect("hand-picked arguments");
                    let failure_after_halting =
                        self.cs.new_action(pg_id).expect("{pg_id:?} exists");
                    self.cs
                        .add_effect(
                            pg_id,
                            failure_after_halting,
                            halting_after_failure,
                            Expression::Boolean(false),
                        )
                        .expect("hand-picked arguments");
                    // If receives tick from parent, tick first child.
                    let loc_tick = pt_tick;
                    let loc_success = self.cs.new_location(pg_id)?;
                    let loc_running = self.cs.new_location(pg_id)?;
                    let loc_failure = self.cs.new_location(pg_id)?;
                    // If parent sends halt, halt first child.
                    let loc_halt = pt_halt;
                    let loc_ack = self.cs.new_location(pg_id)?;
                    self.build_bt_node(
                        pg_id,
                        loc_tick,
                        loc_success,
                        loc_running,
                        loc_failure,
                        loc_halt,
                        loc_ack,
                        step,
                        branch,
                    )?;
                    let mut prev_success = loc_success;
                    let mut prev_failure = loc_failure;
                    let mut prev_ack = loc_ack;
                    for branch in branches.iter().skip(1) {
                        // If receives success from previous child, tick current child.
                        let loc_tick = prev_success;
                        let loc_success = self.cs.new_location(pg_id)?;
                        // If receives running from current child, return running to parent
                        let loc_running = pt_running;
                        let loc_failure = self.cs.new_location(pg_id)?;
                        let loc_halt = self.cs.new_location(pg_id)?;
                        // If previous child acknowledge halting, halts current child.
                        let loc_ack = prev_ack;
                        // If receives failure from previous child, halt current child.
                        self.cs.add_transition(
                            pg_id,
                            prev_failure,
                            halt_after_failure,
                            loc_halt,
                            None,
                        )?;
                        self.build_bt_node(
                            pg_id,
                            loc_tick,
                            loc_success,
                            loc_running,
                            loc_failure,
                            loc_halt,
                            loc_ack,
                            step,
                            branch,
                        )?;
                        prev_success = loc_success;
                        prev_failure = loc_failure;
                        prev_ack = loc_ack;
                    }
                    // If all children are successful, return success to father node.
                    self.cs
                        .add_transition(pg_id, prev_success, step, pt_success, None)?;
                    // If all children acknowledge halting, return ack to father node.
                    self.cs
                        .add_transition(
                            pg_id,
                            prev_ack,
                            step,
                            pt_ack,
                            Some(Expression::Not(Box::new(Expression::Var(
                                halting_after_failure,
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
                            Some(Expression::Var(halting_after_failure)),
                        )
                        .expect("hand-made args");
                } else {
                    todo!()
                }
            }
            BtNode::RFbk(branches) => {
                if let Some(branch) = branches.first() {
                    let halt_after_success = self.cs.new_action(pg_id).expect("{pg_id:?} exists");
                    let halting_after_success = self
                        .cs
                        .new_var(pg_id, Type::Boolean)
                        .expect("{pg_id:?} exists");
                    self.cs
                        .add_effect(
                            pg_id,
                            halt_after_success,
                            halting_after_success,
                            Expression::Boolean(true),
                        )
                        .expect("hand-picked arguments");
                    let success_after_halting =
                        self.cs.new_action(pg_id).expect("{pg_id:?} exists");
                    self.cs
                        .add_effect(
                            pg_id,
                            success_after_halting,
                            halting_after_success,
                            Expression::Boolean(false),
                        )
                        .expect("hand-picked arguments");
                    let loc_tick = self.cs.new_location(pg_id)?;
                    let loc_success = self.cs.new_location(pg_id)?;
                    let loc_running = self.cs.new_location(pg_id)?;
                    let loc_failure = self.cs.new_location(pg_id)?;
                    let loc_halt = self.cs.new_location(pg_id)?;
                    let loc_ack = self.cs.new_location(pg_id)?;
                    // If receives tick from parent, tick first child.
                    self.cs
                        .add_transition(pg_id, pt_tick, step, loc_tick, None)?;
                    // If parent sends halt, halt first child.
                    self.cs
                        .add_transition(pg_id, pt_halt, step, loc_halt, None)?;
                    self.build_bt_node(
                        pg_id,
                        loc_tick,
                        loc_success,
                        loc_running,
                        loc_failure,
                        loc_halt,
                        loc_ack,
                        step,
                        branch,
                    )?;
                    let mut prev_success = loc_success;
                    let mut prev_failure = loc_failure;
                    let mut prev_ack = loc_ack;
                    for branch in branches.iter().skip(1) {
                        let loc_tick = self.cs.new_location(pg_id)?;
                        let loc_success = self.cs.new_location(pg_id)?;
                        let loc_running = self.cs.new_location(pg_id)?;
                        let loc_failure = self.cs.new_location(pg_id)?;
                        let loc_halt = self.cs.new_location(pg_id)?;
                        let loc_ack = self.cs.new_location(pg_id)?;
                        // If receives failure from previous child, tick current child.
                        self.cs
                            .add_transition(pg_id, prev_failure, step, loc_tick, None)?;
                        // If receives running from current child, return running to parent
                        self.cs
                            .add_transition(pg_id, loc_running, step, pt_running, None)?;
                        // If receives success from previous child, halt current child.
                        self.cs.add_transition(
                            pg_id,
                            prev_success,
                            halt_after_success,
                            loc_halt,
                            None,
                        )?;
                        // If previous child acknowledge halting, halts current child.
                        self.cs
                            .add_transition(pg_id, prev_ack, step, loc_halt, None)?;
                        self.build_bt_node(
                            pg_id,
                            loc_tick,
                            loc_success,
                            loc_running,
                            loc_failure,
                            loc_halt,
                            loc_ack,
                            step,
                            branch,
                        )?;
                        prev_success = loc_success;
                        prev_failure = loc_failure;
                        prev_ack = loc_ack;
                    }
                    // If all children are failures, return failure to father node.
                    self.cs
                        .add_transition(pg_id, prev_failure, step, pt_failure, None)?;
                    // If all children acknowledge halting, return ack to father node.
                    self.cs
                        .add_transition(
                            pg_id,
                            prev_ack,
                            step,
                            pt_ack,
                            Some(Expression::Not(Box::new(Expression::Var(
                                halting_after_success,
                            )))),
                        )
                        .expect("hand-made args");
                    // If all children acknowledge halting after a success, return success to father node.
                    self.cs
                        .add_transition(
                            pg_id,
                            prev_ack,
                            success_after_halting,
                            pt_success,
                            Some(Expression::Var(halting_after_success)),
                        )
                        .expect("hand-made args");
                } else {
                    todo!()
                }
            }
            BtNode::MSeq(_branches) => todo!(),
            BtNode::MFbk(_branches) => todo!(),
            BtNode::Invr(branch) => {
                // Swap success and failure.
                self.build_bt_node(
                    pg_id, pt_tick, pt_failure, pt_running, pt_success, pt_halt, pt_ack, step,
                    branch,
                )?;
            }
            BtNode::LAct(id) | BtNode::LCnd(id) => {
                trace!("building bt leaf {id}");
                let builder = self
                    .fsm_builders
                    .values()
                    .find(|b| b.pg_id == pg_id)
                    .expect("it must exist");
                let pg_idx = builder.index as Integer;
                let ext_queue = builder.ext_queue;
                let target = id;
                let target_builder = self.fsm_builders.get(target).unwrap();
                let target_ext_queue = target_builder.ext_queue;

                // TICK
                let event = TICK_CALL;
                // Create event, if it does not exist already.
                let event_idx = *self.event_indexes.get(event).unwrap();
                let send_event = self.cs.new_communication(
                    pg_id,
                    target_ext_queue,
                    Message::Send(CsExpression::Tuple(vec![
                        Expression::Integer(event_idx as Integer),
                        Expression::Integer(pg_idx),
                    ])),
                )?;
                let tick_sent = self.cs.new_location(pg_id)?;
                self.cs
                    .add_transition(pg_id, pt_tick, send_event, tick_sent, None)?;
                let tick_response = self
                    .cs
                    .new_var(pg_id, Type::Product(vec![Type::Integer, Type::Integer]))
                    .expect("{pg_id:?} exists");
                let get_tick_response = self
                    .cs
                    .new_communication(pg_id, ext_queue, Message::Receive(tick_response))
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
                        *self.event_indexes.get(TICK_RETURN).unwrap(),
                        RESULT.to_owned(),
                    ))
                    .or_insert(self.cs.new_channel(Type::Integer, None));
                let tick_response_param = self
                    .cs
                    .new_var(pg_id, Type::Integer)
                    .expect("{pg_id:?} exists");
                let get_tick_response_param = self
                    .cs
                    .new_communication(
                        pg_id,
                        tick_response_param_chn,
                        Message::Receive(tick_response_param),
                    )
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
                    .add_transition(
                        pg_id,
                        got_tick_response_param,
                        step,
                        pt_success,
                        Some(Expression::Equal(Box::new((
                            Expression::Var(tick_response_param),
                            Expression::Integer(*self.enums.get("SUCCESS").unwrap()),
                        )))),
                    )
                    .expect("hope this works");
                self.cs
                    .add_transition(
                        pg_id,
                        got_tick_response_param,
                        step,
                        pt_failure,
                        Some(Expression::Equal(Box::new((
                            Expression::Var(tick_response_param),
                            Expression::Integer(*self.enums.get("FAILURE").unwrap()),
                        )))),
                    )
                    .expect("hope this works");
                self.cs
                    .add_transition(
                        pg_id,
                        got_tick_response_param,
                        step,
                        pt_running,
                        Some(Expression::Equal(Box::new((
                            Expression::Var(tick_response_param),
                            Expression::Integer(*self.enums.get("RUNNING").unwrap()),
                        )))),
                    )
                    .expect("hope this works");

                // HALT
                let event = HALT_CALL;
                // Create event, if it does not exist already.
                let event_idx = self.event_index(event);
                let send_event = self.cs.new_communication(
                    pg_id,
                    target_ext_queue,
                    Message::Send(CsExpression::Tuple(vec![
                        Expression::Integer(event_idx as Integer),
                        Expression::Integer(pg_idx),
                    ])),
                )?;
                let halt_sent = self.cs.new_location(pg_id)?;
                self.cs
                    .add_transition(pg_id, pt_halt, send_event, halt_sent, None)?;
                let halt_response = self
                    .cs
                    .new_var(pg_id, Type::Product(vec![Type::Integer, Type::Integer]))
                    .expect("{pg_id:?} exists");
                let get_halt_response = self
                    .cs
                    .new_communication(pg_id, ext_queue, Message::Receive(halt_response))
                    .expect("hand-made args");
                let got_halt_response = pt_ack;
                self.cs
                    .add_transition(pg_id, halt_sent, get_halt_response, got_halt_response, None)
                    .expect("hand-made args");
            }
        }

        Ok(())
    }

    // TODO: Optimize CS by removing unnecessary states:
    // - initialize state if empty datamodel
    fn build_fsm(&mut self, fsm: &Fsm) -> anyhow::Result<()> {
        trace!("build fsm {}", fsm.id);
        // Initialize fsm.
        let pg_builder = self
            .fsm_builders
            .get(&fsm.id)
            .expect(&format!("builder for {} must already exist", fsm.id));
        let pg_id = pg_builder.pg_id;
        let pg_index = pg_builder.index as Integer;
        let ext_queue = pg_builder.ext_queue;
        // Initial location of Program Graph.
        let initial_loc = self
            .cs
            .initial_location(pg_id)
            .expect("program graph must exist");
        let initialize = self.cs.new_action(pg_id).expect("program graph must exist");
        let mut need_to_initialize = false;
        // Initialize variables from datamodel
        let mut vars = HashMap::new();
        for (location, (type_name, expr)) in fsm.datamodel.iter() {
            let scan_type = self
                .scan_types
                .get(type_name)
                .ok_or(anyhow!("unknown type"))?;
            let var = self
                .cs
                .new_var(pg_id, scan_type.to_owned())
                .expect("program graph exists!");
            vars.insert(location.to_owned(), (var, scan_type.to_owned()));
            // Initialize variable with `expr`, if any, by adding it as effect of `initialize` action.
            if let Some(expr) = expr {
                let expr =
                    self.expression(pg_id, expr, &fsm.interner, &vars, None, &HashMap::new())?;
                // This might fail if `expr` does not typecheck.
                self.cs.add_effect(pg_id, initialize, var, expr)?;
                need_to_initialize = true;
            }
        }
        // Transition initializing datamodel variables.
        // After initializing datamodel, transition to location representing point-of-entry of initial state of State Chart.
        let initial_state;
        if need_to_initialize {
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
        states.insert(fsm.initial.to_owned(), initial_state);
        // Var representing the current event and origin pair
        let current_event_and_origin_var = self
            .cs
            .new_var(pg_id, Type::Product(vec![Type::Integer, Type::Integer]))
            .expect("program graph exists!");
        // Var representing the current event
        let current_event_var = self
            .cs
            .new_var(pg_id, Type::Integer)
            .expect("program graph exists!");
        // Implement internal queue
        let int_queue = self.cs.new_channel(Type::Integer, None);
        let dequeue_int = self
            .cs
            .new_communication(pg_id, int_queue, Message::Receive(current_event_var))
            .expect("hand-coded args");
        // Variable that will store origin of last processed event.
        let origin_var = self
            .cs
            .new_var(pg_id, Type::Integer)
            .expect("program graph exists!");
        let set_int_origin = self.cs.new_action(pg_id).expect("program graph exists!");
        self.cs
            .add_effect(
                pg_id,
                set_int_origin,
                origin_var,
                CsExpression::Integer(pg_index),
            )
            .expect("hand-coded args");
        // Implement external queue
        let dequeue_ext = self
            .cs
            .new_communication(
                pg_id,
                ext_queue,
                Message::Receive(current_event_and_origin_var),
            )
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
                    Box::new(CsExpression::Var(current_event_and_origin_var)),
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
                    Box::new(CsExpression::Var(current_event_and_origin_var)),
                ),
            )
            .expect("hand-coded args");
        // Action representing checking the next transition
        let next_transition = self.cs.new_action(pg_id).expect("program graph exists!");

        // Create variables and channels for the storage of the parameters sent by external events.
        let mut params: HashMap<(usize, String), (CsVar, Type)> = HashMap::new();
        for event_builder in self
            .events
            .iter()
            // only consider events that can activate some transition and that some other process is sending.
            .filter(|eb| eb.receivers.contains(&pg_id) && !eb.senders.is_empty())
        {
            for (param_name, param_type) in event_builder.params.iter() {
                // Variable where to store parameter.
                let param_var = self
                    .cs
                    .new_var(pg_id, param_type.to_owned())
                    .expect("hand-made input");
                let old = params.insert(
                    (event_builder.index, param_name.to_owned()),
                    (param_var, param_type.to_owned()),
                );
                assert!(old.is_none());
                for sender_id in event_builder.senders.iter() {
                    self.parameters
                        .entry((
                            *sender_id,
                            pg_id,
                            event_builder.index,
                            param_name.to_owned(),
                        ))
                        .or_insert_with(|| self.cs.new_channel(param_type.to_owned(), None));
                }
            }
        }
        // Make non-mut
        let params = params;

        // Consider each of the fsm's states
        for (state_id, state) in fsm.states.iter() {
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
                    pg_index,
                    int_queue,
                    onentry_loc,
                    &vars,
                    Some(origin_var),
                    &HashMap::new(),
                    &fsm.interner,
                )?;
            }

            // Location where eventless/NULL transitions activate
            let mut null_trans = onentry_loc;
            // Location where internal events are dequeued
            let int_queue_loc = self.cs.new_location(pg_id).expect("program graph exists!");
            // Location where the origin of internal events is set as own.
            let int_origin_loc = self.cs.new_location(pg_id).expect("program graph exists!");
            // Location where external events are dequeued
            let ext_queue_loc = self.cs.new_location(pg_id).expect("program graph exists!");
            // Location where the index/origin of external events are dequeued
            let ext_event_processing_loc =
                self.cs.new_location(pg_id).expect("program graph exists!");
            // Location where eventful transitions activate
            let mut eventful_trans = self.cs.new_location(pg_id).expect("program graph exists!");
            // Transition dequeueing a new internal event and searching for first active eventful transition
            self.cs
                .add_transition(pg_id, int_queue_loc, dequeue_int, int_origin_loc, None)
                .expect("hand-coded args");
            // Transition dequeueing a new internal event and searching for first active eventful transition
            self.cs
                .add_transition(pg_id, int_origin_loc, set_int_origin, eventful_trans, None)
                .expect("hand-coded args");
            // Action denoting checking if internal queue is empty;
            // if so, move to external queue.
            // Notice that one and only one of `int_dequeue` and `empty_int_queue` can be executed at a given time.
            let empty_int_queue = self
                .cs
                .new_communication(pg_id, int_queue, Message::ProbeEmptyQueue)
                .expect("hand-coded args");
            self.cs
                .add_transition(pg_id, int_queue_loc, empty_int_queue, ext_queue_loc, None)
                .expect("hand-coded args");
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
            let ext_event_processing_param =
                self.cs.new_location(pg_id).expect("program graph exists!");
            self.cs
                .add_transition(
                    pg_id,
                    ext_event_processing_loc,
                    process_ext_event,
                    ext_event_processing_param,
                    None,
                )
                .expect("hand-coded args");
            let step = self.cs.new_action(pg_id).expect("PG exists");
            // Retreive external event's parameters
            // We need to set up the parameter-passing channel for every possible event that could be sent,
            // from any possible other fsm,
            // and for any parameter of the event.
            for event_builder in self
                .events
                .iter()
                .filter(|eb| eb.receivers.contains(&pg_id) && !eb.senders.is_empty())
            {
                for sender_id in event_builder.senders.iter() {
                    // TODO FIXME fsm builders should be indexed
                    let sender_index = self
                        .fsm_builders
                        .iter()
                        .find(|(_, b)| b.pg_id == *sender_id)
                        .expect("sender must exist")
                        .1
                        .index;
                    let mut is_event_sender = Some(Expression::And(vec![
                        Expression::Equal(Box::new((
                            Expression::Integer(event_builder.index as Integer),
                            Expression::Var(current_event_var),
                        ))),
                        Expression::Equal(Box::new((
                            Expression::Integer(sender_index as Integer),
                            Expression::Var(origin_var),
                        ))),
                    ]));
                    let mut current_loc = ext_event_processing_param;
                    for (param_name, _) in event_builder.params.iter() {
                        let (param_var, _) = params
                            .get(&(event_builder.index, param_name.to_owned()))
                            .expect("param should already be in");
                        // Channel where to retreive the parameter from.
                        let channel = *self
                            .parameters
                            .get(&(
                                *sender_id,
                                pg_id,
                                event_builder.index,
                                param_name.to_owned(),
                            ))
                            .expect("parameters have already been registered");
                        let read_param = self
                            .cs
                            .new_communication(pg_id, channel, Message::Receive(*param_var))
                            .expect("hard-coded input");
                        let next = self.cs.new_location(pg_id).expect("program graph exists!");
                        self.cs
                            .add_transition(
                                pg_id,
                                current_loc,
                                read_param,
                                next,
                                // Need to check only once, so `take` Option
                                is_event_sender.take(),
                            )
                            .expect("hand-coded args");
                        current_loc = next;
                    }
                    self.cs
                        .add_transition(pg_id, current_loc, step, eventful_trans, None)
                        .expect("has to work");
                }
            }

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

                // Location corresponding to checking if the transition is active.
                // Has to be defined depending on the type of transition.
                let check_trans_loc;
                // Action correponding to executing the transition.
                let exec_transition = self.cs.new_action(pg_id).expect("{pg_id:?} exists");
                // Location corresponding to verifying the transition is not active and moving to next one.
                let next_trans_loc = self.cs.new_location(pg_id).expect("{pg_id:?} exists");

                // Set up origin and parameters for conditional/executable content.
                let exec_origin;
                let exec_params;
                if let Some(event_name) = transition.event.as_ref() {
                    let event_index = *self
                        .event_indexes
                        .get(event_name)
                        .expect("event must be registered");
                    exec_origin = Some(origin_var);
                    exec_params = params
                        .iter()
                        .filter(|((ev_ix, _), _)| *ev_ix == event_index)
                        .map(|((_, name), (var, tp))| (name.to_owned(), (*var, tp.to_owned())))
                        .collect::<HashMap<String, (CsVar, Type)>>();
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
                        self.expression(
                            pg_id,
                            cond,
                            &fsm.interner,
                            &vars,
                            exec_origin,
                            &exec_params,
                        )
                    })
                    .transpose()?;
                // Guard for transition.
                // Has to be defined depending on the type of transition, etc...
                let guard;
                // Proceed on whether the transition is eventless or activated by event.
                if let Some(event) = &transition.event {
                    // Create event, if it does not exist already.
                    let event_idx =
                        *self.event_indexes.get(event).expect("already exists") as Integer;
                    // let event_idx = self.event_index(event) as Integer;
                    // Check if the current event (internal or external) corresponds to the event activating the transition.
                    let event_match = CsExpression::Equal(Box::new((
                        CsExpression::Var(current_event_var),
                        CsExpression::Integer(event_idx),
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
                    // // NULL (unnamed) event transition
                    // No event needs to happen in order to trigger this transition.
                    guard = cond;
                    // Check this transition after the other eventless transitions.
                    check_trans_loc = null_trans;
                    // Move location of next eventless transitions to a new location.
                    null_trans = next_trans_loc;
                }

                // If transition is active, execute the relevant executable content and then the transition to the target.
                let mut exec_trans_loc = self.cs.new_location(pg_id)?;
                self.cs.add_transition(
                    pg_id,
                    check_trans_loc,
                    exec_transition,
                    exec_trans_loc,
                    guard.to_owned(),
                )?;
                // First execute the executable content of the state's `on_exit` tag,
                // then that of the `transition` tag.
                for exec in state.on_exit.iter().chain(transition.effects.iter()) {
                    exec_trans_loc = self.add_executable(
                        exec,
                        pg_id,
                        pg_index,
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
                    .add_transition(pg_id, exec_trans_loc, exec_transition, target_loc, None)?;
                // If the current transition is not active, move on to check the next one.
                let not_guard = guard
                    .map(|guard| CsExpression::Not(Box::new(guard)))
                    .unwrap_or(CsExpression::Boolean(false));
                self.cs.add_transition(
                    pg_id,
                    check_trans_loc,
                    next_transition,
                    next_trans_loc,
                    Some(not_guard),
                )?;
            }

            // Connect NULL events with named events
            // by transitioning from last "NUll" location to dequeuing event location.
            self.cs
                .add_transition(pg_id, null_trans, next_transition, int_queue_loc, None)?;
            // Return to dequeue a new (internal or external) event.
            self.cs
                .add_transition(pg_id, eventful_trans, next_transition, int_queue_loc, None)?;
        }
        Ok(())
    }

    fn add_executable(
        &mut self,
        executable: &Executable,
        pg_id: PgId,
        pg_idx: Integer,
        int_queue: Channel,
        loc: CsLocation,
        vars: &HashMap<String, (CsVar, Type)>,
        origin: Option<CsVar>,
        params: &HashMap<String, (CsVar, Type)>,
        interner: &boa_interner::Interner,
    ) -> Result<CsLocation, anyhow::Error> {
        match executable {
            Executable::Raise { event } => {
                // Create event, if it does not exist already.
                let event_idx = self.event_index(event);
                let raise = self.cs.new_communication(
                    pg_id,
                    int_queue,
                    Message::Send(CsExpression::Integer(event_idx as Integer)),
                )?;
                let next_loc = self.cs.new_location(pg_id)?;
                // queue the internal event
                self.cs.add_transition(pg_id, loc, raise, next_loc, None)?;
                Ok(next_loc)
            }
            Executable::Send {
                event,
                target,
                params: send_params,
            } => match target {
                Target::Id(target) => {
                    let target_builder = self
                        .fsm_builders
                        .get(target)
                        // TODO FIXME This should be fallible because `target` is unvalidated user input
                        .expect(&format!("builder for {} already exists", target));
                    let target_id = target_builder.pg_id;
                    let event_idx = *self
                        .event_indexes
                        .get(event)
                        .expect("builder for {event} already exists");
                    let send_event = self.cs.new_communication(
                        pg_id,
                        target_builder.ext_queue,
                        Message::Send(CsExpression::Tuple(vec![
                            Expression::Integer(event_idx as Integer),
                            Expression::Integer(pg_idx),
                        ])),
                    )?;

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
                    let targetexpr =
                        self.expression(pg_id, targetexpr, interner, vars, origin, params)?;
                    let event_idx = *self
                        .event_indexes
                        .get(event)
                        .ok_or(anyhow!("event not found"))?;
                    // Location representing having sent the event to the correct target after evaluating expression.
                    let done_loc = self.cs.new_location(pg_id).expect("PG exists");
                    let complete_send = self.cs.new_action(pg_id).expect("PG exists");
                    for &target_id in self.events[event_idx].receivers.clone().iter() {
                        // FIXME TODO: there should be an indexing to avoid search
                        let (_target_name, target_builder) = self
                            .fsm_builders
                            .iter()
                            .find(|(_, b)| b.pg_id == target_id)
                            .expect("fsm has to be here");
                        let target_index = target_builder.index;
                        let target_ext_queue = target_builder.ext_queue;
                        let send_event = self
                            .cs
                            .new_communication(
                                pg_id,
                                target_ext_queue,
                                Message::Send(CsExpression::Tuple(vec![
                                    Expression::Integer(event_idx as Integer),
                                    Expression::Integer(pg_idx),
                                ])),
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
                                    CsExpression::Integer(target_index as Integer),
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
                            .add_transition(pg_id, next_loc, complete_send, done_loc, None)
                            .expect("hand-made args");
                    }

                    // Return exit point
                    Ok(done_loc)
                }
            },
            Executable::Assign { location, expr } => {
                // Add a transition that perform the assignment via the effect of the `assign` action.
                let expr = self.expression(pg_id, expr, interner, &vars, origin, params)?;
                let (var, _scan_type) = vars.get(location).ok_or(anyhow!("undefined variable"))?;
                let assign = self.cs.new_action(pg_id)?;
                self.cs.add_effect(pg_id, assign, *var, expr)?;
                let next_loc = self.cs.new_location(pg_id)?;
                self.cs.add_transition(pg_id, loc, assign, next_loc, None)?;
                Ok(next_loc)
            }
        }
    }

    fn send_param(
        &mut self,
        pg_id: PgId,
        target_id: PgId,
        param: &Param,
        event_idx: usize,
        param_loc: CsLocation,
        vars: &HashMap<String, (CsVar, Type)>,
        origin: Option<CsVar>,
        params: &HashMap<String, (CsVar, Type)>,
        interner: &boa_interner::Interner,
    ) -> Result<CsLocation, anyhow::Error> {
        // Get param type.
        let scan_type = self
            .scan_types
            .get(&param.omg_type)
            .cloned()
            .ok_or(anyhow!("undefined type"))?;
        // Build expression from ECMAScript expression.
        let expr = self.expression(pg_id, &param.expr, interner, &vars, origin, params)?;
        // Retreive or create channel for parameter passing.
        let param_chn = *self
            .parameters
            .entry((pg_id, target_id, event_idx, param.name.to_owned()))
            .or_insert(self.cs.new_channel(scan_type, None));
        // Can return error if expr is badly typed
        let pass_param = self
            .cs
            .new_communication(pg_id, param_chn, Message::Send(expr))?;
        let next_loc = self.cs.new_location(pg_id).expect("PG exists");
        self.cs
            .add_transition(pg_id, param_loc, pass_param, next_loc, None)
            .expect("hand-made params are correct");
        Ok(next_loc)
    }

    fn expression(
        &mut self,
        pg_id: PgId,
        expr: &boa_ast::Expression,
        interner: &boa_interner::Interner,
        vars: &HashMap<String, (CsVar, Type)>,
        origin: Option<CsVar>,
        params: &HashMap<String, (CsVar, Type)>,
    ) -> anyhow::Result<CsExpression> {
        let expr = match expr {
            boa_ast::Expression::This => todo!(),
            boa_ast::Expression::Identifier(ident) => {
                let ident: &str = interner
                    .resolve(ident.sym())
                    .ok_or(anyhow!("unknown identifier"))?
                    .utf8()
                    .ok_or(anyhow!("not utf8"))?;
                match ident {
                    "_event" => todo!(),
                    ident => self
                        .enums
                        .get(ident)
                        .map(|i| Expression::Integer(*i))
                        .or_else(|| vars.get(ident).map(|(var, _)| CsExpression::Var(*var)))
                        .ok_or(anyhow!("unknown identifier"))?,
                }
            }
            boa_ast::Expression::Literal(lit) => {
                use boa_ast::expression::literal::Literal;
                match lit {
                    Literal::String(_) => todo!(),
                    Literal::Num(_) => todo!(),
                    Literal::Int(i) => Expression::Integer(*i),
                    Literal::BigInt(_) => todo!(),
                    Literal::Bool(b) => Expression::Boolean(*b),
                    Literal::Null => todo!(),
                    Literal::Undefined => todo!(),
                }
            }
            boa_ast::Expression::RegExpLiteral(_) => todo!(),
            boa_ast::Expression::ArrayLiteral(_) => todo!(),
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
                use boa_ast::expression::access::{PropertyAccess, PropertyAccessField};
                match prop_acc {
                    PropertyAccess::Simple(simp_prop_acc) => match simp_prop_acc.field() {
                        // FIXME WARN this makes overly simplified assumptions on field access and will not work with complex types
                        PropertyAccessField::Const(sym) => {
                            let ident: &str = interner
                                .resolve(*sym)
                                .ok_or(anyhow!("unknown identifier"))?
                                .utf8()
                                .ok_or(anyhow!("not utf8"))?;
                            match ident {
                                "origin" => {
                                    let origin = origin.ok_or(anyhow!("origin not available"))?;
                                    CsExpression::Var(origin)
                                }
                                var_ident => {
                                    if let Some((param_var, var_type)) = params.get(var_ident) {
                                        match var_type {
                                            Type::Boolean => CsExpression::Var(*param_var),
                                            Type::Integer => CsExpression::Var(*param_var),
                                            Type::Product(_) => todo!(),
                                        }
                                    } else {
                                        return Err(anyhow!(
                                            "no parameter `{ident}` found among: {params:#?}"
                                        ));
                                    }
                                }
                            }
                        }
                        PropertyAccessField::Expr(_) => todo!(),
                    },
                    PropertyAccess::Private(_) => todo!(),
                    PropertyAccess::Super(_) => todo!(),
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
            boa_ast::Expression::Unary(_) => todo!(),
            boa_ast::Expression::Update(_) => todo!(),
            boa_ast::Expression::Binary(bin) => {
                use boa_ast::expression::operator::binary::{ArithmeticOp, BinaryOp, RelationalOp};
                match bin.op() {
                    BinaryOp::Arithmetic(ar_bin) => {
                        let lhs =
                            self.expression(pg_id, bin.lhs(), interner, vars, origin, params)?;
                        let rhs =
                            self.expression(pg_id, bin.rhs(), interner, vars, origin, params)?;
                        match ar_bin {
                            ArithmeticOp::Add => CsExpression::Sum(vec![lhs, rhs]),
                            ArithmeticOp::Sub => {
                                CsExpression::Sum(vec![lhs, CsExpression::Opposite(Box::new(rhs))])
                            }
                            ArithmeticOp::Div => todo!(),
                            ArithmeticOp::Mul => todo!(),
                            ArithmeticOp::Exp => todo!(),
                            ArithmeticOp::Mod => todo!(),
                        }
                    }
                    BinaryOp::Bitwise(_) => todo!(),
                    BinaryOp::Relational(rel_bin) => {
                        // WARN FIXME TODO: this assumes relations are between integers
                        let lhs =
                            self.expression(pg_id, bin.lhs(), interner, vars, origin, params)?;
                        let rhs =
                            self.expression(pg_id, bin.rhs(), interner, vars, origin, params)?;
                        match rel_bin {
                            RelationalOp::Equal => CsExpression::Equal(Box::new((lhs, rhs))),
                            RelationalOp::NotEqual => todo!(),
                            RelationalOp::StrictEqual => todo!(),
                            RelationalOp::StrictNotEqual => todo!(),
                            RelationalOp::GreaterThan => {
                                CsExpression::Greater(Box::new((lhs, rhs)))
                            }
                            RelationalOp::GreaterThanOrEqual => {
                                CsExpression::GreaterEq(Box::new((lhs, rhs)))
                            }
                            RelationalOp::LessThan => CsExpression::Less(Box::new((lhs, rhs))),
                            RelationalOp::LessThanOrEqual => {
                                CsExpression::LessEq(Box::new((lhs, rhs)))
                            }
                            RelationalOp::In => todo!(),
                            RelationalOp::InstanceOf => todo!(),
                        }
                    }
                    BinaryOp::Logical(_) => todo!(),
                    BinaryOp::Comma => todo!(),
                }
            }
            boa_ast::Expression::BinaryInPrivate(_) => todo!(),
            boa_ast::Expression::Conditional(_) => todo!(),
            boa_ast::Expression::Await(_) => todo!(),
            boa_ast::Expression::Yield(_) => todo!(),
            boa_ast::Expression::Parenthesized(_) => todo!(),
            _ => todo!(),
        };
        Ok(expr)
    }

    fn build(self) -> CsModel {
        let fsm_names = self
            .fsm_builders
            .iter()
            .map(|(name, id)| (id.pg_id, name.to_owned()))
            .collect();
        CsModel {
            cs: self.cs.build(),
            fsm_names,
        }
    }
}
