//! Model builder for SCAN's XML specification format.

use crate::parser::{Executable, If, OmgType, OmgTypes, Param, Parser, Scxml, Send, Target};
use anyhow::anyhow;
use boa_interner::{Interner, ToInternedString};
use log::{info, trace};
use rand::rngs::SmallRng;
use scan_core::{channel_system::*, *};
use std::{
    collections::{HashMap, HashSet},
    ops::Not,
};

// TODO:
//
// -[ ] WARN FIXME System is fragile if name/id/path do not coincide

#[derive(Debug, Clone)]
pub struct ScxmlModel {
    pub fsm_names: HashMap<PgId, String>,
    pub fsm_indexes: HashMap<usize, String>,
    pub parameters: HashMap<Channel, (PgId, PgId, usize, String)>,
    pub int_queues: HashSet<Channel>,
    pub ext_queues: HashMap<Channel, PgId>,
    pub events: Vec<String>,
    pub ports: Vec<(String, Type)>,
    pub assumes: Vec<String>,
    pub guarantees: Vec<String>,
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
enum EcmaObj<V: Clone> {
    PrimitiveData(Expression<V>, String),
    // Associates property name with content, which can be another object.
    Properties(HashMap<String, EcmaObj<V>>),
}

/// Builder turning a [`Parser`] into a [`ChannelSystem`].
#[derive(Debug, Default)]
pub struct ModelBuilder {
    cs: ChannelSystemBuilder<SmallRng>,
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
    guarantees: Vec<(String, Pmtl<usize>)>,
    assumes: Vec<(String, Pmtl<usize>)>,
    predicates: Vec<Expression<Atom>>,
    ports: HashMap<String, (Atom, Val)>,
    // extra data
    int_queues: HashSet<Channel>,
}

impl ModelBuilder {
    /// Turns the [`Parser`] into a [`ChannelSystem`].
    ///
    /// Can fail if the model specification contains semantic errors
    /// (particularly type mismatches)
    /// or references to non-existing items.
    pub fn build(mut parser: Parser) -> anyhow::Result<(CsModel<SmallRng>, ScxmlModel)> {
        let mut model_builder = ModelBuilder::default();
        model_builder.build_types(&parser.types)?;
        model_builder.prebuild_processes(&mut parser)?;

        info!(target: "build", "Visit process list");
        for (_id, fsm) in parser.process_list.iter() {
            model_builder.build_fsm(fsm, &mut parser.interner)?;
        }

        model_builder.build_ports(&parser)?;
        model_builder.build_properties(&parser)?;

        let model = model_builder.build_model();

        Ok(model)
    }

    fn build_types(&mut self, omg_types: &OmgTypes) -> anyhow::Result<()> {
        info!(target: "build", "Building types");
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

    fn prebuild_processes(&mut self, parser: &mut Parser) -> anyhow::Result<()> {
        for (id, fsm) in parser.process_list.iter_mut() {
            let pg_id = self.fsm_builder(id).pg_id;
            self.prebuild_fsms(pg_id, fsm, &parser.interner)?;
        }
        Ok(())
    }

    fn prebuild_fsms(
        &mut self,
        pg_id: PgId,
        fmt: &mut Scxml,
        interner: &Interner,
    ) -> anyhow::Result<()> {
        let mut types = HashMap::new();
        for data in &fmt.datamodel {
            types.insert(data.id.to_owned(), data.omg_type.as_str().to_owned());
        }
        for (_, state) in fmt.states.iter_mut() {
            for exec in state.on_entry.iter_mut() {
                self.prebuild_exec(pg_id, exec, &types, interner)?;
            }
            for transition in state.transitions.iter_mut() {
                if let Some(ref event) = transition.event {
                    // Event may or may not have been processed before
                    let event_index = self.event_index(event);
                    let builder = self.events.get_mut(event_index).expect("index must exist");
                    builder.receivers.insert(pg_id);
                }
                for exec in transition.effects.iter_mut() {
                    self.prebuild_exec(pg_id, exec, &types, interner)?;
                }
            }
            for exec in state.on_exit.iter_mut() {
                self.prebuild_exec(pg_id, exec, &types, interner)?;
            }
        }
        Ok(())
    }

    fn prebuild_exec(
        &mut self,
        pg_id: PgId,
        executable: &mut Executable,
        types: &HashMap<String, String>,
        interner: &Interner,
    ) -> anyhow::Result<()> {
        match executable {
            Executable::Assign {
                location: _,
                expr: _,
            } => Ok(()),
            Executable::Raise { event: _ } => Ok(()),
            Executable::Send(Send {
                event,
                target: _,
                delay: _,
                params,
            }) => {
                let event_index = self.event_index(event);
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.senders.insert(pg_id);
                for param in params {
                    let param_type = param.omg_type.to_owned();
                    let param_type = param_type
                        .or_else(|| self.infer_type(&param.expr, types, interner).ok())
                        .ok_or(anyhow!("missing type annotation for param {}", param.name))?;
                    // Update omg_type value so that it contains its type for sure
                    param.omg_type = Some(param_type.to_owned());
                    let builder = self.events.get_mut(event_index).expect("index must exist");
                    let prev_type = builder
                        .params
                        .insert(param.name.to_owned(), param_type.to_owned());
                    // Type parameters should not change type
                    if let Some(prev_type) = prev_type {
                        if prev_type != param_type {
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
                        self.prebuild_exec(pg_id, executable, types, interner)?;
                    }
                }
                for executable in r#else {
                    self.prebuild_exec(pg_id, executable, types, interner)?;
                }
                Ok(())
            }
        }
    }

    fn infer_type(
        &self,
        expr: &boa_ast::Expression,
        types: &HashMap<String, String>,
        interner: &Interner,
    ) -> anyhow::Result<String> {
        match expr {
            boa_ast::Expression::Identifier(ident) => {
                let ident = ident.to_interned_string(interner);
                types
                    .get(&ident)
                    .cloned()
                    .or_else(|| {
                        if self.enums.contains_key(&ident) {
                            Some(String::from("int32"))
                        } else {
                            None
                        }
                    })
                    .ok_or(anyhow!("type cannot be inferred"))
            }
            boa_ast::Expression::Literal(lit) => {
                use boa_ast::expression::literal::Literal;
                match lit {
                    Literal::String(_) => todo!(),
                    Literal::Num(_) => Ok(String::from("f64")),
                    Literal::Int(_) => Ok(String::from("int32")),
                    Literal::BigInt(_) => todo!(),
                    Literal::Bool(_) => Ok(String::from("bool")),
                    _ => unimplemented!(),
                }
            }
            boa_ast::Expression::Unary(unary) => {
                let type_name = self.infer_type(unary.target(), types, interner)?;
                match unary.op() {
                    boa_ast::expression::operator::unary::UnaryOp::Minus
                    | boa_ast::expression::operator::unary::UnaryOp::Plus => Ok(type_name),
                    boa_ast::expression::operator::unary::UnaryOp::Not => Ok(String::from("bool")),
                    _ => unimplemented!(),
                }
            }
            boa_ast::Expression::Binary(bin) => {
                let type_name = self.infer_type(bin.lhs(), types, interner)?;
                let lhs = self
                    .types
                    .get(&type_name)
                    .ok_or(anyhow!("unknown type {type_name}"))?
                    .1
                    .clone();
                let rhs = self
                    .infer_type(bin.lhs(), types, interner)
                    .and_then(|t| self.types.get(&t).ok_or(anyhow!("unknown type {t}")))?
                    .1
                    .clone();
                match bin.op() {
                    boa_ast::expression::operator::binary::BinaryOp::Arithmetic(_) => {
                        if lhs == rhs {
                            Ok(type_name)
                        } else {
                            todo!()
                        }
                    }
                    boa_ast::expression::operator::binary::BinaryOp::Bitwise(_) => todo!(),
                    boa_ast::expression::operator::binary::BinaryOp::Relational(_)
                    | boa_ast::expression::operator::binary::BinaryOp::Logical(_) => {
                        Ok(String::from("bool"))
                    }
                    boa_ast::expression::operator::binary::BinaryOp::Comma => todo!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn build_fsm(&mut self, scxml: &Scxml, interner: &mut Interner) -> anyhow::Result<()> {
        trace!(target: "build", "build fsm {}", scxml.name);
        // Initialize fsm.
        let pg_builder = self
            .fsm_builders
            .get(&scxml.name)
            .unwrap_or_else(|| panic!("builder for {} must already exist", scxml.name));
        let pg_id = pg_builder.pg_id;
        let ext_queue = pg_builder.ext_queue;
        // Initial location of Program Graph.
        let initial_loc = self
            .cs
            // .initial_location(pg_id)
            .new_initial_location(pg_id)
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
                let expr = self.expression(expr, interner, &vars, &None, &HashMap::new())?;
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
                CsExpression::from(u16::from(pg_id) as Integer),
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
                    .ok_or(anyhow!("type {} not found", param_name))?
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
            trace!(target: "build", "build state {}", state_id);
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
                    interner,
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
                            CsExpression::from(u16::from(sender_id) as Integer),
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
                Some(CsExpression::not(CsExpression::or(known_events)))
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
                    target: "build",
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
                    .map(|cond| self.expression(cond, interner, &vars, &exec_origin, &exec_params))
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
                        .map(|cond| CsExpression::and(vec![event_match.clone(), cond]))
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
                        interner,
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
                    .map(CsExpression::not)
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
        interner: &Interner,
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
                delay,
                params: send_params,
            }) => {
                let event_idx = *self
                    .event_indexes
                    .get(event)
                    .ok_or(anyhow!("event not found"))?;
                let mut loc = loc;
                if let Some(delay) = delay {
                    // WARN NOTE FIXME: here we could reuse some other clock instead of creating a new one every time.
                    let clock = self.cs.new_clock(pg_id).expect("new clock");
                    let reset = self.cs.new_action(pg_id).expect("action");
                    self.cs.add_reset(pg_id, reset, clock).expect("add reset");
                    let next_loc = self
                        .cs
                        .new_timed_location(pg_id, &[(clock, None, Some(*delay + 1))])
                        .expect("PG exists");
                    self.cs
                        .add_transition(pg_id, loc, reset, next_loc, None)
                        .expect("params are right");
                    loc = next_loc;
                    let next_loc = self.cs.new_location(pg_id).expect("PG exists");
                    self.cs
                        .add_autonomous_timed_transition(
                            pg_id,
                            loc,
                            next_loc,
                            None,
                            &[(clock, Some(*delay), None)],
                        )
                        .expect("autonomous timed transition");
                    loc = next_loc;
                }
                if let Some(target) = target {
                    let done_loc = self.cs.new_location(pg_id)?;
                    let targets;
                    let target_expr;
                    match target {
                        Target::Id(target) => {
                            let target_builder = self
                                .fsm_builders
                                .get(target)
                                .ok_or(anyhow!(format!("target {target} not found")))?;
                            targets = vec![target_builder.pg_id];
                            target_expr = Some(CsExpression::from(
                                u16::from(target_builder.pg_id) as Integer
                            ));
                        }
                        Target::Expr(targetexpr) => {
                            target_expr =
                                Some(self.expression(targetexpr, interner, vars, &origin, params)?);
                            targets = self.events[event_idx].receivers.iter().cloned().collect();
                        }
                    }
                    for target_id in targets {
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
                                    CsExpression::from(u16::from(pg_id) as Integer),
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
                                target_expr.as_ref().map(|target_expr| {
                                    CsExpression::Equal(Box::new((
                                        CsExpression::from(u16::from(target_id) as Integer),
                                        target_expr.to_owned(),
                                    )))
                                }),
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
                } else {
                    // WARN: This behavior is non-compliant with the SCXML specification
                    // An event sent without specifiying the target is sent to all FSMs that can process it
                    let targets = self.events[event_idx]
                        .receivers
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>();
                    let mut next_loc = loc;
                    for target in targets {
                        let target_name = self.fsm_names.get(&target).cloned();
                        next_loc = self.add_executable(
                            &Executable::Send(Send {
                                event: event.to_owned(),
                                target: target_name.map(Target::Id),
                                delay: *delay,
                                params: send_params.to_owned(),
                            }),
                            pg_id,
                            int_queue,
                            next_loc,
                            vars,
                            origin,
                            params,
                            interner,
                        )?;
                    }
                    Ok(next_loc)
                }
            }
            Executable::Assign { location, expr } => {
                // Add a transition that perform the assignment via the effect of the `assign` action.
                let expr = self.expression(expr, interner, vars, &origin, params)?;
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
                    let cond = self.expression(cond, interner, vars, &origin, params)?;
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
                            Some(Expression::not(cond)),
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
        interner: &Interner,
    ) -> Result<Location, anyhow::Error> {
        // Get param type.
        let scan_type = self
            .types
            .get(param.omg_type.as_ref().expect("type name annotation"))
            .cloned()
            .ok_or(anyhow!("undefined type"))?
            .1;
        // Build expression from ECMAScript expression.
        let expr = self.expression(&param.expr, interner, vars, &origin, params)?;
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
    fn expression<V: Clone>(
        &mut self,
        expr: &boa_ast::Expression,
        interner: &Interner,
        vars: &HashMap<String, (V, String)>,
        origin: &Option<V>,
        params: &HashMap<String, (V, String)>,
    ) -> anyhow::Result<Expression<V>> {
        let expr = match expr {
            boa_ast::Expression::This => todo!(),
            boa_ast::Expression::Identifier(ident) => {
                let ident = ident.to_interned_string(interner);
                self.enums
                    .get(&ident)
                    .map(|i| Expression::from(*i))
                    .or_else(|| {
                        vars.get(&ident).and_then(|(var, t)| {
                            self.types
                                .get(t)
                                .map(|(_, t)| Expression::Var(var.clone(), t.to_owned()))
                            // .ok_or(anyhow!("missing type {t}"))
                        })
                    })
                    .ok_or(anyhow!("unknown identifier: {ident}"))?
            }
            boa_ast::Expression::Literal(lit) => {
                use boa_ast::expression::literal::Literal;
                match lit {
                    Literal::String(_) => todo!(),
                    Literal::Num(f) => Expression::from(*f),
                    Literal::Int(i) => Expression::from(*i),
                    Literal::BigInt(_) => todo!(),
                    Literal::Bool(b) => Expression::from(*b),
                    Literal::Null => todo!(),
                    Literal::Undefined => todo!(),
                }
            }
            boa_ast::Expression::ArrayLiteral(_arr) => {
                todo!()
                // arr
                //             .to_pattern(true)
                //             .ok_or(anyhow!("array syntax error"))?
                //             .into_iter()
                //             .map(|element| match element {})
            }
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
            boa_ast::Expression::Unary(unary) => {
                use boa_ast::expression::operator::unary::UnaryOp;
                let expr = self.expression(unary.target(), interner, vars, origin, params)?;
                match unary.op() {
                    UnaryOp::Minus => -expr,
                    UnaryOp::Plus => expr,
                    UnaryOp::Not => Expression::not(expr),
                    _ => return Err(anyhow!("unimplemented operator")),
                }
            }
            boa_ast::Expression::Binary(bin) => {
                use boa_ast::expression::operator::binary::{
                    ArithmeticOp, BinaryOp, LogicalOp, RelationalOp,
                };
                let lhs = self.expression(bin.lhs(), interner, vars, origin, params)?;
                let rhs = self.expression(bin.rhs(), interner, vars, origin, params)?;
                match bin.op() {
                    BinaryOp::Arithmetic(ar_bin) => match ar_bin {
                        ArithmeticOp::Add => lhs + rhs,
                        ArithmeticOp::Sub => lhs + (-rhs),
                        ArithmeticOp::Div => todo!(),
                        ArithmeticOp::Mul => lhs * rhs,
                        ArithmeticOp::Exp => todo!(),
                        ArithmeticOp::Mod => Expression::Mod(Box::new((lhs, rhs))),
                    },
                    BinaryOp::Relational(rel_bin) => match rel_bin {
                        RelationalOp::Equal => Expression::Equal(Box::new((lhs, rhs))),
                        RelationalOp::NotEqual => !(Expression::Equal(Box::new((lhs, rhs)))),
                        RelationalOp::GreaterThan => Expression::Greater(Box::new((lhs, rhs))),
                        RelationalOp::GreaterThanOrEqual => {
                            Expression::GreaterEq(Box::new((lhs, rhs)))
                        }
                        RelationalOp::LessThan => Expression::Less(Box::new((lhs, rhs))),
                        RelationalOp::LessThanOrEqual => Expression::LessEq(Box::new((lhs, rhs))),
                        _ => return Err(anyhow!("unimplemented operator")),
                    },
                    BinaryOp::Logical(op) => match op {
                        LogicalOp::And => Expression::and(vec![lhs, rhs]),
                        LogicalOp::Or => Expression::or(vec![lhs, rhs]),
                        _ => return Err(anyhow!("unimplemented operator")),
                    },
                    BinaryOp::Comma => todo!(),
                    _ => return Err(anyhow!("unimplemented operator")),
                }
            }
            boa_ast::Expression::Conditional(_) => todo!(),
            boa_ast::Expression::Parenthesized(par) => {
                self.expression(par.expression(), interner, vars, origin, params)?
            }
            _ => return Err(anyhow!("unimplemented expression")),
        };
        Ok(expr)
    }

    fn value(&self, expr: &boa_ast::Expression, interner: &Interner) -> anyhow::Result<Val> {
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
                    Literal::Num(f) => Val::from(*f),
                    Literal::Int(i) => Val::Integer(*i),
                    Literal::Bool(b) => Val::Boolean(*b),
                    _ => return Err(anyhow!("unsupported type")),
                }
            }
            boa_ast::Expression::PropertyAccess(_prop_acc) => {
                todo!()
            }
            boa_ast::Expression::Unary(unary) => {
                use boa_ast::expression::operator::unary::UnaryOp;
                let val = self.value(unary.target(), interner)?;
                match unary.op() {
                    UnaryOp::Minus => match val {
                        Val::Integer(v) => Val::Integer(-v),
                        Val::Float(v) => Val::Float(-v),
                        _ => return Err(anyhow!("non-numeric type")),
                    },
                    UnaryOp::Plus => {
                        if matches!(val, Val::Integer(_) | Val::Float(_)) {
                            val
                        } else {
                            todo!()
                        }
                    }
                    UnaryOp::Not => {
                        if let Val::Boolean(b) = val {
                            Val::Boolean(!b)
                        } else {
                            todo!()
                        }
                    }
                    _ => return Err(anyhow!("unimplemented operator")),
                }
            }
            boa_ast::Expression::Binary(bin) => {
                use boa_ast::expression::operator::binary::{ArithmeticOp, BinaryOp};
                match bin.op() {
                    // TODO: Float arithmetics
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
                            ArithmeticOp::Div if rhs != 0 => Val::Integer(lhs / rhs),
                            ArithmeticOp::Mul => Val::Integer(lhs * rhs),
                            ArithmeticOp::Exp if !rhs.is_negative() => {
                                Val::Integer(lhs.pow(rhs as u32))
                            }
                            ArithmeticOp::Mod if rhs != 0 => Val::Integer(lhs % rhs),
                            _ => return Err(anyhow!("unimplemented expression")),
                        }
                    }
                    BinaryOp::Relational(_rel_bin) => {
                        todo!()
                    }
                    BinaryOp::Logical(_) => todo!(),
                    _ => unimplemented!(),
                }
            }
            _ => return Err(anyhow!("unimplemented expression")),
        };
        Ok(expr)
    }

    fn expression_prop_access<V: Clone>(
        &mut self,
        expr: &boa_ast::Expression,
        interner: &Interner,
        vars: &HashMap<String, (V, String)>,
        origin: &Option<V>,
        params: &HashMap<String, (V, String)>,
    ) -> anyhow::Result<EcmaObj<V>> {
        match expr {
            boa_ast::Expression::This => todo!(),
            boa_ast::Expression::Identifier(ident) => {
                let ident = ident.to_interned_string(interner);
                match ident.as_str() {
                    "_event" => Ok(EcmaObj::Properties(HashMap::from_iter(
                        [
                            (
                                String::from("origin"),
                                EcmaObj::PrimitiveData(
                                    Expression::Var(
                                        origin
                                            .clone()
                                            .ok_or(anyhow!("missing origin of _event"))?,
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
                                                Expression::Var(
                                                    v.clone(),
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
                        ]
                        // WARN: This allows the non-conformant notation `_event.<PARAM>` in place of `_event.data.<PARAM>`
                        // for compatibility with the format produced by AS2FM.
                        // TODO: remove when not necessary anymore.
                        .into_iter()
                        .chain(params.iter().map(|(n, (v, t))| {
                            (
                                n.to_owned(),
                                EcmaObj::PrimitiveData(
                                    Expression::Var(
                                        v.clone(),
                                        self.types
                                            .get(t)
                                            .map(|(_, t)| t.to_owned())
                                            .expect("type of data parameter"),
                                    ),
                                    t.to_owned(),
                                ),
                            )
                        })),
                    ))),
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
                                                    Expression::component(expr, index),
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

    fn build_ports(&mut self, parser: &Parser) -> anyhow::Result<()> {
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
                let init = self.value(init, &parser.interner)?;
                let channel = *self
                    .parameters
                    .get(&(origin, target, event_id, param.to_owned()))
                    .ok_or(anyhow!("param {param} not found"))?;
                self.ports
                    .insert(port_id.to_owned(), (Atom::State(channel), init));
            } else {
                let channel = target_builder.ext_queue;
                self.ports.insert(
                    port_id.to_owned(),
                    (
                        Atom::Event(Event {
                            pg_id: origin,
                            channel,
                            event_type: EventType::Send(Val::Tuple(vec![
                                Val::Integer(event_id as Integer),
                                Val::Integer(u16::from(origin) as Integer),
                            ])),
                        }),
                        Val::Boolean(false),
                    ),
                );
            }
        }
        Ok(())
    }

    fn build_properties(&mut self, parser: &Parser) -> anyhow::Result<()> {
        for predicate in parser.properties.predicates.iter() {
            let predicate = self.expression(
                predicate,
                &parser.interner,
                &self
                    .ports
                    .iter()
                    .map(|(name, (atom, _val))| {
                        (
                            name.clone(),
                            (
                                atom.clone(),
                                parser.properties.ports.get(name).unwrap().r#type.clone(),
                            ),
                        )
                    })
                    .collect(),
                &None,
                &HashMap::new(),
            )?;
            self.predicates.push(predicate);
        }
        self.guarantees = parser.properties.guarantees.clone();
        self.assumes = parser.properties.assumes.clone();
        Ok(())
    }

    fn build_model(self) -> (CsModel<SmallRng>, ScxmlModel) {
        let mut model = CsModelBuilder::new(self.cs.build());
        let mut ports = Vec::new();
        for (port_name, (atom, init)) in self.ports {
            // TODO FIXME handle error.
            if let Atom::State(channel) = atom {
                model.add_port(channel, init.clone());
                ports.push((port_name, init.r#type()));
            }
        }
        for pred_expr in self.predicates {
            // TODO FIXME handle error.
            let _id = model.add_predicate(pred_expr);
        }
        let mut guarantees = Vec::new();
        for (name, guarantee) in self.guarantees.into_iter() {
            guarantees.push(name);
            model.add_guarantee(guarantee);
        }
        let mut assumes = Vec::new();
        for (name, assume) in self.assumes.into_iter() {
            assumes.push(name.clone());
            model.add_assume(assume.clone());
        }
        let mut events = Vec::from_iter(self.event_indexes);
        events.sort_unstable_by_key(|(_, idx)| *idx);
        let events = events
            .into_iter()
            .enumerate()
            .map(|(enum_i, (name, idx))| {
                assert_eq!(enum_i, idx);
                name
            })
            .collect();

        (
            model.build(),
            ScxmlModel {
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
                events,
                fsm_indexes: self
                    .fsm_builders
                    .into_iter()
                    .map(|(name, b)| (u16::from(b.pg_id) as usize, name))
                    .collect(),
                ports,
                assumes,
                guarantees,
            },
        )
    }
}
