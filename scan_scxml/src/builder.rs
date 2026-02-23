//! Model builder for SCAN's XML specification format.

use crate::parser::{
    Executable, If, OmgBaseType, OmgType, OmgTypeDef, OmgTypes, Param, Parser, Scxml, Send, Target,
};
use anyhow::{Context, anyhow, bail};
use boa_ast::expression::access::{PropertyAccess, PropertyAccessField};
use boa_interner::{Interner, ToInternedString};
use log::{info, trace, warn};
use scan_core::{channel_system::*, *};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ops::Not,
};

// TODO:
//
// -[ ] WARN FIXME System is fragile if name/id/path do not coincide

#[derive(Debug, Clone)]
pub struct ScxmlModel {
    // u16 here represents PgId
    pub fsm_names: HashMap<u16, String>,
    // usize here represents event index
    pub parameters: HashMap<Channel, (PgId, PgId, usize)>,
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
    // Associates parameter's name with its type's name.
    name: String,
    params: BTreeMap<String, Option<OmgType>>,
    senders: HashSet<PgId>,
    receivers: HashSet<PgId>,
}

/// Builder turning a [`Parser`] into a [`ChannelSystem`].
#[derive(Default)]
pub struct ModelBuilder {
    cs: ChannelSystemBuilder,
    // Associates an enum's label with a **globally unique** index.
    // The same label can belong to multiple enums,
    // and given a label it is not possible to recover the originating enum.
    // WARN FIXME TODO: simplistic implementation of enums
    enums: HashMap<String, Integer>,
    // Associates a struct's id and field id with the index it is assigned in the struct's representation as a product.
    // NOTE: This is decided arbitrarily and not imposed by the OMG type definition.
    // QUESTION: Is there a better way?
    // structs: HashMap<(String, String), usize>,
    // Each State Chart has an associated Program Graph,
    // and an arbitrary, progressive index
    fsm_names: HashMap<u16, String>,
    fsm_builders: HashMap<String, FsmBuilder>,
    // Each event is associated to a unique global index and parameter(s).
    // WARN FIXME TODO: name clashes
    events: Vec<EventBuilder>,
    event_indexes: HashMap<String, usize>,
    parameter_channels: HashMap<(PgId, PgId, usize), Channel>,
    // Properties
    guarantees: Vec<(String, Pmtl<usize>)>,
    assumes: Vec<(String, Pmtl<usize>)>,
    predicates: Vec<Expression<Atom>>,
    ports: HashMap<String, (OmgType, Vec<(Atom, Val)>)>,
    // extra data
    int_queues: HashSet<Channel>,
}

impl ModelBuilder {
    /// Turns the [`Parser`] into a [`ChannelSystem`].
    ///
    /// Can fail if the model specification contains semantic errors
    /// (particularly type mismatches)
    /// or references to non-existing items.
    pub fn build(
        mut parser: Parser,
        properties: &[String],
        all_properties: bool,
    ) -> anyhow::Result<(CsModel, PmtlOracle, ScxmlModel)> {
        let mut model_builder = ModelBuilder::default();
        model_builder
            .build_types(&parser.types)
            .context("failed building types")?;
        model_builder
            .prebuild_processes(&mut parser)
            .context("failed prebuilding processes")?;

        info!(target: "build", "Visit process list");
        for (id, fsm) in parser.processes.iter() {
            model_builder
                .build_fsm(fsm, &mut parser.interner, &parser.types)
                .with_context(|| format!("failed building FSM '{id}'"))?;
        }

        model_builder
            .build_ports(&parser)
            .context("failed building ports")?;
        model_builder
            .build_properties(&parser, properties, all_properties)
            .context("failed building properties")?;

        let model = model_builder.build_model();

        Ok(model)
    }

    fn build_types(&mut self, omg_types: &OmgTypes) -> anyhow::Result<()> {
        info!(target: "build", "Building types");
        for (_name, omg_type) in omg_types.type_defs.iter() {
            if let OmgTypeDef::Enumeration(labels) = omg_type {
                // NOTE: enum labels are assigned a **globally unique** index,
                // and the same label can appear in different enums.
                // This makes it so that SUCCESS and FAILURE from ActionResponse are the same as those in ConditionResponse.
                for label in labels.iter() {
                    if !self.enums.contains_key(label) {
                        let idx = self.enums.len();
                        self.enums.insert(label.to_owned(), idx as Integer);
                    }
                }
            }
        }
        Ok(())
    }

    fn event_index(&mut self, id: &str) -> usize {
        self.event_indexes.get(id).cloned().unwrap_or_else(|| {
            let index = self.events.len();
            self.events.push(EventBuilder {
                name: id.to_string(),
                params: BTreeMap::new(),
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
                .new_channel(vec![Type::Integer, Type::Integer], None);
            let fsm = FsmBuilder { pg_id, ext_queue };
            self.fsm_builders.insert(id.to_string(), fsm);
            self.fsm_names.insert(pg_id.into(), id.to_string());
        }
        self.fsm_builders.get(id).expect("just inserted")
    }

    fn prebuild_processes(&mut self, parser: &mut Parser) -> anyhow::Result<()> {
        for (id, fsm) in parser.processes.iter_mut() {
            let pg_id = self.fsm_builder(id).pg_id;
            self.prebuild_fsm(pg_id, fsm, &parser.interner, &parser.types)
                .with_context(|| {
                    format!(
                        "failed pre-processing of fsm {}",
                        self.fsm_names
                            .get(&pg_id.into())
                            .unwrap_or(&String::from("unknown")),
                    )
                })?;
        }
        for eb in &self.events {
            for (param, t) in &eb.params {
                if t.is_none() {
                    bail!("param {param} of event {} needs type annotation", eb.name);
                }
            }
        }
        Ok(())
    }

    fn prebuild_fsm(
        &mut self,
        pg_id: PgId,
        fmt: &mut Scxml,
        interner: &Interner,
        omg_types: &OmgTypes,
    ) -> anyhow::Result<()> {
        let mut vars: HashMap<String, OmgType> = HashMap::new();
        for data in &fmt.datamodel {
            if let Some(r#type) = &data.omg_type
                // need to know len of arrays
                && !matches!(r#type, OmgType::Array(_, None))
            {
                vars.insert(data.id.to_owned(), r#type.clone());
            } else if let Some(expr) = data.expression.as_ref() {
                let r#type = self.infer_type(expr, &vars, interner, omg_types)?;
                vars.insert(data.id.to_owned(), r#type);
            }
        }
        for (_, state) in fmt.states.iter_mut() {
            for exec in state.on_entry.iter_mut() {
                self.prebuild_exec(pg_id, exec, &vars, interner, omg_types)
                    .with_context(|| {
                        format!(
                            "failed pre-processing of executable on entry of state {}",
                            state.id
                        )
                    })?;
            }
            for (index, transition) in state.transitions.iter_mut().enumerate() {
                if let Some(ref event) = transition.event {
                    // Event may or may not have been processed before
                    let event_index = self.event_index(event);
                    let builder = self.events.get_mut(event_index).expect("index must exist");
                    builder.receivers.insert(pg_id);
                }
                for exec in transition.effects.iter_mut() {
                    self.prebuild_exec(pg_id, exec, &vars, interner, omg_types).with_context(|| {
                        format!("failed pre-processing of executable in transition {index} of state {}", state.id)
                    })?;
                }
            }
            for exec in state.on_exit.iter_mut() {
                self.prebuild_exec(pg_id, exec, &vars, interner, omg_types)
                    .with_context(|| {
                        format!(
                            "failed pre-processing of executable on exit of state {}",
                            state.id
                        )
                    })?;
            }
        }
        Ok(())
    }

    fn prebuild_exec(
        &mut self,
        pg_id: PgId,
        executable: &mut Executable,
        vars: &HashMap<String, OmgType>,
        interner: &Interner,
        omg_types: &OmgTypes,
    ) -> anyhow::Result<()> {
        match executable {
            Executable::Assign {
                location: _,
                expr: _,
            } => Ok(()),
            Executable::Raise { event } => {
                // Treat raised events as sent and received by the FSM itself.
                // Raised events cannot have params.
                let event_index = self.event_index(event);
                let builder = self.events.get_mut(event_index).expect("index must exist");
                builder.senders.insert(pg_id);
                builder.receivers.insert(pg_id);
                Ok(())
            }
            Executable::Send(Send {
                event,
                target,
                delay: _,
                params,
            }) => {
                let event_index = self.event_index(event);
                // add FSM to event's senders
                self.events
                    .get_mut(event_index)
                    .expect("index must exist")
                    .senders
                    .insert(pg_id);
                // If target is given by Id, add it to event's receivers
                // This is not possible with dynamic targets
                if let Some(Target::Id(target)) = target {
                    let target_id = self.fsm_builder(target).pg_id;
                    self.events
                        .get_mut(event_index)
                        .expect("index must exist")
                        .receivers
                        .insert(target_id);
                }
                for param in params {
                    // Update OMG_type value so that it contains its type for sure
                    let builder = self.events.get(event_index).expect("index must exist");
                    if let Some(Some(t)) = builder.params.get(&param.name)
                        // need to know len of arrays
                        && !matches!(t, OmgType::Array(_, None))
                    {
                        if let Some(omg) = param.omg_type.as_ref() {
                            if t != omg {
                                bail!(
                                    "type parameter mismatch: {t:?} != {omg:?} for parameter {}",
                                    param.name
                                );
                            }
                        } else {
                            let _ = param.omg_type.insert(t.clone());
                        }
                    } else if let Some(t) = param.omg_type.as_ref()
                        // need to know len of arrays
                        && !matches!(t, OmgType::Array(_, None))
                    {
                        let builder = self.events.get_mut(event_index).expect("index must exist");
                        builder.params.insert(param.name.clone(), Some(t.clone()));
                    } else if let Ok(t) = self.infer_type(&param.expr, vars, interner, omg_types) {
                        let _ = param.omg_type.insert(t.clone());
                        let builder = self.events.get_mut(event_index).expect("index must exist");
                        builder.params.insert(param.name.clone(), Some(t));
                    } else {
                        // Mark with None parameters without known type
                        let builder = self.events.get_mut(event_index).expect("index must exist");
                        builder.params.insert(param.name.clone(), None);
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
                        self.prebuild_exec(pg_id, executable, vars, interner, omg_types)
                            .context("failed pre-processing executable content in <if> element")?;
                    }
                }
                for executable in r#else.iter_mut().flatten() {
                    self.prebuild_exec(pg_id, executable, vars, interner, omg_types)
                        .context("failed pre-processing executable content in <else> element")?;
                }
                Ok(())
            }
        }
    }

    fn infer_type(
        &self,
        expr: &boa_ast::Expression,
        vars: &HashMap<String, OmgType>,
        interner: &Interner,
        omg_types: &OmgTypes,
    ) -> anyhow::Result<OmgType> {
        match expr {
            boa_ast::Expression::Identifier(ident) => {
                let ident = ident.to_interned_string(interner);
                vars.get(&ident)
                    .cloned()
                    .or_else(|| {
                        if self.enums.contains_key(&ident) {
                            Some(OmgBaseType::Int32.into())
                        } else {
                            None
                        }
                    })
                    .ok_or(anyhow!("type cannot be inferred"))
            }
            boa_ast::Expression::ArrayLiteral(lit) => {
                let omg_type = self.infer_type(
                    lit.as_ref()
                        .first()
                        .ok_or(anyhow!("array literal missing expression"))?
                        .as_ref()
                        .ok_or(anyhow!("cannot infer type of this array"))?,
                    vars,
                    interner,
                    omg_types,
                )?;
                let len = lit.as_ref().len();
                if let OmgType::Base(omg_base_type) = omg_type {
                    Ok(OmgType::Array(omg_base_type, Some(len)))
                } else {
                    Err(anyhow!("only arrays of base types are currently supported"))
                }
            }
            boa_ast::Expression::Literal(lit) => {
                use boa_ast::expression::literal::LiteralKind;
                match lit.kind() {
                    LiteralKind::String(_) => Ok(OmgBaseType::String.into()),
                    LiteralKind::Num(_) => Ok(OmgBaseType::F64.into()),
                    LiteralKind::Int(_) => Ok(OmgBaseType::Int32.into()),
                    // Literal::BigInt(_) => todo!(),
                    LiteralKind::Bool(_) => Ok(OmgBaseType::Boolean.into()),
                    _ => Err(anyhow!(
                        "unable to infer type for literal expression '{lit:?}'"
                    )),
                }
            }
            boa_ast::Expression::Unary(unary) => {
                let type_name = self.infer_type(unary.target(), vars, interner, omg_types)?;
                match unary.op() {
                    boa_ast::expression::operator::unary::UnaryOp::Minus
                    | boa_ast::expression::operator::unary::UnaryOp::Plus => Ok(type_name),
                    boa_ast::expression::operator::unary::UnaryOp::Not => {
                        Ok(OmgBaseType::Boolean.into())
                    }
                    _ => Err(anyhow!(
                        "unable to infer type for operator '{:?}'",
                        unary.op()
                    )),
                }
            }
            boa_ast::Expression::Binary(bin) => {
                let lhs = self.infer_type(bin.lhs(), vars, interner, omg_types)?;
                let rhs = self.infer_type(bin.rhs(), vars, interner, omg_types)?;
                match bin.op() {
                    boa_ast::expression::operator::binary::BinaryOp::Arithmetic(op) => {
                        if let (OmgType::Base(lhs_type), OmgType::Base(rhs_type)) = (lhs, rhs) {
                            if lhs_type == rhs_type {
                                Ok(OmgType::Base(lhs_type))
                            } else {
                                Err(anyhow!(
                                    "unable to infer type for operator '{op:?}' arithmetic expression"
                                ))
                            }
                        } else {
                            Err(anyhow!(
                                "unable to infer type for operator '{op:?}' arithmetic expression"
                            ))
                        }
                    }
                    boa_ast::expression::operator::binary::BinaryOp::Bitwise(_) => {
                        Err(anyhow!("bitwise operations not supported"))
                    }
                    boa_ast::expression::operator::binary::BinaryOp::Relational(_)
                    | boa_ast::expression::operator::binary::BinaryOp::Logical(_) => {
                        Ok(OmgBaseType::Boolean.into())
                    }
                    boa_ast::expression::operator::binary::BinaryOp::Comma => Err(anyhow!(
                        "unknown binary operator 'comma', unable to infer type"
                    )),
                }
            }
            boa_ast::Expression::Call(call) => {
                // TODO FIXME: fix name of args
                let mut vars_args = vars.clone();
                let args = call
                    .args()
                    .iter()
                    .map(|arg| {
                        self.infer_type(arg, vars, interner, omg_types)
                            .map(|omg_type| (String::from("name of arg???"), omg_type))
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                vars_args.extend(args);
                self.infer_type(call.function(), &vars_args, interner, omg_types)
            }
            boa_ast::Expression::PropertyAccess(property_access) => match property_access {
                boa_ast::expression::access::PropertyAccess::Simple(simple_property_access) => {
                    if let &boa_ast::Expression::Identifier(ident) = simple_property_access.target()
                        && ident.to_interned_string(interner) == "Math"
                    {
                        match simple_property_access.field() {
                            boa_ast::expression::access::PropertyAccessField::Const(identifier) => {
                                if identifier.sym() == interner.get("floor").unwrap() {
                                    Ok(OmgBaseType::Int32.into())
                                } else if identifier.sym() == interner.get("random").unwrap() {
                                    Ok(OmgBaseType::F64.into())
                                } else {
                                    Err(anyhow!(
                                        "unknown expression '{expr:?}', unable to infer type"
                                    ))
                                }
                            }
                            boa_ast::expression::access::PropertyAccessField::Expr(expression) => {
                                Err(anyhow!(
                                    "unknown expression '{expression:?}', unable to infer type"
                                ))
                            }
                        }
                    } else {
                        match self.infer_type(
                            simple_property_access.target(),
                            vars,
                            interner,
                            omg_types,
                        )? {
                            OmgType::Base(omg_base_type) => {
                                bail!("trying to access property of base type {omg_base_type:?}")
                            }
                            OmgType::Array(omg_base_type, _) => {
                                bail!("trying to access property of array [{omg_base_type:?}]")
                            }
                            OmgType::Custom(omg_type_name) => {
                                match simple_property_access.field() {
                                    PropertyAccessField::Const(identifier) => {
                                        let field = identifier.to_interned_string(interner);
                                        match omg_types.type_defs.get(&omg_type_name).ok_or_else(
                                            || anyhow!("type {omg_type_name} undefined"),
                                        )? {
                                            OmgTypeDef::Enumeration(_items) => bail!(
                                                "trying to access property of enumeration {omg_type_name}"
                                            ),
                                            OmgTypeDef::Structure(btree_map) => {
                                                btree_map.get(&field).cloned().ok_or_else(|| anyhow!("field {field} of type {omg_type_name} undefined"))
                                            }
                                        }
                                    }
                                    PropertyAccessField::Expr(expression) => Err(anyhow!(
                                        "unknown field expression '{expression:?}', unable to infer type"
                                    )),
                                }
                            }
                        }
                    }
                }
                boa_ast::expression::access::PropertyAccess::Private(_private_property_access) => {
                    todo!()
                }
                boa_ast::expression::access::PropertyAccess::Super(_super_property_access) => {
                    todo!()
                }
            },
            _ => Err(anyhow!(
                "unknown expression '{expr:?}', unable to infer type"
            )),
        }
    }

    fn build_fsm(
        &mut self,
        scxml: &Scxml,
        interner: &mut Interner,
        omg_types: &OmgTypes,
    ) -> anyhow::Result<()> {
        trace!(target: "build", "build FSM {}", scxml.name);
        // Initialize FSM.
        let pg_builder = self
            .fsm_builders
            .get(&scxml.name)
            .unwrap_or_else(|| panic!("builder for {} must already exist", scxml.name));
        let pg_id = pg_builder.pg_id;
        let ext_queue = pg_builder.ext_queue;
        // Initialize variables from datamodel
        // NOTE vars cannot be initialized using previously defined vars because datamodel is an HashMap
        let mut vars: HashMap<String, (OmgType, Vec<(Var, Type)>)> = HashMap::new();
        for data in scxml.datamodel.iter() {
            let mut omg_type = data
                .omg_type
                .clone()
                .ok_or_else(|| anyhow!("data {} has unknown type", data.id))?;
            // Need to know len of array
            if matches!(omg_type, OmgType::Array(_, None)) {
                omg_type = data
                    .expression
                    .as_ref()
                    .ok_or_else(|| anyhow!("expression for data '{}' required", data.id))
                    .and_then(|expr| self.infer_type(expr, &HashMap::new(), interner, omg_types))?;
            }
            let vars_types = if let Some(expr) = data.expression.as_ref() {
                self.expression(expr, interner, &vars, Some(&omg_type), omg_types)?
                    .iter()
                    .map(|expr| Ok((self.cs.new_var(pg_id, expr.clone())?, expr.r#type()?)))
                    .collect::<anyhow::Result<Vec<(Var, Type)>>>()?
            } else {
                omg_type
                    .to_scan_types(omg_types).with_context(|| format!("failed converting type {omg_type:?} of location {} to Scan native types", data.id))?
                    .into_iter()
                    .map(|t| {
                        (
                            self.cs
                                .new_var(pg_id, Expression::Const(t.default_value()))
                                .expect("new var"),
                            t,
                        )
                    })
                    .collect::<Vec<(Var, Type)>>()
            };
            vars.insert(data.id.to_owned(), (omg_type.clone(), vars_types));
        }
        // Initial location of Program Graph.
        let initial_loc = self
            .cs
            .new_initial_location(pg_id)
            .expect("program graph must exist");
        // Transition initializing datamodel variables.
        // After initializing datamodel, transition to location representing point-of-entry of initial state of State Chart.
        // Map FSM's state ids to corresponding CS's locations.
        let mut states = HashMap::new();
        // Conventionally, the entry-point for a state is a location associated to the id of the state.
        states.insert(scxml.initial.to_owned(), initial_loc);
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
        let int_queue = self.cs.new_channel(vec![Type::Integer], None);
        // This we only need for backtracking.
        let _ = self.int_queues.insert(int_queue);
        let dequeue_int = self
            .cs
            .new_receive(pg_id, int_queue, vec![current_event_var])
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
            .new_receive(pg_id, ext_queue, vec![current_event_var, origin_var])
            .expect("hand-coded args");

        // Create variables and channels for the storage of the parameters sent by external events.
        // Use BTreeMap to iter in fixed order
        let mut params_vars: BTreeMap<(usize, String), (OmgType, Vec<(Var, Type)>)> =
            BTreeMap::new(); // maps (event_idx, param_name) -> (omg_type, (param_vars, var_types))
        let mut params_actions: HashMap<(PgId, usize), Action> = HashMap::new(); // maps (sender_pg_id, event) -> param_action
        for (event_index, event_builder) in self
            .events
            .iter()
            .enumerate()
            // only consider events that can activate some transition and that some other process is sending.
            .filter(|(_, eb)| eb.receivers.contains(&pg_id) && !eb.senders.is_empty())
            .map(|(index, eb)| (index, eb.clone()))
            // WARN TODO Necessary to satisfy the borrow checker but it should be possible to avoid cloning.
            .collect::<Vec<_>>()
        {
            let mut param_vars_vec = Vec::new();
            let mut param_types_vec = Vec::new();
            // sorted in alphabetical order because of BTreeMap
            for (param_name, param_type) in event_builder.params.iter() {
                let param_omg_type = param_type
                    .as_ref()
                    .ok_or_else(|| anyhow!("type of param {param_name} not found"))?;
                // Variables where to store parameter.
                let param_vars_types = param_omg_type
                    .to_scan_types(omg_types).with_context(|| format!("failed converting type {param_omg_type:?} of param {param_name} to Scan native types"))?
                    .into_iter()
                    .map(|t| {
                        (
                            self.cs
                                .new_var(pg_id, Expression::Const(t.default_value()))
                                .expect("new var"),
                            t,
                        )
                    })
                    .collect::<Vec<(Var, Type)>>();
                param_vars_vec.extend(param_vars_types.iter().map(|(v, _)| *v));
                param_types_vec.extend(param_vars_types.iter().map(|(_, t)| *t));
                let old = params_vars.insert(
                    (event_index, param_name.to_owned()),
                    (param_omg_type.clone(), param_vars_types),
                );
                assert!(old.is_none());
            }
            if !param_vars_vec.is_empty() {
                for &sender_id in event_builder.senders.iter() {
                    let params_channel = *self
                        .parameter_channels
                        .entry((sender_id, pg_id, event_index))
                        .or_insert(self.cs.new_channel(param_types_vec.clone(), None));
                    let read = self
                        .cs
                        .new_receive(pg_id, params_channel, param_vars_vec.clone())
                        .expect("must work");
                    let old = params_actions.insert((sender_id, event_index), read);
                    assert!(old.is_none());
                }
            }
        }
        // Make non-mut
        let param_vars = params_vars;
        let param_actions = params_actions;

        // This will be needed later
        let mut omg_types = omg_types.clone();

        // Consider each of the FSM's states
        for (state_id, state) in scxml.states.iter() {
            trace!(target: "build", "build state {state_id}");
            // Each state is modeled by multiple locations connected by transitions
            // A starting location is used as a point-of-entry to the execution of the state.
            let start_loc = *states
                .entry(state_id.to_owned())
                .or_insert_with(|| self.cs.new_location(pg_id).expect("program graph exists!"));
            let mut onentry_loc = start_loc;
            // Execute the state's `onentry` executable content
            for executable in state.on_entry.iter() {
                // Each executable content attaches suitable transitions to the point-of-entry location
                // and returns the target of such transitions as updated point-of-entry location.
                onentry_loc = self
                    .add_executable(
                        executable,
                        pg_id,
                        int_queue,
                        onentry_loc,
                        &vars,
                        interner,
                        &omg_types,
                    )
                    .with_context(|| {
                        format!(
                            "failed building executable content on entry of state {}",
                            state.id
                        )
                    })?;
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
            // Dequeue a new external event and search for first active named transition.
            self.cs
                .add_transition(
                    pg_id,
                    ext_queue_loc,
                    dequeue_ext,
                    ext_event_processing_param,
                    None,
                )
                .expect("hand-coded args");
            // Keep track of all known events.
            let mut known_events = Vec::new();
            // Retrieve external event's parameters
            // We need to set up the parameter-passing channel for every possible event that could be sent,
            // from any possible other FSM,
            // and for any parameter of the event.
            for (event_index, event_builder) in self
                .events
                .iter()
                .enumerate()
                // only consider events that can activate some transition and that some other process is sending.
                .filter(|(_, eb)| eb.receivers.contains(&pg_id) && !eb.senders.is_empty())
            {
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
                    if let Some(&read_params) = param_actions.get(&(sender_id, event_index)) {
                        self.cs
                            .add_transition(
                                pg_id,
                                ext_event_processing_param,
                                read_params,
                                eventful_trans,
                                Some(is_event_sender),
                            )
                            .expect("hand-coded args");
                    } else {
                        self.cs
                            .add_autonomous_transition(
                                pg_id,
                                ext_event_processing_param,
                                eventful_trans,
                                Some(is_event_sender),
                            )
                            .expect("hand-coded args");
                    }
                }
            }
            // Proceed if event is unknown (without retrieving parameters).
            let unknown_event = if known_events.is_empty() {
                None
            } else {
                Some(CsExpression::not(CsExpression::or(known_events)?)?)
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
            for (transition_index, transition) in state.transitions.iter().enumerate() {
                // Skip if event is never sent/raised
                if let Some(ref event_name) = transition.event {
                    let event_index = *self
                        .event_indexes
                        .get(event_name)
                        .expect("event must be registered");
                    if self.events[event_index].senders.is_empty() {
                        warn!(
                            "event '{event_name}' in FSM '{}' is never sent, skipping",
                            self.fsm_names.get(&pg_id.into()).expect("PG name")
                        );
                        continue;
                    }
                }
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
                if let Some(event_name) = transition.event.as_ref() {
                    let event_index = *self
                        .event_indexes
                        .get(event_name)
                        .expect("event must be registered");
                    omg_types.type_defs.extend([
                        (
                            String::from("_EventDataType"),
                            OmgTypeDef::Structure(BTreeMap::from_iter(
                                param_vars
                                    .iter()
                                    .filter(|((ev_ix, _), _)| *ev_ix == event_index)
                                    .map(|((_, param_name), (t, _))| {
                                        (param_name.to_owned(), t.clone())
                                    }),
                            )),
                        ),
                        (
                            String::from("_EventType"),
                            OmgTypeDef::Structure(BTreeMap::from_iter([
                                (String::from("origin"), OmgType::Base(OmgBaseType::Uri)),
                                (
                                    String::from("data"),
                                    OmgType::Custom(String::from("_EventDataType")),
                                ),
                            ])),
                        ),
                    ]);
                    vars.insert(
                        String::from("_event"),
                        (
                            OmgType::Custom(String::from("_EventType")),
                            Vec::from_iter(
                                param_vars
                                    .iter()
                                    .filter(|((ev_ix, _), _)| *ev_ix == event_index)
                                    .flat_map(|(_, (_, vars))| vars)
                                    .cloned()
                                    .chain([(origin_var, Type::Integer)]),
                            ),
                        ),
                    );
                }
                // Condition activating the transition.
                // It has to be parsed/built as a Boolean expression.
                // Could fail if `expr` is invalid.
                let cond: Option<Vec<CsExpression>> = transition
                    .cond
                    .as_ref()
                    .map(|cond| {
                        self.expression(
                            cond,
                            interner,
                            &vars,
                            Some(&OmgBaseType::Boolean.into()),
                            &omg_types,
                        ).with_context(|| format!("failed building conditional expression for transition #{transition_index} in state {}", state.id))
                    })
                    .transpose()?;
                if cond.as_ref().is_some_and(|cond| cond.len() != 1) {
                    bail!("condition is not a boolean expression");
                }
                let cond = cond.map(|cond| cond.first().expect("length 1").clone());

                // Location corresponding to checking if the transition is active.
                // Has to be defined depending on the type of transition.
                let check_trans_loc;
                // Location corresponding to verifying the transition is not active and moving to next one.
                let next_trans_loc = self.cs.new_location(pg_id).expect("{pg_id:?} exists");

                // Guard for transition.
                // Has to be defined depending on the type of transition, etc...
                let guard;
                // Proceed depending on whether the transition is eventless or activated by event.
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
                        .transpose()?
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
                for exec in state.on_exit.iter() {
                    exec_trans_loc = self
                        .add_executable(
                            exec,
                            pg_id,
                            int_queue,
                            exec_trans_loc,
                            &vars,
                            interner,
                            &omg_types,
                        )
                        .with_context(|| {
                            format!(
                                "failed building executable content on exit of state {}",
                                state.id
                            )
                        })?;
                }
                for exec in transition.effects.iter() {
                    exec_trans_loc = self
                        .add_executable(
                            exec,
                            pg_id,
                            int_queue,
                            exec_trans_loc,
                            &vars,
                            interner,
                            &omg_types,
                        )
                        .with_context(|| {
                            format!(
                                "failed building executable content of transition #{transition_index} of state {}",
                                state.id
                            )
                        })?;
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
                    .transpose()?
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
        vars: &HashMap<String, (OmgType, Vec<(Var, Type)>)>,
        interner: &Interner,
        omg_types: &OmgTypes,
    ) -> Result<Location, anyhow::Error> {
        match executable {
            Executable::Raise { event } => {
                // Create event, if it does not exist already.
                let event_idx = self.event_index(event);
                let raise = self.cs.new_send(
                    pg_id,
                    int_queue,
                    vec![CsExpression::from(event_idx as Integer)],
                )?;
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
                // params have to be ordered by name!
                let mut send_params = send_params.clone();
                send_params.sort_unstable_by_key(|p| p.name.clone());

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
                            let target_exprs = self.expression(
                                targetexpr,
                                interner,
                                vars,
                                Some(&OmgBaseType::Uri.into()),
                                omg_types,
                            ).with_context(|| format!("failed building target expression of <send event=\"{event}\"> element"))?;
                            if target_exprs.len() != 1 {
                                bail!("epression is not a target");
                            }
                            target_expr = target_exprs.first().cloned();
                            targets = self.events[event_idx].receivers.iter().cloned().collect();
                        }
                    }
                    for target_id in targets {
                        let target_name = self.fsm_names.get(&target_id.into()).unwrap();
                        let target_builder =
                            self.fsm_builders.get(target_name).expect("it must exist");
                        let target_ext_queue = target_builder.ext_queue;
                        let send_event = self
                            .cs
                            .new_send(
                                pg_id,
                                target_ext_queue,
                                vec![
                                    CsExpression::from(event_idx as Integer),
                                    CsExpression::from(u16::from(pg_id) as Integer),
                                ],
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
                        if !send_params.is_empty() {
                            // Updates next location.
                            next_loc = self
                                .send_params(
                                    pg_id,
                                    target_id,
                                    &send_params,
                                    event_idx,
                                    next_loc,
                                    vars,
                                    interner,
                                    omg_types,
                                )
                                .with_context(|| {
                                    format!("failed sending params for event '{event}'")
                                })?;
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
                    // An event sent without specifying the target is sent to all FSMs that can process it
                    let targets = self.events[event_idx]
                        .receivers
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>();
                    let mut next_loc = loc;
                    for target in targets {
                        let target_name = self.fsm_names.get(&target.into()).cloned();
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
                            interner,
                            omg_types,
                        )?;
                    }
                    Ok(next_loc)
                }
            }
            Executable::Assign { location, expr } => {
                // Add a transition that perform the assignment via the effect of the `assign` action.
                let (omg_type, scan_vars) =
                    vars.get(location).ok_or(anyhow!("undefined variable"))?;
                let expr = self
                    .expression(expr, interner, vars, Some(omg_type), omg_types)
                    .with_context(|| {
                        format!(
                            "failed building expression in <assign> element for location {location}"
                        )
                    })?;
                let assign = self.cs.new_action(pg_id).expect("PG exists");
                scan_vars
                    .iter()
                    .zip(expr)
                    .try_for_each(|((var, scan_type), expr)| {
                        if expr.r#type().map_err(CsError::Type)? == *scan_type {
                            self.cs.add_effect(pg_id, assign, *var, expr)
                        } else {
                            Err(CsError::Type(TypeError::TypeMismatch))
                        }
                    })
                    .with_context(|| {
                        format!("failed building assignments for location '{location}'")
                    })?;
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
                    let cond = self
                        .expression(
                            cond,
                            interner,
                            vars,
                            Some(&OmgBaseType::Boolean.into()),
                            omg_types,
                        )
                        .context("failed building condition expression in <if> element")?;
                    if cond.len() != 1 {
                        bail!("<cond> is not a boolean expression");
                    }
                    let cond = cond.first().expect("len equals 1").clone();
                    self.cs.add_autonomous_transition(
                        pg_id,
                        curr_loc,
                        next_loc,
                        Some(cond.to_owned()),
                    )?;
                    for exec in execs {
                        next_loc = self
                            .add_executable(
                                exec, pg_id, int_queue, next_loc, vars, interner, omg_types,
                            )
                            .context("failed building executable content in <if> element")?;
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
                            Some(Expression::not(cond)?),
                        )
                        .unwrap();
                }
                // Add executables for `else` (if any)
                for executable in r#else.iter().flatten() {
                    curr_loc = self
                        .add_executable(
                            executable, pg_id, int_queue, curr_loc, vars, interner, omg_types,
                        )
                        .context("failed building executable content in <else> element")?;
                }
                self.cs
                    .add_autonomous_transition(pg_id, curr_loc, end_loc, None)?;
                Ok(end_loc)
            }
        }
    }

    // WARN: vars and params have the same type so they could be easily swapped by mistake when calling the function.
    fn send_params(
        &mut self,
        pg_id: PgId,
        target_id: PgId,
        params: &Vec<Param>,
        event_idx: usize,
        param_loc: Location,
        vars: &HashMap<String, (OmgType, Vec<(Var, Type)>)>,
        interner: &Interner,
        omg_types: &OmgTypes,
    ) -> Result<Location, anyhow::Error> {
        assert!(!params.is_empty());
        // Check that no param is missing
        for p in self.events[event_idx].params.keys() {
            if !params.iter().any(|param| param.name == *p) {
                bail!("missing param {p}");
            }
        }
        let mut exprs = Vec::new();
        for param in params {
            // Build expression from ECMAScript expression.
            let expr = self.expression(
                &param.expr,
                interner,
                vars,
                param.omg_type.as_ref(),
                omg_types,
            )?;
            exprs.extend_from_slice(&expr);
        }
        // Retreive or create channel for parameter passing.
        let scan_types = exprs
            .iter()
            .map(|expr| expr.r#type())
            .collect::<Result<Vec<_>, _>>()?;
        let param_chn = *self
            .parameter_channels
            .entry((pg_id, target_id, event_idx))
            .or_insert(self.cs.new_channel(scan_types, None));
        // Can return error if expr is badly typed
        let pass_param = self.cs.new_send(pg_id, param_chn, exprs)?;
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
        vars: &HashMap<String, (OmgType, Vec<(V, Type)>)>,
        expr_type: Option<&OmgType>,
        omg_types: &OmgTypes,
    ) -> anyhow::Result<Vec<Expression<V>>> {
        let expr = match expr {
            boa_ast::Expression::This(_this) => todo!(),
            boa_ast::Expression::Identifier(ident) => {
                let ident = ident.to_interned_string(interner);
                self.enums
                    .get(&ident)
                    .map(|i| vec![Expression::from(*i)])
                    .or_else(|| {
                        vars.get(&ident).map(|(_, vars)| {
                            vars.iter()
                                .map(|(var, t)| Expression::Var(var.clone(), t.to_owned()))
                                .collect::<Vec<Expression<V>>>()
                            // .ok_or(anyhow!("missing type {t}"))
                        })
                    })
                    .ok_or(anyhow!("unknown identifier: {ident}"))?
            }
            boa_ast::Expression::Literal(lit) => {
                use boa_ast::expression::literal::LiteralKind;
                vec![match lit.kind() {
                    LiteralKind::String(s) => {
                        let len = self.enums.len() as Integer;
                        Expression::from(
                            *self
                                .enums
                                .entry(interner.resolve_expect(*s).to_string())
                                .or_insert(len),
                        )
                    }
                    LiteralKind::Num(f) => Expression::from(*f),
                    LiteralKind::Int(i)
                        if expr_type
                            .is_some_and(|t| matches!(t, OmgType::Base(OmgBaseType::F64))) =>
                    {
                        Expression::from(*i as f64)
                    }
                    LiteralKind::Int(i) => Expression::from(*i),
                    LiteralKind::BigInt(_) => todo!(),
                    LiteralKind::Bool(b) => Expression::from(*b),
                    LiteralKind::Null => todo!(),
                    LiteralKind::Undefined => todo!(),
                }]
            }
            boa_ast::Expression::ArrayLiteral(arr) => {
                let expr_type = expr_type.ok_or(anyhow!("unknown array type"))?;
                if let OmgType::Array(omg_base_type, len) = expr_type {
                    let default = Expression::Const(Type::from(*omg_base_type).default_value());
                    if len.is_some_and(|len| len != arr.as_ref().len()) {
                        bail!("array literal of length incompatible with declared type len");
                    }
                    arr.as_ref()
                        .iter()
                        .map(|entry| {
                            entry.as_ref().map_or(Ok(vec![default.clone()]), |entry| {
                                self.expression(
                                    entry,
                                    interner,
                                    vars,
                                    Some((*omg_base_type).into()).as_ref(),
                                    omg_types,
                                )
                            })
                        })
                        .collect::<anyhow::Result<Vec<_>>>()?
                        .into_iter()
                        .flatten()
                        .collect()
                } else {
                    bail!("expression not matching its type {expr_type:?}");
                }
            }
            boa_ast::Expression::PropertyAccess(prop_acc) => match prop_acc {
                PropertyAccess::Simple(simple_property_access) => {
                    let target = simple_property_access.target();
                    let target_type = self.infer_type(
                        target,
                        &vars
                            .iter()
                            .map(|(var, (omg_type, _))| (var.clone(), omg_type.clone()))
                            .collect(),
                        interner,
                        omg_types,
                    )?;
                    let target =
                        self.expression(target, interner, vars, Some(&target_type), omg_types)?;
                    let target_type_def = match target_type {
                        OmgType::Base(omg_base_type) => {
                            bail!("property access on base type {omg_base_type:?}")
                        }
                        OmgType::Array(omg_base_type, _) => {
                            bail!("property access on array [{omg_base_type:?}]")
                        }
                        OmgType::Custom(omg_name) => omg_types
                            .type_defs
                            .get(&omg_name)
                            .ok_or(anyhow!("type '{omg_name}' undefined"))?,
                    };
                    match simple_property_access.field() {
                        PropertyAccessField::Const(identifier) => {
                            let field_name = identifier.to_interned_string(interner);
                            match target_type_def {
                                OmgTypeDef::Enumeration(_items) => {
                                    bail!("property access on enumeration")
                                }
                                OmgTypeDef::Structure(fields) => {
                                    let mut target = target.as_slice();
                                    for (next_field_name, next_field_type) in fields {
                                        let field_size = next_field_type.size(omg_types).with_context(|| {
                                            format!(
                                                "failed computing field '{next_field_name}' type '{next_field_type:?}' size"
                                            )
                                        })?;
                                        if *next_field_name == field_name {
                                            return Ok(target[..field_size].to_vec());
                                        } else {
                                            // Skip field-size-many expressions
                                            target = &target[field_size..];
                                        }
                                    }
                                    bail!("unknown field {field_name}");
                                }
                            }
                        }
                        PropertyAccessField::Expr(_expression) => todo!(),
                    }
                }
                PropertyAccess::Private(_private_property_access) => todo!(),
                PropertyAccess::Super(_super_property_access) => todo!(),
            },
            boa_ast::Expression::Unary(unary) => {
                use boa_ast::expression::operator::unary::UnaryOp;
                let expr = self.expression(unary.target(), interner, vars, expr_type, omg_types)?;
                if expr.len() != 1 {
                    bail!("expression does not support unary operator");
                }
                let expr = expr[0].clone();
                let new_expr = match unary.op() {
                    UnaryOp::Minus => -expr,
                    UnaryOp::Plus => expr,
                    UnaryOp::Not => Expression::not(expr)?,
                    _ => return Err(anyhow!("unimplemented operator")),
                };
                vec![new_expr]
            }
            boa_ast::Expression::Binary(bin) => {
                use boa_ast::expression::operator::binary::{
                    ArithmeticOp, BinaryOp, LogicalOp, RelationalOp,
                };
                match bin.op() {
                    BinaryOp::Arithmetic(ar_bin) => {
                        let lhs_hint;
                        let rhs_hint;
                        match ar_bin {
                            ArithmeticOp::Add
                            | ArithmeticOp::Sub
                            | ArithmeticOp::Mul
                            | ArithmeticOp::Exp => {
                                lhs_hint = expr_type;
                                rhs_hint = expr_type;
                            }
                            ArithmeticOp::Div => {
                                // WARN: Type inference is tricky: integer division could produce a float
                                lhs_hint = None;
                                rhs_hint = None;
                            }
                            ArithmeticOp::Mod => {
                                lhs_hint = Some(&OmgType::Base(OmgBaseType::Int32));
                                rhs_hint = Some(&OmgType::Base(OmgBaseType::Int32));
                            }
                        }
                        let lhs =
                            self.expression(bin.lhs(), interner, vars, lhs_hint, omg_types)?;
                        if lhs.len() != 1 {
                            bail!("expression lhs does not support arithmetic binary operator");
                        }
                        let lhs = lhs[0].clone();
                        let rhs =
                            self.expression(bin.rhs(), interner, vars, rhs_hint, omg_types)?;
                        if rhs.len() != 1 {
                            bail!("expression rhs does not support arithmetic binary operator");
                        }
                        let rhs = rhs[0].clone();
                        let new_expr = match ar_bin {
                            ArithmeticOp::Add => lhs + rhs,
                            ArithmeticOp::Sub => lhs + (-rhs),
                            ArithmeticOp::Div => Expression::Div(Box::new((lhs, rhs))),
                            ArithmeticOp::Mul => lhs * rhs,
                            ArithmeticOp::Exp => todo!(),
                            ArithmeticOp::Mod => Expression::Mod(Box::new((lhs, rhs))),
                        };
                        vec![new_expr]
                    }
                    BinaryOp::Relational(rel_bin) => {
                        // Type inference is not possible as multiple types are possible
                        let lhs = self.expression(bin.lhs(), interner, vars, None, omg_types)?;
                        if lhs.len() != 1 {
                            bail!("expression lhs does not support binary relational operator");
                        }
                        let lhs = lhs[0].clone();
                        let rhs = self.expression(bin.rhs(), interner, vars, None, omg_types)?;
                        if rhs.len() != 1 {
                            bail!("expression rhs does not support binary relational operator");
                        }
                        let rhs = rhs[0].clone();
                        let new_expr = match rel_bin {
                            RelationalOp::Equal => Expression::Equal(Box::new((lhs, rhs))),
                            RelationalOp::NotEqual => {
                                Expression::Equal(Box::new((lhs, rhs))).not()?
                            }
                            RelationalOp::GreaterThan => Expression::Greater(Box::new((lhs, rhs))),
                            RelationalOp::GreaterThanOrEqual => {
                                Expression::GreaterEq(Box::new((lhs, rhs)))
                            }
                            RelationalOp::LessThan => Expression::Less(Box::new((lhs, rhs))),
                            RelationalOp::LessThanOrEqual => {
                                Expression::LessEq(Box::new((lhs, rhs)))
                            }
                            _ => return Err(anyhow!("unimplemented operator")),
                        };
                        vec![new_expr]
                    }
                    BinaryOp::Logical(op) => {
                        let lhs = self.expression(
                            bin.lhs(),
                            interner,
                            vars,
                            Some(&OmgType::Base(OmgBaseType::Boolean)),
                            omg_types,
                        )?;
                        if lhs.len() != 1 {
                            bail!("expression lhs does not support binary logical operator");
                        }
                        let lhs = lhs[0].clone();
                        let rhs = self.expression(
                            bin.rhs(),
                            interner,
                            vars,
                            Some(&OmgType::Base(OmgBaseType::Boolean)),
                            omg_types,
                        )?;
                        if rhs.len() != 1 {
                            bail!("expression rhs does not support binary logical operator");
                        }
                        let rhs = rhs[0].clone();
                        let new_expr = match op {
                            LogicalOp::And => Expression::and(vec![lhs, rhs])?,
                            LogicalOp::Or => Expression::or(vec![lhs, rhs])?,
                            _ => return Err(anyhow!("unimplemented operator")),
                        };
                        vec![new_expr]
                    }
                    BinaryOp::Comma => todo!(),
                    _ => return Err(anyhow!("unimplemented operator")),
                }
            }
            boa_ast::Expression::Conditional(_) => todo!(),
            boa_ast::Expression::Parenthesized(par) => {
                self.expression(par.expression(), interner, vars, expr_type, omg_types)?
            }
            boa_ast::Expression::Call(call) => {
                let fun = call.function();
                let args = call.args();
                if let boa_ast::Expression::PropertyAccess(
                    boa_ast::expression::access::PropertyAccess::Simple(property_access),
                ) = fun
                {
                    if let boa_ast::Expression::Identifier(target_id) = property_access.target() {
                        let target = target_id.to_interned_string(interner);
                        match property_access.field() {
                            boa_ast::expression::access::PropertyAccessField::Const(field_id) => {
                                let field = field_id.to_interned_string(interner);
                                if target == "Math" {
                                    match field.as_str() {
                                        "random" => vec![Expression::RandFloat(0., 1.)],
                                        "floor" => {
                                            if let [arg] = args {
                                                let arg = self.expression(
                                                    arg,
                                                    interner,
                                                    vars,
                                                    Some(&OmgType::Base(OmgBaseType::F64)),
                                                    omg_types,
                                                )?;
                                                if arg.len() != 1 {
                                                    bail!(
                                                        "expression does not support floor operator"
                                                    );
                                                }
                                                let arg = arg[0].clone();
                                                vec![Expression::Floor(Box::new(arg))]
                                            } else {
                                                bail!(
                                                    "Math.floor() called with wrong number of arguments"
                                                );
                                            }
                                        }
                                        _ => bail!("unknown call"),
                                    }
                                } else {
                                    bail!("unknown call");
                                }
                            }
                            boa_ast::expression::access::PropertyAccessField::Expr(expression) => {
                                return Err(anyhow!("unimplemented expression {expression:?}"));
                            }
                        }
                    } else {
                        return Err(anyhow!("unknown target"));
                    }
                } else {
                    return Err(anyhow!("unknown call"));
                }
            }
            _ => return Err(anyhow!("unimplemented expression")),
        };
        Ok(expr)
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
            let event_builder = &self.events[event_id];
            if let Some((param, init)) = &port.param {
                let param_start_idx = event_builder
                    .params
                    .range(..param.clone())
                    .map(|(_, omg_type)| {
                        omg_type
                            .as_ref()
                            .ok_or_else(|| {
                                anyhow!("type of param {param} of event {} not found", port.event)
                            })
                            .and_then(|omg_type| omg_type.size(&parser.types))
                    })
                    .sum::<anyhow::Result<usize>>()?;
                let param_type = event_builder
                    .params
                    .get(param)
                    .ok_or_else(|| {
                        anyhow!(
                            "param {param} in event {} in port {port_id} not found",
                            port.event
                        )
                    })?
                    .clone();
                let init = self
                    .expression::<Var>(
                        init,
                        &parser.interner,
                        &HashMap::new(),
                        param_type.as_ref(),
                        &parser.types,
                    )
                    .with_context(|| {
                        format!("failed building default value expression for port {port_id}")
                    })?
                    .iter()
                    .map(|expr| expr.eval_constant())
                    .collect::<Result<Vec<_>, _>>()
                    .with_context(|| {
                        format!("failed evaluating default value for port {port_id}")
                    })?;
                let channel = self
                    .parameter_channels
                    .get(&(origin, target, event_id))
                    .ok_or(anyhow!("parameters' channel for {} not found", port.event))?;
                self.ports.insert(
                    port_id.to_owned(),
                    (
                        param_type.ok_or_else(|| {
                            anyhow!("type of param {param} of event {} not found", port.event)
                        })?,
                        init.iter()
                            .enumerate()
                            .map(|(param_idx, &init)| {
                                (Atom::State(*channel, param_start_idx + param_idx), init)
                            })
                            .collect(),
                    ),
                );
            } else {
                let channel = target_builder.ext_queue;
                self.ports.insert(
                    port_id.to_owned(),
                    (
                        OmgType::Base(OmgBaseType::Boolean),
                        vec![(
                            Atom::Event(Event {
                                pg_id: origin,
                                channel,
                                event_type: EventType::Send(
                                    vec![
                                        Val::Integer(event_id as Integer),
                                        Val::Integer(u16::from(origin) as Integer),
                                    ]
                                    .into(),
                                ),
                            }),
                            Val::Boolean(false),
                        )],
                    ),
                );
            }
        }
        Ok(())
    }

    fn build_properties(
        &mut self,
        parser: &Parser,
        properties: &[String],
        all_properties: bool,
    ) -> anyhow::Result<()> {
        for predicate in parser.properties.predicates.iter() {
            let predicate = self.expression(
                predicate,
                &parser.interner,
                &self
                    .ports
                    .iter()
                    .map(|(name, (omg_type, atoms))| {
                        (
                            name.clone(),
                            (
                                omg_type.clone(),
                                atoms
                                    .iter()
                                    .map(|(atom, val)| (atom.clone(), val.r#type()))
                                    .collect(),
                            ),
                        )
                    })
                    .collect(),
                Some(&OmgType::Base(OmgBaseType::Boolean)),
                &parser.types,
            )?;
            if predicate.len() != 1 {
                bail!("predicate must be a boolean expression");
            }
            let predicate = predicate[0].clone();
            self.predicates.push(predicate);
        }
        self.guarantees = parser
            .properties
            .guarantees
            .iter()
            .filter(|(name, _)| all_properties || properties.contains(name))
            .cloned()
            .collect();
        self.assumes = parser.properties.assumes.clone();
        Ok(())
    }

    fn build_model(self) -> (CsModel, PmtlOracle, ScxmlModel) {
        let mut model = CsModel::new(self.cs);
        let mut ports = Vec::new();
        for (port_name, (_omg_type, atoms)) in self.ports {
            // TODO FIXME handle error.
            for (atom, init) in atoms {
                if let Atom::State(channel, param_idx) = atom {
                    model.add_port(channel, param_idx, init);
                    ports.push((
                        channel,
                        param_idx,
                        port_name.clone() + &param_idx.to_string(),
                        init.r#type(),
                    ));
                }
            }
        }
        // Ports need to be sorted by channel or will not match state iterator
        ports.sort_by_key(|(c, param_idx, _, _)| (*c, *param_idx));
        let ports = ports.into_iter().map(|(_, _, n, t)| (n, t)).collect();
        for pred_expr in self.predicates {
            // TODO FIXME handle error.
            let _id = model.add_predicate(pred_expr);
        }
        let (guarantee_names, guarantees): (Vec<_>, Vec<_>) = self.guarantees.into_iter().unzip();
        let (assume_names, assumes): (Vec<_>, Vec<_>) = self.assumes.into_iter().unzip();
        let oracle = PmtlOracle::new(assumes.as_slice(), guarantees.as_slice());
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
            model,
            oracle,
            ScxmlModel {
                fsm_names: self.fsm_names,
                parameters: self
                    .parameter_channels
                    .into_iter()
                    .map(|((src, trg, event), chn)| (chn, (src, trg, event)))
                    .collect(),
                ext_queues: self
                    .fsm_builders
                    .values()
                    .map(|b| (b.ext_queue, b.pg_id))
                    .collect(),
                int_queues: self.int_queues,
                events,
                ports,
                assumes: assume_names,
                guarantees: guarantee_names,
            },
        )
    }
}
