use super::Model;
use crate::parser::{
    self, Automaton, BoolOp, ConstantDeclaration, Destination, Edge, Expression,
    PropertyExpression, Sync, VariableDeclaration,
};
use anyhow::{Context, anyhow, bail};
use either::Either;
use log::warn;
use scan_core::{
    Atom, BooleanExpr, Float, FloatExpr, Integer, IntegerExpr, Natural, NaturalExpr,
    TransitionSystem, Type, TypeError, Val,
    channel_system::{Action, Channel, ChannelSystemBuilder, CsExpression, Location, PgId, Var},
};
use scan_mtl::{Mtl, MtlOracle};
use serde::de::IgnoredAny;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct JaniModelData {
    pub actions: HashMap<Action, String>,
    pub ports: Vec<(String, Type)>,
    pub guarantees: Vec<String>,
}

pub(crate) fn build(
    jani_model: Model,
    properties: &[String],
) -> anyhow::Result<(TransitionSystem, MtlOracle, JaniModelData)> {
    let builder = JaniBuilder::default();
    builder.build(jani_model, properties)
}

#[derive(Default, Debug, Clone)]
struct JaniBuilder {
    system_actions: HashMap<String, Action>,
    // Associating name to variable, initial value and type.
    global_state_channel: Option<Channel>,
    global_vars: HashMap<String, (Var, Val, Type)>,
    global_state_vec: Vec<String>,
    global_constants: HashMap<String, Val>,
    automaton_builders: Vec<AutomatonBuilder>,
}

#[derive(Debug, Clone)]
struct AutomatonBuilder {
    // tracks locations and their "idle" side-location
    locations: HashMap<String, (Location, Natural)>,
    local_vars: HashMap<String, (Var, Val, Type)>,
    idle_locations: HashMap<Action, Location>,
    return_location: Location,
    // assign action name -> cs base action + destination actions
    dest_actions: HashMap<
        // triggering sync action
        Action,
        Vec<(
            // dest action
            Action,
            // dest location
            Location,
        )>,
    >,
    rng: Var,
    current_loc: Var,
}

impl AutomatonBuilder {
    fn new(rng: Var, current_loc: Var, return_location: Location) -> Self {
        AutomatonBuilder {
            locations: HashMap::new(),
            local_vars: HashMap::new(),
            dest_actions: HashMap::new(),
            rng,
            current_loc,
            idle_locations: HashMap::new(),
            return_location,
        }
    }
}

// TRANSITIONS DECOMPOSITION
//
// [pre_location]
//     | edge guard | sync_action: unset transient vars
// [edge_location] <> previous automata dest_actions
//     | dest probability guard | dest_action: set current_location + dest assignments + set transient_vars
// [dest_location] <> successive automata dest_actions
//     | system_action: send global vars
// [post_location]

impl JaniBuilder {
    const SILENT: &str = "__SILENT__";
    const INITIAL: &str = "__INITIAL__";

    pub(crate) fn build(
        mut self,
        jani_model: Model,
        properties: &[String],
    ) -> anyhow::Result<(TransitionSystem, MtlOracle, JaniModelData)> {
        let mut cs = ChannelSystemBuilder::new();
        let pg_id = cs.new_program_graph();
        let automata: HashMap<&str, &Automaton> = jani_model
            .automata
            .iter()
            .map(|automaton| (automaton.name.as_str(), automaton))
            .collect();

        jani_model
            .constants
            .iter()
            .try_for_each(|c| self.add_global_constant(c))?;

        let global_state_len = jani_model.variables.len();
        let mut global_state_init = Vec::with_capacity(global_state_len);
        let mut global_state_type = Vec::with_capacity(global_state_len);
        let mut global_state_expr = Vec::with_capacity(global_state_len);

        // Create global variables
        for variable_dec in jani_model.variables.iter() {
            let var_type = (&variable_dec.r#type).try_into().expect("convert type");
            let init = variable_dec
                .initial_value
                .as_ref()
                .map(|expr| self.build_expression(expr, Some(var_type), &self.global_vars))
                .transpose()?
                .unwrap_or_else(|| CsExpression::from(var_type.default_value()));
            let val = init.eval_constant()?;
            if var_type != val.r#type() {
                bail!(
                    "system variable {} initialization value wrong type",
                    variable_dec.name
                );
            }

            let var = cs.new_var(pg_id, val)?;
            self.global_vars
                .insert(variable_dec.name.clone(), (var, val, var_type));
            self.global_state_vec.push(variable_dec.name.clone());
            global_state_type.push(var_type);
            global_state_init.push(val);
            global_state_expr.push(CsExpression::from_var(var, var_type));
        }

        // Create channel to send global state expression to
        let global_state_channel = cs.new_sink(global_state_type);
        self.global_state_channel = Some(global_state_channel);

        // for every system action (including silent ones),
        // create action which sends global state to channel
        for system_action in &jani_model.actions {
            let system_action_id =
                cs.new_send(pg_id, global_state_channel, global_state_expr.clone())?;
            self.system_actions
                .insert(system_action.name.clone(), system_action_id);
        }
        let silent_system_action =
            cs.new_send(pg_id, global_state_channel, global_state_expr.clone())?;
        // Add initialization action
        let system_initial_action_id =
            cs.new_send(pg_id, global_state_channel, global_state_expr)?;
        self.system_actions
            .insert(Self::INITIAL.to_string(), system_initial_action_id);

        for element in jani_model.system.elements.iter() {
            let rng = cs.new_var(pg_id, Val::from(0.)).expect("new var");
            let current_loc = cs.new_var(pg_id, Val::from(0 as Natural)).expect("new var");
            let return_location = cs.new_location(pg_id).expect("new location");

            let mut automaton_builder = AutomatonBuilder::new(rng, current_loc, return_location);
            let automaton = automata
                .get(element.automaton.as_str())
                .ok_or_else(|| anyhow!("missing automaton {}", element.automaton))?;

            // initial locations
            let initial = cs.new_initial_location(pg_id).expect("initial location");
            assert!(automaton_builder.locations.is_empty());
            automaton_builder
                .locations
                .insert(Self::INITIAL.to_string(), (initial, 0));

            // create locations
            for location in &automaton.locations {
                let loc = cs.new_location(pg_id).expect("new location");
                // Give every location an ID unique within the automaton
                // (not necessarily globally unique)
                let loc_id = automaton_builder.locations.len() as Natural;
                automaton_builder
                    .locations
                    .insert(location.name.clone(), (loc, loc_id));
                let dest_guard = BooleanExpr::NatEqual(
                    scan_core::NaturalExpr::from(loc_id),
                    scan_core::NaturalExpr::Var(current_loc),
                );
                cs.add_autonomous_transition(pg_id, return_location, loc, Some(dest_guard))
                    .expect("add transition");
            }

            // Add local variables
            automaton
                .variables
                .iter()
                .try_for_each(|var| {
                    self.add_local_var(&mut cs, pg_id, var, &mut automaton_builder.local_vars)
                })
                .context("failed adding local variables")?;

            self.automaton_builders.push(automaton_builder);
        }

        // Extend system syncs with async silent actions
        // for each element that has an edge activated by silent action
        let num_elements = jani_model.system.elements.len();
        assert_eq!(self.automaton_builders.len(), num_elements);
        let silent_syncs = (0..num_elements)
            .filter(|&n| {
                let element = jani_model.system.elements.get(n).unwrap();
                let automaton = automata.get(element.automaton.as_str()).expect("automaton");
                automaton.edges.iter().any(|edge| edge.action.is_none())
            })
            .map(|n| {
                let mut synchronise = vec![None; num_elements];
                synchronise[n] = Some(Self::SILENT.to_string());
                parser::Sync {
                    synchronise,
                    result: None,
                    comment: IgnoredAny,
                }
            });
        // Sync calling initial action on all elements
        let initial_sync = Sync {
            synchronise: vec![Some(Self::INITIAL.to_string()); jani_model.system.elements.len()],
            result: Some(Self::INITIAL.to_string()),
            comment: IgnoredAny,
        };

        // for every sync, create sync action
        for sync in silent_syncs
            .chain(jani_model.system.syncs)
            .chain(std::iter::once(initial_sync))
        {
            let sync_action = cs.new_action(pg_id).expect("new action");
            let system_action = sync
                .result
                .as_ref()
                .map(|result| {
                    self.system_actions
                        .get(result)
                        .ok_or_else(|| anyhow!("sync result {result} unknown"))
                })
                .transpose()?
                .copied()
                .unwrap_or(silent_system_action);

            // elements unaffected by sync must ignore the sync action in every location
            // by moving to idle location
            let mut any_unaffected = false;
            for (element_idx, _) in sync
                .synchronise
                .iter()
                .enumerate()
                .filter(|(_, action)| action.is_none())
            {
                any_unaffected = true;
                let automaton_builder = self
                    .automaton_builders
                    .get_mut(element_idx)
                    .expect("automaton builder");

                // Create idle location for this sync/automaton
                let sync_idle_location = cs.new_location(pg_id).expect("new location");
                automaton_builder
                    .idle_locations
                    .insert(sync_action, sync_idle_location);
                cs.add_transition(
                    pg_id,
                    sync_idle_location,
                    system_action,
                    automaton_builder.return_location,
                    None,
                )
                .expect("add transition");

                for (location, loc_id) in automaton_builder.locations.values() {
                    // Skip initial location (not needed)
                    if *loc_id == 0 {
                        continue;
                    }
                    cs.add_transition(pg_id, *location, sync_action, sync_idle_location, None)
                        .expect("add transition");
                }
            }

            // For every element involved in the sync action,
            // build transitions associated to relevant edges
            for (element_idx, element, automaton_action) in jani_model
                .system
                .elements
                .iter()
                .zip(&sync.synchronise)
                .enumerate()
                .filter_map(|(element_idx, (element, action))| {
                    action.as_ref().map(|action| (element_idx, element, action))
                })
            {
                let automaton = automata
                    .get(element.automaton.as_str())
                    .ok_or_else(|| anyhow!("missing automaton {}", element.automaton))?;

                let automaton_builder = self
                    .automaton_builders
                    .get(element_idx)
                    .expect("automaton builder");

                // Sync actions randomize automaton's RNG
                cs.add_effect(
                    pg_id,
                    sync_action,
                    automaton_builder.rng,
                    CsExpression::Float(FloatExpr::Rand(Box::new((
                        FloatExpr::from(0.),
                        FloatExpr::from(1.),
                    )))),
                )
                .expect("add effect");

                let initial_edge = Edge {
                    location: Self::INITIAL.to_string(),
                    action: Some(Self::INITIAL.to_string()),
                    guard: None,
                    destinations: automaton
                        .initial_locations
                        .iter()
                        .map(|location| Destination {
                            location: location.clone(),
                            probability: None,
                            assignments: Vec::new(),
                            comment: IgnoredAny,
                        })
                        .collect(),
                    comment: IgnoredAny,
                };

                let initial_location = parser::Location {
                    name: Self::INITIAL.to_string(),
                    transient_values: Vec::new(),
                    comment: IgnoredAny,
                };

                // create edges for the automaton action corresponding to the sync action
                for edge in automaton
                    .edges
                    .iter()
                    .chain(std::iter::once(&initial_edge))
                    .filter(|edge| {
                        edge.action.as_deref().unwrap_or(Self::SILENT) == automaton_action
                    })
                {
                    let automaton_builder = self
                        .automaton_builders
                        .get(element_idx)
                        .expect("automaton builder");
                    let (pre_loc, _) = *automaton_builder
                        .locations
                        .get(&edge.location)
                        .ok_or_else(|| {
                            anyhow!(
                                "missing location {} in automaton {}",
                                edge.location,
                                element.automaton
                            )
                        })?;

                    // Sync actions initiate a transition so they need to reset transient variables
                    // WARN: We assume that no other automaton is setting the same transient global variables,
                    // so that resetting all of them at the level of sync action is consistent with expected behavior.
                    for transient in &automaton
                        .locations
                        .iter()
                        .chain(std::iter::once(&initial_location))
                        .find(|loc| loc.name == edge.location)
                        .ok_or_else(|| anyhow!("edge location {} not found", edge.location))?
                        .transient_values
                    {
                        let (var, init, _) = automaton_builder
                            .local_vars
                            .get(&transient.r#ref)
                            .or_else(|| self.global_vars.get(&transient.r#ref))
                            .ok_or_else(|| {
                                anyhow!("transient value {} not found", transient.r#ref)
                            })?;
                        cs.add_effect(pg_id, sync_action, *var, CsExpression::from(*init))
                            .expect("set transient value");
                    }

                    // Transition to edge location
                    let edge_location = cs.new_location(pg_id).expect("new location");
                    let guard = edge
                        .guard
                        .as_ref()
                        .map(|guard| {
                            self.build_expression(
                                &guard.exp,
                                Some(Type::Boolean),
                                &automaton_builder.local_vars,
                            )
                            .and_then(|guard| {
                                if let CsExpression::Boolean(guard) = guard {
                                    Ok(guard)
                                } else {
                                    Err(anyhow!("guard is not a boolean expression"))
                                }
                            })
                        })
                        .transpose()
                        .with_context(|| {
                            format!(
                                "failed creating guard expression for edge in automaton {}",
                                automaton.name
                            )
                        })?;

                    // NOTE: different edges can have the same sync action
                    // and a non-deterministic choice of edge location
                    cs.add_transition(pg_id, pre_loc, sync_action, edge_location, guard)
                        .expect("add transition");

                    // consider previous occurrences of transitions triggered by same sync action
                    // and allow them to execute first while current automaton sits on edge_location
                    for prev_automaton_builder in &mut self.automaton_builders[..element_idx] {
                        if let Some(v) = prev_automaton_builder.dest_actions.get(&sync_action) {
                            for (prev_dest_action, _prev_dest_location) in v.iter() {
                                // for the same sync_action, add previous dest_actions loops in edge_location
                                cs.add_transition(
                                    pg_id,
                                    edge_location,
                                    *prev_dest_action,
                                    edge_location,
                                    None,
                                )
                                .expect("add transition");
                            }
                        }
                    }

                    let mut prob_lower_bound: Option<FloatExpr<_>> = None;

                    for dest in edge.destinations.iter() {
                        let dest_action = cs.new_action(pg_id)?;
                        let dest_location = cs.new_location(pg_id).expect("new location");

                        let automaton_builder = self
                            .automaton_builders
                            .get(element_idx)
                            .expect("automaton builder");

                        // add effects
                        for assignment in &dest.assignments {
                            if assignment.index > 0 {
                                warn!("index of assignments unsupported; ignored");
                            }
                            let (var, _, r#type) = automaton_builder
                                .local_vars
                                .get(&assignment.r#ref)
                                .or_else(|| self.global_vars.get(&assignment.r#ref))
                                .ok_or_else(|| anyhow!("unknown variable {}", assignment.r#ref))?;
                            let effect = self
                                .build_expression(
                                    &assignment.value,
                                    Some(*r#type),
                                    &automaton_builder.local_vars,
                                )
                                .with_context(|| {
                                    format!(
                                        "failed building expression for assignment of variable {}",
                                        assignment.r#ref
                                    )
                                })?;
                            cs.add_effect(pg_id, dest_action, *var, effect)
                                .expect("add effect");
                        }

                        // dest actions need to also set transient variables of destination
                        for transient in &automaton
                            .locations
                            .iter()
                            .find(|loc| loc.name == dest.location)
                            .ok_or_else(|| {
                                anyhow!("transition destination {} not found", dest.location)
                            })?
                            .transient_values
                        {
                            let (var, _, r#type) = automaton_builder
                                .local_vars
                                .get(&transient.r#ref)
                                .or_else(|| self.global_vars.get(&transient.r#ref))
                                .ok_or_else(|| {
                                    anyhow!("transient value {} not found", transient.r#ref)
                                })?;
                            let effect = self.build_expression(
                                &transient.value,
                                Some(*r#type),
                                &automaton_builder.local_vars,
                            )?;
                            cs.add_effect(pg_id, dest_action, *var, effect)
                                .expect("set transient value");
                        }

                        let prob_expr;
                        if let Some(ref prob) = dest.probability {
                            let probability;
                            if let scan_core::Expression::Float(prob) = self.build_expression(
                                &prob.exp,
                                Some(Type::Float),
                                &automaton_builder.local_vars,
                            )? {
                                probability = prob;
                            } else {
                                bail!("probability is not a float expression");
                            }
                            if let Some(prob_lower_bound) = prob_lower_bound.as_mut() {
                                let prob_upper_bound = prob_lower_bound.clone() + probability;
                                prob_expr = Some(
                                    BooleanExpr::FloatLessEq(
                                        prob_lower_bound.clone(),
                                        FloatExpr::Var(automaton_builder.rng),
                                    ) & BooleanExpr::FloatLess(
                                        FloatExpr::Var(automaton_builder.rng),
                                        prob_upper_bound.clone(),
                                    ),
                                );
                                *prob_lower_bound = prob_upper_bound;
                            } else {
                                prob_expr = Some(BooleanExpr::FloatLess(
                                    FloatExpr::Var(automaton_builder.rng),
                                    probability.clone(),
                                ));
                                prob_lower_bound = Some(probability);
                            }
                        } else {
                            if let Some(prob_lower_bound) = prob_lower_bound.as_mut() {
                                prob_expr = Some(BooleanExpr::FloatLessEq(
                                    prob_lower_bound.clone(),
                                    FloatExpr::Var(automaton_builder.rng),
                                ));
                                *prob_lower_bound = FloatExpr::Const(1.0);
                            } else {
                                prob_expr = None;
                            }
                        }
                        cs.add_transition(
                            pg_id,
                            edge_location,
                            dest_action,
                            dest_location,
                            prob_expr,
                        )
                        .expect("add transition");

                        // consider previous occurrences of transitions triggered by same sync action
                        for prev_automaton_builder in &mut self.automaton_builders[..element_idx] {
                            if let Some(v) = prev_automaton_builder.dest_actions.get(&sync_action) {
                                for (_prev_dest_action, prev_dest_location) in v.iter() {
                                    // add dest_action loop in previous dest_locations for the same sync_action
                                    cs.add_transition(
                                        pg_id,
                                        *prev_dest_location,
                                        dest_action,
                                        *prev_dest_location,
                                        None,
                                    )
                                    .expect("add transition");
                                }
                            }
                        }

                        // elements unaffected by sync must ignore the dest action in every idle location
                        for (automaton_builder, _) in self
                            .automaton_builders
                            .iter()
                            .zip(&sync.synchronise)
                            .filter(|(_, action)| action.is_none())
                        {
                            let idle_loc = *automaton_builder
                                .idle_locations
                                .get(&sync_action)
                                .expect("idle location");
                            cs.add_transition(pg_id, idle_loc, dest_action, idle_loc, None)
                                .expect("add transition");
                        }

                        // need to borrow mutably now
                        let automaton_builder = self
                            .automaton_builders
                            .get_mut(element_idx)
                            .expect("automaton builder");

                        automaton_builder
                            .dest_actions
                            .entry(sync_action)
                            .or_default()
                            .push((dest_action, dest_location));

                        let (post_loc, post_loc_idx) = *automaton_builder
                            .locations
                            .get(&dest.location)
                            .ok_or_else(|| {
                                anyhow!(
                                    "missing location {} in automaton {}",
                                    dest.location,
                                    element.automaton
                                )
                            })?;

                        cs.add_effect(
                            pg_id,
                            dest_action,
                            automaton_builder.current_loc,
                            CsExpression::from(post_loc_idx),
                        )
                        .expect("set current location");

                        // Send global state to channel and transition to post location
                        if any_unaffected {
                            cs.add_transition(
                                pg_id,
                                dest_location,
                                system_action,
                                dest_location,
                                None,
                            )
                            .expect("add location");
                            cs.add_autonomous_transition(pg_id, dest_location, post_loc, None)
                                .expect("add location");
                        } else {
                            cs.add_transition(pg_id, dest_location, system_action, post_loc, None)
                                .expect("add location");
                        }
                    }
                }
            }
        }

        let cs = cs.build();
        let mut cs_model = TransitionSystem::new(cs);
        // global state port, only one we need
        cs_model.add_port(global_state_channel, global_state_init)?;

        // Add properties
        let properties = if properties.is_empty() {
            jani_model
                .properties
                .iter()
                .map(|p| p.name.clone())
                .collect()
        } else {
            properties.to_vec()
        };

        let property_exprs = jani_model
            .properties
            .iter()
            .filter(|p| properties.contains(&p.name))
            .map(|p| {
                self.build_property(&p.expression).and_then(|p| match p {
                    Either::Left(expr) => {
                        if let scan_core::Expression::Boolean(bexpr) = expr {
                            Ok(Mtl::Atom(bexpr))
                        } else {
                            Err(anyhow!("predicate not a boolean expression"))
                        }
                    }
                    Either::Right(mtl) => Ok(mtl),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        fn extract_predicates(
            prop: &Mtl<scan_core::BooleanExpr<Atom>>,
        ) -> Vec<scan_core::BooleanExpr<Atom>> {
            match prop {
                Mtl::Atom(pred) => vec![pred.clone()],
                Mtl::Until(lhs, rhs) => vec![lhs.clone(), rhs.clone()],
                Mtl::Eventually(exp) | Mtl::Always(exp) => vec![exp.clone()],
            }
        }
        fn extract_mtl(prop: &Mtl<scan_core::BooleanExpr<Atom>>, idx: &mut usize) -> Mtl<usize> {
            match prop {
                Mtl::Atom(_) => {
                    let prop = Mtl::Atom(*idx);
                    *idx += 1;
                    prop
                }
                Mtl::Until(_, _) => {
                    let prop = Mtl::Until(*idx, *idx + 1);
                    *idx += 2;
                    prop
                }
                Mtl::Eventually(_) => {
                    let prop = Mtl::Eventually(*idx);
                    *idx += 1;
                    prop
                }
                Mtl::Always(_) => {
                    let prop = Mtl::Always(*idx);
                    *idx += 1;
                    prop
                }
            }
        }
        let mut idx = 0;
        let mut oracle = MtlOracle::default();
        property_exprs
            .iter()
            .flat_map(|prop| extract_predicates(prop).into_iter())
            .try_for_each(|predicate| cs_model.add_predicate(predicate))?;
        property_exprs
            .into_iter()
            .map(|p| extract_mtl(&p, &mut idx))
            .for_each(|mtl| oracle.add_guarantee(mtl));

        // Finalize, build and return everything
        // , global_vars, predicates);
        let data = self.data(properties);
        Ok((cs_model, oracle, data))
    }

    fn add_global_constant(&mut self, c: &ConstantDeclaration) -> anyhow::Result<()> {
        // TODO WARN FIXME: in JANI initial values are random?
        let c_type = (&c.r#type).try_into().expect("convert type");
        let val = c
            .value
            .as_ref()
            .and_then(|expr| {
                self.build_expression(expr, Some(c_type), &HashMap::new())
                    .and_then(|e| e.eval_constant().map_err(|err| anyhow!(err)))
                    .ok()
            })
            .ok_or_else(|| anyhow!("missing initial value"))?;
        self.global_constants.insert(c.name.clone(), val);
        Ok(())
    }

    fn add_local_var(
        &self,
        cs: &mut ChannelSystemBuilder,
        pg_id: PgId,
        var: &VariableDeclaration,
        local_vars: &mut HashMap<String, (Var, Val, Type)>,
    ) -> anyhow::Result<()> {
        // TODO WARN FIXME: in JANI initial values are random?
        let var_type = (&var.r#type).try_into().expect("convert type");
        let init = var
            .initial_value
            .as_ref()
            .and_then(|expr| self.build_expression(expr, Some(var_type), local_vars).ok())
            .unwrap_or_else(|| CsExpression::from(var_type.default_value()));
        let val = init.eval_constant()?;
        let t = val.r#type();
        let var_id = cs.new_var(pg_id, val)?;
        local_vars.insert(var.name.clone(), (var_id, val, t));
        Ok(())
    }

    fn data(self, properties: Vec<String>) -> JaniModelData {
        JaniModelData {
            actions: self
                .system_actions
                .into_iter()
                .map(|(name, action)| (action, name))
                .collect::<HashMap<_, _>>(),
            ports: self
                .global_state_vec
                .into_iter()
                .map(|name| {
                    self.global_vars
                        .get(&name)
                        .map(|(_var, _, t)| (name, *t))
                        .unwrap()
                })
                .collect(),
            guarantees: properties,
        }
    }

    fn build_expression(
        &self,
        expr: &Expression,
        type_hint: Option<Type>,
        local_vars: &HashMap<String, (Var, Val, Type)>,
    ) -> anyhow::Result<CsExpression> {
        match expr {
            Expression::ConstantValue(constant_value) => match constant_value {
                parser::ConstantValue::Boolean(b) => Ok(CsExpression::from(*b)),
                parser::ConstantValue::Constant(constant) => match constant {
                    parser::Constant::Euler => Ok(CsExpression::from(std::f64::consts::E)),
                    parser::Constant::Pi => Ok(CsExpression::from(std::f64::consts::PI)),
                },
                parser::ConstantValue::NumberReal(num) => Ok(CsExpression::from(*num)),
                parser::ConstantValue::NumberInt(num) => match type_hint {
                    Some(Type::Float) => Ok(CsExpression::from(*num as Float)),
                    Some(Type::Natural) => Ok(CsExpression::from(*num as Natural)),
                    _ => Ok(CsExpression::from(*num as Integer)),
                },
            },
            Expression::Identifier(id) => local_vars
                .get(id)
                .or_else(|| self.global_vars.get(id))
                .map(|(var, _, t)| CsExpression::from_var(*var, *t))
                .or_else(|| {
                    self.global_constants
                        .get(id)
                        .cloned()
                        .map(CsExpression::from)
                })
                .ok_or_else(|| anyhow!("unknown id `{id}`")),
            Expression::IfThenElse {
                op,
                r#if,
                then,
                r#else,
            } => {
                let r#if = self.build_expression(r#if, Some(Type::Boolean), local_vars)?;
                let then = self.build_expression(then, type_hint, local_vars)?;
                let r#else = self.build_expression(r#else, type_hint, local_vars)?;
                match op {
                    parser::IteOp::Ite => r#if.ite(then, r#else).map_err(anyhow::Error::from),
                }
            }
            Expression::Bool { op, left, right } => {
                let left = self.build_expression(left, Some(Type::Boolean), local_vars)?;
                let right = self.build_expression(right, Some(Type::Boolean), local_vars)?;
                match op {
                    BoolOp::And => left & right,
                    BoolOp::Or => left | right,
                    BoolOp::Implies => Ok(CsExpression::Boolean(BooleanExpr::Implies(Box::new((
                        BooleanExpr::try_from(left)?,
                        BooleanExpr::try_from(right)?,
                    ))))),
                }
                .map_err(anyhow::Error::from)
            }
            Expression::Neg { op, exp } => {
                let exp = self.build_expression(exp, Some(Type::Boolean), local_vars)?;
                match op {
                    parser::NegOp::Neg => (!exp).map_err(|err| err.into()),
                }
            }
            Expression::EqComp { op, left, right } => {
                let left = self.build_expression(left, None, local_vars)?;
                let right = self.build_expression(right, None, local_vars)?;
                match op {
                    parser::EqCompOp::Eq => CsExpression::equal_to(left, right),
                    parser::EqCompOp::Neq => CsExpression::equal_to(left, right).map(|expr| !expr),
                }
                .map(CsExpression::Boolean)
                .map_err(anyhow::Error::from)
            }
            Expression::NumComp { op, left, right } => {
                let left = self.build_expression(left, None, local_vars)?;
                let right = self.build_expression(right, None, local_vars)?;

                match op {
                    parser::NumCompOp::Less => CsExpression::less_than(left, right),
                    parser::NumCompOp::Leq => CsExpression::less_than_or_equal_to(left, right),
                    parser::NumCompOp::Greater => CsExpression::greater_than(left, right),
                    parser::NumCompOp::Geq => CsExpression::greater_than_or_equal_to(left, right),
                }
                .map(CsExpression::Boolean)
                .map_err(anyhow::Error::from)
            }
            Expression::IntOp { op, left, right } => {
                let left = self.build_expression(left, type_hint, local_vars)?;
                let right = self.build_expression(right, type_hint, local_vars)?;
                match op {
                    parser::IntOp::Plus => left + right,
                    parser::IntOp::Minus => left + (-right)?,
                    parser::IntOp::Mult => left * right,
                    parser::IntOp::Mod
                        if type_hint.is_some_and(|hint| matches!(hint, Type::Natural)) =>
                    {
                        let left = NaturalExpr::try_from(left)?;
                        let right = NaturalExpr::try_from(right)?;
                        Ok(CsExpression::Natural(NaturalExpr::Rem(Box::new((
                            left, right,
                        )))))
                    }
                    parser::IntOp::Mod => {
                        let left = IntegerExpr::try_from(left)?;
                        let right = IntegerExpr::try_from(right)?;
                        Ok(CsExpression::Integer(IntegerExpr::Rem(Box::new((
                            left, right,
                        )))))
                    }
                }
                .map_err(anyhow::Error::from)
            }
            Expression::RealOp { op, left, right } => {
                let left = self.build_expression(left, Some(Type::Float), local_vars)?;
                let right = self.build_expression(right, Some(Type::Float), local_vars)?;
                match op {
                    parser::RealOp::Div => left / right,
                    parser::RealOp::Pow => todo!(),
                    parser::RealOp::Log => todo!(),
                }
                .map_err(anyhow::Error::from)
            }
            Expression::Real2IntOp { op, exp } => {
                let exp = self.build_expression(exp, None, local_vars)?;
                if matches!(exp.r#type(), Type::Float) {
                    Ok(CsExpression::Integer(match op {
                        parser::Real2IntOp::Floor => IntegerExpr::Floor,
                        parser::Real2IntOp::Ceil => IntegerExpr::Ceil,
                    }(Box::new(
                        FloatExpr::try_from(exp)?,
                    ))))
                } else {
                    bail!(TypeError::TypeMismatch)
                }
            }
            Expression::MinMax { op, left, right } => {
                let left = self.build_expression(left, None, local_vars)?;
                let right = self.build_expression(right, None, local_vars)?;
                match op {
                    parser::MinMaxOp::Min => left.min(right),
                    parser::MinMaxOp::Max => left.max(right),
                }
                .map_err(anyhow::Error::from)
            }
        }
    }

    fn build_property(
        &self,
        prop: &PropertyExpression,
    ) -> anyhow::Result<Either<scan_core::Expression<Atom>, Mtl<scan_core::BooleanExpr<Atom>>>>
    {
        use scan_core::Expression;
        match prop {
            PropertyExpression::Filter {
                op,
                fun,
                values,
                states,
            } => {
                match op {
                    parser::FilterOp::Filter => {
                        // OK
                        // (nothing to do as this is only choice)
                    }
                }
                match fun {
                    parser::FunOp::Values => {
                        // OK
                    }
                    _ => bail!(
                        "unsupported property filter function: only uniform scheduler is supported"
                    ),
                }
                match states.as_ref() {
                    PropertyExpression::States { op } => match op {
                        parser::StatesOp::Initial => {
                            // OK
                        }
                        parser::StatesOp::Deadlock | parser::StatesOp::Timelock => {
                            bail!("unsupported filter states: only initial states supported")
                        }
                    },
                    _ => {
                        bail!("unsupported filter states: expression has to be a state expression")
                    }
                }
                match values.as_ref() {
                    PropertyExpression::PMinMax { op, exp } => {
                        match op {
                            parser::PMinMaxOp::Pmin => warn!("only uniform scheduler is supported"),
                            parser::PMinMaxOp::Pmax => warn!("only uniform scheduler is supported"),
                        }
                        self.build_property(exp.as_ref())
                    }
                    _ => bail!("unexpected expression: expected maximum/minimum probability"),
                }
            }
            PropertyExpression::PMinMax { op: _, exp: _ } => {
                bail!("unexpected expression: must occur inside of a filter expression")
            }
            PropertyExpression::ConstantValue(constant_value) => {
                Ok(Either::Left(match constant_value {
                    parser::ConstantValue::Boolean(b) => Expression::from(*b),
                    parser::ConstantValue::Constant(constant) => match constant {
                        parser::Constant::Euler => Expression::from(std::f64::consts::E),
                        parser::Constant::Pi => Expression::from(std::f64::consts::PI),
                    },
                    parser::ConstantValue::NumberReal(num) => Expression::from(*num),
                    parser::ConstantValue::NumberInt(num) => Expression::from(*num),
                }))
            }
            PropertyExpression::Identifier(id) => {
                if let Some((_, _, t)) = self.global_vars.get(id) {
                    let var_idx = self
                        .global_state_vec
                        .iter()
                        .position(|name| name == id)
                        .ok_or_else(|| anyhow!("var not a system var"))?;
                    let atom = Atom::State(
                        self.global_state_channel.expect("global state channel"),
                        var_idx,
                    );
                    let expr = Expression::from_var(atom, *t);
                    Ok(Either::Left(expr))
                } else if let Some(constant) =
                    self.global_constants.get(id).cloned().map(Expression::from)
                {
                    Ok(Either::Left(constant))
                } else {
                    Err(anyhow!("unknown identifier"))
                }
            }
            PropertyExpression::IfThenElse {
                op,
                r#if,
                then,
                r#else,
            } => {
                let r#if = self.build_property(r#if)?.left().expect("expression");
                let then = self.build_property(then)?.left().expect("expression");
                let r#else = self.build_property(r#else)?.left().expect("expression");
                match op {
                    parser::IteOp::Ite => r#if.ite(then, r#else),
                }
                .map(Either::Left)
                .map_err(anyhow::Error::from)
            }
            PropertyExpression::Bool { op, left, right } => {
                let left = self.build_property(left)?.left().expect("expression");
                let right = self.build_property(right)?.left().expect("expression");
                match op {
                    BoolOp::And => left & right,
                    BoolOp::Or => left | right,
                    BoolOp::Implies => Ok(Expression::Boolean(BooleanExpr::Implies(Box::new((
                        BooleanExpr::try_from(left)?,
                        BooleanExpr::try_from(right)?,
                    ))))),
                }
                .map(Either::Left)
                .map_err(anyhow::Error::from)
            }
            PropertyExpression::Neg { op, exp } => {
                let exp = self.build_property(exp)?.left().expect("expression");
                match op {
                    parser::NegOp::Neg => (!exp).map_err(|err| err.into()),
                }
                .map(Either::Left)
            }
            PropertyExpression::EqComp { op, left, right } => {
                let left = self.build_property(left)?.left().expect("expression");
                let right = self.build_property(right)?.left().expect("expression");
                if left.r#type() == right.r#type()
                    || (matches!(left.r#type(), Type::Integer | Type::Float)
                        && matches!(right.r#type(), Type::Integer | Type::Float))
                {
                    match op {
                        parser::EqCompOp::Eq => Expression::equal_to(left, right),
                        parser::EqCompOp::Neq => {
                            Expression::equal_to(left, right).map(|expr| !expr)
                        }
                    }
                    .map(Expression::Boolean)
                    .map(Either::Left)
                    .map_err(anyhow::Error::from)
                } else {
                    bail!(TypeError::TypeMismatch)
                }
            }
            PropertyExpression::NumComp { op, left, right } => {
                let left = self.build_property(left)?.left().expect("expression");
                let right = self.build_property(right)?.left().expect("expression");
                match op {
                    parser::NumCompOp::Less => Expression::less_than(left, right),
                    parser::NumCompOp::Leq => Expression::less_than_or_equal_to(left, right),
                    parser::NumCompOp::Greater => Expression::greater_than(left, right),
                    parser::NumCompOp::Geq => Expression::greater_than_or_equal_to(left, right),
                }
                .map(Expression::Boolean)
                .map(Either::Left)
                .map_err(anyhow::Error::from)
            }
            PropertyExpression::IntOp { op, left, right } => {
                let left = self.build_property(left)?.left().expect("expression");
                let right = self.build_property(right)?.left().expect("expression");
                match op {
                    parser::IntOp::Plus => left + right,
                    parser::IntOp::Minus => left + (-right)?,
                    parser::IntOp::Mult => left * right,
                    parser::IntOp::Mod => left / right,
                }
                .map(Either::Left)
                .map_err(anyhow::Error::from)
            }
            PropertyExpression::RealOp { op, left, right } => {
                let left = self.build_property(left)?.left().expect("expression");
                let right = self.build_property(right)?.left().expect("expression");
                match op {
                    parser::RealOp::Div => left / right,
                    parser::RealOp::Pow => todo!(),
                    parser::RealOp::Log => todo!(),
                }
                .map(Either::Left)
                .map_err(anyhow::Error::from)
            }
            PropertyExpression::Real2IntOp { op: _, exp: _ } => todo!(),
            PropertyExpression::Until {
                op,
                left,
                right,
                time_bounds: _,
            } => {
                let left = self
                    .build_property(left)?
                    .left()
                    .ok_or(anyhow!("unsupported property"))?;
                let left = if let scan_core::Expression::Boolean(expr) = left {
                    expr
                } else {
                    bail!("not a boolean expression")
                };
                let right = self
                    .build_property(right)?
                    .left()
                    .ok_or(anyhow!("unsupported property"))?;
                let right = if let scan_core::Expression::Boolean(expr) = right {
                    expr
                } else {
                    bail!("not a boolean expression")
                };
                Ok(Either::Right(match op {
                    parser::UntilOp::Until => Mtl::Until(left, right),
                    parser::UntilOp::WeakUntil => todo!(),
                }))
            }
            PropertyExpression::DerivedTemp {
                op,
                exp,
                time_bounds: _,
            } => {
                let exp = self
                    .build_property(exp)?
                    .left()
                    .ok_or(anyhow!("unsupported property"))?;
                let exp = if let scan_core::Expression::Boolean(exp) = exp {
                    exp
                } else {
                    bail!("not a boolean expression")
                };
                Ok(Either::Right(match op {
                    parser::DerivedTempOp::Eventually => Mtl::Eventually(exp),
                    parser::DerivedTempOp::Always => Mtl::Always(exp),
                }))
            }
            PropertyExpression::States { op: _ } => bail!("unexpected states expression"),
        }
    }
}
