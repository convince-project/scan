use super::Model;
use crate::parser::{
    self, BoolOp, ConstantDeclaration, Expression, PropertyExpression, VariableDeclaration,
};
use anyhow::{Context, anyhow, bail};
use either::Either;
use scan_core::{
    Atom, BooleanExpr, CsModel, Float, FloatExpr, Integer, Mtl, MtlOracle, Natural, Type,
    TypeError, Val,
    channel_system::{
        self, Action, Channel, ChannelSystemBuilder, CsExpression, Location, PgId, Var,
    },
};
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
) -> anyhow::Result<(CsModel, MtlOracle, JaniModelData)> {
    let builder = JaniBuilder::default();
    builder.build(jani_model, properties)
}

#[derive(Default, Debug, Clone)]
struct JaniBuilder {
    system_actions: HashMap<String, channel_system::Action>,
    // Sync actions (same order as syncs in model)
    sync_actions: Vec<channel_system::Action>,
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
    locations: HashMap<String, (channel_system::Location, channel_system::Location)>,
    local_vars: HashMap<String, (Var, Val, Type)>,
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
}

impl AutomatonBuilder {
    fn new(rng: Var) -> Self {
        AutomatonBuilder {
            locations: HashMap::new(),
            local_vars: HashMap::new(),
            dest_actions: HashMap::new(),
            rng,
        }
    }
}

impl JaniBuilder {
    const RNG: &str = "__RNG__";
    const SILENT: &str = "__SILENT__";

    pub(crate) fn build(
        mut self,
        jani_model: Model,
        properties: &[String],
    ) -> anyhow::Result<(CsModel, MtlOracle, JaniModelData)> {
        let mut cs = ChannelSystemBuilder::new();
        let pg_id = cs.new_program_graph();

        jani_model
            .constants
            .iter()
            .try_for_each(|c| self.add_global_constant(c))?;

        let len = jani_model.variables.len();
        let mut global_state_init = Vec::with_capacity(len);
        let mut global_state_type = Vec::with_capacity(len);
        let mut global_state_expr = Vec::with_capacity(len);
        for variable_dec in jani_model.variables.iter() {
            let var_type = (&variable_dec.r#type).try_into().expect("convert type");
            let init = variable_dec
                .initial_value
                .as_ref()
                .and_then(|expr| {
                    self.build_expression(expr, Some(var_type), &self.global_vars, None)
                        .ok()
                })
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
        let global_state_channel = cs.new_channel(global_state_type, None);
        self.global_state_channel = Some(global_state_channel);

        // for every system action (including silent one),
        // create action which send global state to channel
        let silent_system_action =
            cs.new_send(pg_id, global_state_channel, global_state_expr.clone())?;
        for system_action in &jani_model.actions {
            let system_action_id =
                cs.new_send(pg_id, global_state_channel, global_state_expr.clone())?;
            self.system_actions
                .insert(system_action.name.clone(), system_action_id);
        }

        for element in &jani_model.system.elements {
            let rng = cs.new_var(pg_id, Val::from(0.)).expect("new var");

            let mut automaton_builder = AutomatonBuilder::new(rng);
            let automaton = jani_model
                .automata
                .iter()
                .find(|aut| aut.name == element.automaton)
                .ok_or_else(|| anyhow!("missing automaton {}", element.automaton))?;

            // create locations
            for location in &automaton.locations {
                let loc = cs.new_location(pg_id).expect("new location");
                let idle_loc = cs.new_location(pg_id).expect("new location");
                // elements must return to non-idle states upon system actions in every location
                cs.add_transition(pg_id, idle_loc, silent_system_action, loc, None)
                    .expect("add transition");
                for system_action in self.system_actions.values() {
                    cs.add_transition(pg_id, idle_loc, *system_action, loc, None)
                        .expect("add transition");
                }
                automaton_builder
                    .locations
                    .insert(location.name.clone(), (loc, idle_loc));
            }

            // initial locations
            let initial = cs.new_initial_location(pg_id).expect("initial location");
            for loc in &automaton.initial_locations {
                let (loc, _) = *automaton_builder
                    .locations
                    .get(loc)
                    .ok_or_else(|| anyhow!("initial location {loc} missing"))?;
                cs.add_autonomous_transition(pg_id, initial, loc, None)
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

        // Extend system syncs with async silent actions for each element
        let num_elements = self.automaton_builders.len();
        let silent_syncs = (0..num_elements).map(|n| {
            let mut synchronise = vec![None; num_elements];
            synchronise[n] = Some(Self::SILENT.to_string());
            parser::Sync {
                synchronise,
                result: None,
                _comment: String::new(),
            }
        });

        // for every sync, create sync action
        for sync in silent_syncs.chain(jani_model.system.syncs) {
            let sync_action = cs.new_action(pg_id).expect("new action");
            self.sync_actions.push(sync_action);

            // Sync actions initiate a transition so they need to reset transient variables
            for var_decl in jani_model
                .variables
                .iter()
                .filter(|var_decl| var_decl.transient)
            {
                let (var, init, _) = self.global_vars.get(&var_decl.name).expect("variable");
                cs.add_effect(pg_id, sync_action, *var, CsExpression::from(*init))
                    .expect("effect");
            }

            // elements unaffected by sync must ignore the sync action in every location
            // by moving to idle location
            for (element_idx, (element, _)) in jani_model
                .system
                .elements
                .iter()
                .zip(&sync.synchronise)
                .enumerate()
                .filter(|(_, (_, action))| action.is_none())
            {
                let automaton = jani_model
                    .automata
                    .iter()
                    .find(|aut| aut.name == element.automaton)
                    .ok_or_else(|| anyhow!("missing automaton {}", element.automaton))?;

                let automaton_builder = self
                    .automaton_builders
                    .get(element_idx)
                    .expect("automaton builder");

                for location in &automaton.locations {
                    let (loc, idle_loc) = *automaton_builder
                        .locations
                        .get(&location.name)
                        .ok_or_else(|| {
                            anyhow!(
                                "missing location {} in automaton {}",
                                location.name,
                                element.automaton
                            )
                        })?;
                    cs.add_transition(pg_id, loc, sync_action, idle_loc, None)
                        .expect("add transition");
                }

                // Sync actions initiate a transition so they need to reset transient variables
                for var_decl in automaton
                    .variables
                    .iter()
                    .filter(|var_decl| var_decl.transient)
                {
                    let (var, init, _) = automaton_builder
                        .local_vars
                        .get(&var_decl.name)
                        .expect("variable");
                    cs.add_effect(pg_id, sync_action, *var, CsExpression::from(*init))
                        .expect("effect");
                }
            }

            // For every element involved in the sync action,
            // build transitions associated to relevant edges
            for (element_idx, element, action) in jani_model
                .system
                .elements
                .iter()
                .zip(&sync.synchronise)
                .enumerate()
                .filter_map(|(element_idx, (element, action))| {
                    action.as_ref().map(|action| (element_idx, element, action))
                })
            {
                let automaton = jani_model
                    .automata
                    .iter()
                    .find(|aut| aut.name == element.automaton)
                    .ok_or_else(|| anyhow!("missing automaton {}", element.automaton))?;

                let automaton_builder_rng = self
                    .automaton_builders
                    .get(element_idx)
                    .expect("automaton builder")
                    .rng;

                // Sync actions randomize automaton's RNG
                cs.add_effect(
                    pg_id,
                    sync_action,
                    automaton_builder_rng,
                    CsExpression::Float(FloatExpr::Rand(Box::new((
                        FloatExpr::from(0.),
                        FloatExpr::from(1.),
                    )))),
                )
                .expect("add effect");

                // create edges for the automaton action corresponding to the sync action
                for edge in automaton
                    .edges
                    .iter()
                    .filter(|edge| edge.action.as_ref().map_or(Self::SILENT, |v| v) == action)
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
                                // guards should not use rand (or have effects at all)
                                None,
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
                                    Some(automaton_builder.rng),
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
                        let location = automaton
                            .locations
                            .iter()
                            .find(|loc| loc.name == dest.location)
                            .ok_or_else(|| {
                                anyhow!("transition destination {} not found", dest.location)
                            })?;
                        for transient in &location.transient_values {
                            let (var, _init, r#type) = automaton_builder
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
                                Some(automaton_builder.rng),
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
                                None,
                            )? {
                                probability = prob;
                            } else {
                                bail!("probability is not a float expression");
                            }
                            if let Some(prob_lower_bound) = prob_lower_bound.as_mut() {
                                let prob_upper_bound = prob_lower_bound.clone() + probability;
                                prob_expr = Some(
                                    BooleanExpr::FloatLess(
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
                                prob_expr = Some(BooleanExpr::FloatLess(
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
                        for (element_idx, (element, _)) in jani_model
                            .system
                            .elements
                            .iter()
                            .zip(&sync.synchronise)
                            .enumerate()
                            .filter(|(_, (_, action))| action.is_none())
                        {
                            let automaton = jani_model
                                .automata
                                .iter()
                                .find(|automaton| automaton.name == element.automaton)
                                .ok_or_else(|| {
                                    anyhow!("missing automaton {}", element.automaton)
                                })?;

                            let automaton_builder = self
                                .automaton_builders
                                .get(element_idx)
                                .expect("automaton builder");

                            for location in &automaton.locations {
                                let (_, idle_loc) = *automaton_builder
                                    .locations
                                    .get(&location.name)
                                    .ok_or_else(|| {
                                        anyhow!(
                                            "missing location {} in automaton {}",
                                            location.name,
                                            element.automaton
                                        )
                                    })?;
                                cs.add_transition(pg_id, idle_loc, dest_action, idle_loc, None)
                                    .expect("add transition");
                            }
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

                        let (post_loc, _) = *automaton_builder
                            .locations
                            .get(&dest.location)
                            .ok_or_else(|| {
                                anyhow!(
                                    "missing location {} in automaton {}",
                                    dest.location,
                                    element.automaton
                                )
                            })?;

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
                        // Send global state to channel and transition to post location
                        cs.add_transition(pg_id, dest_location, system_action, post_loc, None)
                            .expect("add location");
                    }
                }
            }
        }

        let mut cs_model = CsModel::new(cs);
        // global state port, only one we need
        cs_model.add_port(global_state_channel, global_state_init);

        // jani_model.system.syncs.iter().for_each(|sync| {
        //     let result = sync.result.as_ref().expect("no silent actions");
        //     if !self.system_actions.contains_key(result) {
        //         let action = cs.new_action(pg_id).expect("pg exists");
        //         let prev = self.system_actions.insert(result.clone(), action);
        //         assert!(prev.is_none(), "checked by above if condition");
        //     }
        // });

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
            }
        }
        let mut idx = 0;
        let mut oracle = MtlOracle::default();
        property_exprs
            .iter()
            .flat_map(|prop| extract_predicates(prop).into_iter())
            .for_each(|predicate| {
                cs_model.add_predicate(predicate);
            });
        property_exprs
            .into_iter()
            .map(|p| extract_mtl(&p, &mut idx))
            .for_each(|mtl| oracle.add_guarantee(mtl));

        // Finalize, build and return everything
        // , global_vars, predicates);
        let data = self.data(properties);
        Ok((cs_model, oracle, data))
    }

    // fn init(&mut self, jani_model: &mut Model) -> anyhow::Result<()> {
    //     // New initial action sync over all elements
    //     let sync_initial = Sync {
    //         synchronise: vec![Some(String::from(Self::INITIAL)); jani_model.system.elements.len()],
    //         result: Some(String::from(Self::INITIAL)),
    //         _comment: String::new(),
    //     };
    //     jani_model.system.syncs.push(sync_initial);

    //     for automaton in &mut jani_model.automata {
    //         // New "real" initial location, needed to instantiate automaton's initial locations
    //         let gen_initial = Location {
    //             name: String::from(Self::INITIAL),
    //             transient_values: Vec::new(),
    //             _comment: String::new(),
    //         };
    //         automaton.locations.push(gen_initial);

    //         let init_edge = Edge {
    //             location: String::from(Self::INITIAL),
    //             action: Some(String::from(Self::INITIAL)),
    //             guard: None,
    //             destinations: automaton
    //                 .initial_locations
    //                 .iter()
    //                 .cloned()
    //                 .map(|location| Destination {
    //                     location,
    //                     probability: None,
    //                     assignments: Vec::new(),
    //                     _comment: String::new(),
    //                 })
    //                 .collect(),
    //             _comment: String::new(),
    //         };
    //         automaton.edges.push(init_edge);

    //         // Replace initial location
    //         automaton.initial_locations = vec![String::from(Self::INITIAL)];
    //     }
    //     Ok(())
    // }

    // // An action in JANI does not carry effects,
    // // so we need to duplicate actions until each one has unique effects.
    // // The modified model is such that:
    // //
    // // - Every action has a unique set of assignments (duplicates actions).
    // // - Every edge has a unique destination (because destinations are tied to assignments).
    // // - Probability is encoded in guard
    // fn normalize(&mut self, jani_model: &mut Model) -> anyhow::Result<()> {
    //     // index is global so there is no risk of name-clash
    //     let mut idx = 0;
    //     let rng = Expression::Identifier(String::from(Self::RNG));
    //     for automaton in &mut jani_model.automata {
    //         let mut new_edges = Vec::new();
    //         for edge in &mut automaton.edges {
    //             // Avoid silent actions
    //             if edge.action.is_none() {
    //                 edge.action = Some(Self::GEN.to_string() + &idx.to_string());
    //                 idx += 1;
    //             }
    //             let edge_action = edge.action.clone().expect("no silent action");
    //             assert!(!edge_action.is_empty());
    //             let mut prob: Option<Expression> = None;
    //             for dest in &mut edge.destinations {
    //                 // Add probability to guard
    //                 let mut guard_exp = edge.guard.as_ref().map(|guard| guard.exp.clone());
    //                 if let Some(ref p) = dest.probability {
    //                     // First probability case has no lower bound
    //                     if let Some(ref prob) = prob {
    //                         let lower_bound = Expression::NumComp {
    //                             op: NumCompOp::Leq,
    //                             left: Box::new(prob.clone()),
    //                             right: Box::new(rng.clone()),
    //                         };
    //                         guard_exp = guard_exp
    //                             .map(|g| Expression::Bool {
    //                                 op: BoolOp::And,
    //                                 left: Box::new(lower_bound.clone()),
    //                                 right: Box::new(g),
    //                             })
    //                             .or(Some(lower_bound));
    //                     }
    //                     let upper_prob = prob.map_or_else(
    //                         || p.exp.clone(),
    //                         |prob| Expression::IntOp {
    //                             op: parser::IntOp::Plus,
    //                             left: Box::new(prob),
    //                             right: Box::new(p.exp.clone()),
    //                         },
    //                     );
    //                     let upper_bound = Expression::NumComp {
    //                         op: NumCompOp::Less,
    //                         left: Box::new(rng.clone()),
    //                         right: Box::new(upper_prob.clone()),
    //                     };
    //                     guard_exp = guard_exp
    //                         .map(|g| Expression::Bool {
    //                             op: BoolOp::And,
    //                             left: Box::new(upper_bound.clone()),
    //                             right: Box::new(g),
    //                         })
    //                         .or(Some(upper_bound));
    //                     // Update accumulated probability
    //                     prob = Some(upper_prob);
    //                 } else if let Some(ref prob) = prob {
    //                     // Last probability could be left implicit
    //                     let lower_bound = Expression::NumComp {
    //                         op: NumCompOp::Leq,
    //                         left: Box::new(prob.clone()),
    //                         right: Box::new(rng.clone()),
    //                     };
    //                     guard_exp = guard_exp
    //                         .map(|g| Expression::Bool {
    //                             op: BoolOp::And,
    //                             left: Box::new(lower_bound.clone()),
    //                             right: Box::new(g),
    //                         })
    //                         .or(Some(lower_bound));
    //                     // Need to remember this had a probability
    //                     dest.probability = Some(parser::Probability {
    //                         exp: Expression::IntOp {
    //                             op: parser::IntOp::Minus,
    //                             left: Box::new(Expression::ConstantValue(
    //                                 parser::ConstantValue::NumberReal(1.),
    //                             )),
    //                             right: Box::new(prob.clone()),
    //                         },
    //                         _comment: String::new(),
    //                     });
    //                 }

    //                 // actions need new unique names (tracking when new name is not necessary is difficult because of transient variables)
    //                 let action = edge_action.clone() + Self::GEN + &idx.to_string();
    //                 idx += 1;

    //                 new_edges.push(Edge {
    //                     location: edge.location.clone(),
    //                     action: Some(action.clone()),
    //                     guard: guard_exp.map(|exp| Guard {
    //                         exp,
    //                         _comment: String::new(),
    //                     }),
    //                     destinations: vec![dest.clone()],
    //                     _comment: String::new(),
    //                 });

    //                 // Update syncs with new action (has to synchronise like original one)
    //                 for e_idx in (0..jani_model.system.elements.len()).filter(|e_idx| {
    //                     jani_model.system.elements[*e_idx].automaton == automaton.name
    //                 }) {
    //                     // add new syncs for newly generated action
    //                     let to_add = jani_model
    //                         .system
    //                         .syncs
    //                         .iter()
    //                         .filter(|sync| {
    //                             sync.synchronise[e_idx]
    //                                 .as_ref()
    //                                 .is_some_and(|a| *a == edge_action)
    //                         })
    //                         .map(|sync| {
    //                             let mut synchronise = sync.synchronise.clone();
    //                             let _ = synchronise[e_idx].insert(action.clone());
    //                             // Generate new unique result action
    //                             let result = sync.result.clone().unwrap_or_default()
    //                                 + Self::GEN
    //                                 + &idx.to_string();
    //                             idx += 1;
    //                             Sync {
    //                                 synchronise,
    //                                 result: Some(result),
    //                                 _comment: String::new(),
    //                             }
    //                         })
    //                         .collect::<Vec<_>>();
    //                     jani_model.system.syncs.extend(to_add);

    //                     // If original action did not appear in syncs it means that it does not sync between automata.
    //                     // We still want to keep track of it explicitly.
    //                     if jani_model.system.syncs.iter().all(|sync| {
    //                         sync.synchronise[e_idx]
    //                             .as_ref()
    //                             .is_none_or(|a| *a != edge_action)
    //                     }) {
    //                         let mut synchronise = vec![None; jani_model.system.elements.len()];
    //                         synchronise[e_idx] = Some(action.clone());
    //                         // ensure result is unique
    //                         let result = action.clone() + Self::GEN + &idx.to_string();
    //                         idx += 1;
    //                         jani_model.system.syncs.push(Sync {
    //                             synchronise,
    //                             result: Some(result),
    //                             _comment: String::new(),
    //                         });
    //                     }
    //                 }
    //             }
    //         }
    //         // Keep only syncs that are actually used
    //         jani_model.system.syncs.retain(|sync| {
    //             jani_model
    //                 .system
    //                 .elements
    //                 .iter()
    //                 .enumerate()
    //                 .filter(|(_, e)| e.automaton == automaton.name)
    //                 .all(|(e_idx, _)| {
    //                     sync.synchronise[e_idx].as_ref().is_none_or(|a| {
    //                         new_edges
    //                             .iter()
    //                             .any(|edge| edge.action.as_ref().unwrap() == a)
    //                     })
    //                 })
    //         });
    //         // Replace edges with new ones
    //         automaton.edges = new_edges;
    //     }
    //     Ok(())
    // }

    // fn add_global_var(
    //     &mut self,
    //     cs: &mut ChannelSystemBuilder,
    //     pg_id: PgId,
    //     var: &VariableDeclaration,
    // ) -> anyhow::Result<()> {
    //     // TODO WARN FIXME: in JANI initial values are random?
    //     let var_type = (&var.r#type).try_into().expect("convert type");
    //     let init = var
    //         .initial_value
    //         .as_ref()
    //         .and_then(|expr| {
    //             self.build_expression(expr, Some(var_type), &HashMap::new(), None)
    //                 .ok()
    //         })
    //         .unwrap_or_else(|| CsExpression::from(var_type.default_value()));
    //     let val = init.eval_constant()?;
    //     let t = val.r#type();
    //     let var_id = cs.new_var(pg_id, val)?;
    //     self.global_vars.insert(var.name.clone(), (var_id, val, t));
    //     self.global_state_vec.push(var.name.clone());
    //     Ok(())
    // }

    fn add_global_constant(&mut self, c: &ConstantDeclaration) -> anyhow::Result<()> {
        // TODO WARN FIXME: in JANI initial values are random?
        let c_type = (&c.r#type).try_into().expect("convert type");
        let val = c
            .value
            .as_ref()
            .and_then(|expr| {
                self.build_expression(expr, Some(c_type), &HashMap::new(), None)
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
            .and_then(|expr| {
                self.build_expression(expr, Some(var_type), local_vars, None)
                    .ok()
            })
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

    // fn build_automaton(
    //     &mut self,
    //     jani_model: &Model,
    //     pgb: &mut ProgramGraphBuilder,
    //     automaton: &Automaton,
    //     e_idx: usize,
    // ) -> anyhow::Result<()> {
    //     // Initialize RNG
    //     let rng = pgb.new_var(Val::from(0.)).expect("new var");
    //     automaton
    //         .edges
    //         .iter()
    //         .filter(|edge| edge.location.as_str() == Self::INITIAL)
    //         .for_each(|edge| {
    //             // For all edges starting from the unique initial locations,
    //             // get the corresponding action and add RNG assignment as an effect.
    //             let system_action = jani_model
    //                 .system
    //                 .syncs
    //                 .iter()
    //                 .find(|sync| {
    //                     sync.synchronise[e_idx]
    //                         .as_ref()
    //                         .is_some_and(|a| a == edge.action.as_ref().expect("init action"))
    //                 })
    //                 .expect("sync")
    //                 .result
    //                 .as_ref()
    //                 .expect("no silent result");
    //             let init_action = *self.system_actions.get(system_action).expect("exist");
    //             pgb.add_effect(
    //                 init_action,
    //                 rng,
    //                 PgExpression::Float(FloatExpr::Rand(Box::new((
    //                     FloatExpr::from(0.),
    //                     FloatExpr::from(1.),
    //                 )))),
    //             )
    //             .expect("add effect");
    //         });

    //     // Add locations
    //     let mut locations: HashMap<String, scan_core::program_graph::Location> = HashMap::new();
    //     for location in &automaton.locations {
    //         self.build_location(jani_model, pgb, location, e_idx, &mut locations)
    //             .with_context(|| format!("failed building location: {}", &location.name))?;
    //     }

    //     // Add local variables
    //     let mut local_vars: HashMap<String, (Var, Val, Type)> = HashMap::new();
    //     automaton
    //         .variables
    //         .iter()
    //         .try_for_each(|var| self.add_local_var(pgb, var, &mut local_vars))
    //         .context("failed adding local variables")?;

    //     // Add edges
    //     let mut rng_actions = HashSet::new();
    //     for (n_edge, edge) in automaton.edges.iter().enumerate() {
    //         self.build_edge(
    //             jani_model,
    //             automaton,
    //             pgb,
    //             edge,
    //             e_idx,
    //             &local_vars,
    //             &locations,
    //             &mut rng_actions,
    //             rng,
    //         )
    //         .with_context(|| {
    //             format!(
    //                 "failed building {}-th edge for action {}",
    //                 n_edge + 1,
    //                 edge.action.clone().unwrap_or(String::from("silent"))
    //             )
    //         })?;
    //     }
    //     Ok(())
    // }

    // fn build_location(
    //     &mut self,
    //     jani_model: &Model,
    //     pgb: &mut ProgramGraphBuilder,
    //     location: &Location,
    //     e_idx: usize,
    //     locations: &mut HashMap<String, scan_core::program_graph::Location>,
    // ) -> anyhow::Result<()> {
    //     let loc = pgb.new_location();
    //     // Initial location has to be the start of the new process
    //     if location.name.as_str() == Self::INITIAL {
    //         pgb.new_process(loc).expect("new process");
    //     }
    //     assert!(locations.insert(location.name.clone(), loc).is_none());
    //     // For every action that is **NOT** synchronised on this automaton,
    //     // allow action with no change in state.
    //     jani_model
    //         .system
    //         .syncs
    //         .iter()
    //         .filter(|sync| sync.synchronise[e_idx].is_none())
    //         .for_each(|sync| {
    //             let result = sync.result.as_ref().expect("result must have name");
    //             let action = self.system_actions.get(result).expect("system action");
    //             pgb.add_transition(loc, *action, loc, None).unwrap();
    //         });
    //     Ok(())
    // }

    // fn build_edge(
    //     &mut self,
    //     jani_model: &Model,
    //     automaton: &Automaton,
    //     pgb: &mut ProgramGraphBuilder,
    //     edge: &Edge,
    //     e_idx: usize,
    //     local_vars: &HashMap<String, (Var, Val, Type)>,
    //     locations: &HashMap<String, scan_core::program_graph::Location>,
    //     rng_actions: &mut HashSet<Action>,
    //     rng: Var,
    // ) -> anyhow::Result<()> {
    //     let pre = locations.get(&edge.location).ok_or(anyhow!(
    //         "pre-transition location {} not found",
    //         edge.location
    //     ))?;
    //     let guard = edge
    //         .guard
    //         .as_ref()
    //         .map(|guard| self.build_expression(&guard.exp,None, local_vars, Some(rng)))
    //         .transpose()
    //         .with_context(|| {
    //             format!(
    //                 "failed to build guard with expression {:?}",
    //                 edge.guard.as_ref().map(|g| &g.exp)
    //             )
    //         })?;
    //     let guard = guard
    //         .map(|guard| {
    //             if let PgExpression::Boolean(bool_expr) = guard {
    //                 Ok(bool_expr)
    //             } else {
    //                 bail!("guard is not a boolean expression")
    //             }
    //         })
    //         .transpose()?;
    //     // There must be only one destination per edge!
    //     if let [dest] = edge.destinations.as_slice() {
    //         let post = locations.get(&dest.location).ok_or(anyhow!(
    //             "post-transition location {} not found",
    //             dest.location
    //         ))?;
    //         jani_model
    //             .system
    //             .syncs
    //             .iter()
    //             .filter(|sync| {
    //                 sync.synchronise[e_idx].as_ref().is_some_and(|sync_action| {
    //                     *edge.action.as_ref().expect("no silent action") == *sync_action
    //                 })
    //             })
    //             .try_for_each(|sync| {
    //                 let result = sync.result.as_ref().expect("no silent actions generated");
    //                 let action = self.system_actions.get(result).unwrap();
    //                 // checks to do this only once per action
    //                 if dest.probability.is_some() && !rng_actions.contains(action) {
    //                     pgb.add_effect(
    //                         *action,
    //                         rng,
    //                         PgExpression::Float(FloatExpr::Rand(Box::new((
    //                             FloatExpr::from(0.),
    //                             FloatExpr::from(1.),
    //                         )))),
    //                     )
    //                     .expect("effect");
    //                     rng_actions.insert(*action);
    //                 }
    //                 // Set transient variables' values as their initial values before transition from pre location
    //                 for transient_value in automaton
    //                     .locations
    //                     .iter()
    //                     .find(|loc| loc.name == edge.location)
    //                     .map(|loc| &loc.transient_values)
    //                     .ok_or(anyhow!(
    //                         "post-transition location {} not found",
    //                         edge.location
    //                     ))?
    //                 {
    //                     let r#ref = &transient_value.r#ref;
    //                     let (var, val, _) = local_vars
    //                         .get(r#ref)
    //                         .or_else(|| self.global_vars.get(r#ref))
    //                         .ok_or(anyhow!("variable {} not found", r#ref))?;
    //                     let expr = scan_core::Expression::from(*val);
    //                     pgb.add_effect(*action, *var, expr).context(
    //                         "failed setting transient variable {r#ref} to initial value",
    //                     )?;
    //                 }
    //                 // Apply assignments
    //                 for (n, assignment) in dest.assignments.iter().enumerate() {
    //                     let (var, ..) = local_vars
    //                         .get(&assignment.r#ref)
    //                         .or_else(|| self.global_vars.get(&assignment.r#ref))
    //                         .ok_or_else(|| anyhow!("unknown id `{}`", &assignment.r#ref))?;
    //                     let expr = self
    //                         .build_expression(&assignment.value,None, local_vars, Some(rng))
    //                         .context("failed building expression")?;
    //                     pgb.add_effect(*action, *var, expr).with_context(|| {
    //                         format!("failed adding {}-th assignment to action", n + 1)
    //                     })?;
    //                 }
    //                 // Set transient variables' values for destination location
    //                 for transient_value in automaton
    //                     .locations
    //                     .iter()
    //                     .find(|loc| loc.name == dest.location)
    //                     .map(|loc| &loc.transient_values)
    //                     .ok_or(anyhow!(
    //                         "post-transition location {} not found",
    //                         dest.location
    //                     ))?
    //                 {
    //                     let r#ref = &transient_value.r#ref;
    //                     let (var, ..) = local_vars
    //                         .get(r#ref)
    //                         .or_else(|| self.global_vars.get(r#ref))
    //                         .ok_or(anyhow!("variable {} not found", r#ref))?;
    //                     let expr =
    //                         self.build_expression(&transient_value.value,None, local_vars, Some(rng))?;
    //                     pgb.add_effect(*action, *var, expr).context(
    //                         "failed setting transient variable {r#ref} to transient value",
    //                     )?;
    //                 }
    //                 pgb.add_transition(*pre, *action, *post, guard.clone())
    //                     .context("failed adding transition")
    //             })?;
    //     } else {
    //         panic!("edges should be normalized");
    //     }
    //     Ok(())
    // }

    fn build_expression(
        &self,
        expr: &Expression,
        type_hint: Option<Type>,
        local_vars: &HashMap<String, (Var, Val, Type)>,
        rng: Option<Var>,
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
            Expression::Identifier(id) if id == Self::RNG => rng
                .ok_or_else(|| anyhow!("rng not available"))
                .map(|rng| CsExpression::Float(FloatExpr::Var(rng))),
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
                let r#if = self.build_expression(r#if, Some(Type::Boolean), local_vars, rng)?;
                let then = self.build_expression(then, type_hint, local_vars, rng)?;
                let r#else = self.build_expression(r#else, type_hint, local_vars, rng)?;
                match op {
                    parser::IteOp::Ite => r#if.ite(then, r#else).map_err(anyhow::Error::from),
                }
            }
            Expression::Bool { op, left, right } => {
                let left = self.build_expression(left, Some(Type::Boolean), local_vars, rng)?;
                let right = self.build_expression(right, Some(Type::Boolean), local_vars, rng)?;
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
                let exp = self.build_expression(exp, Some(Type::Boolean), local_vars, rng)?;
                match op {
                    parser::NegOp::Neg => (!exp).map_err(|err| err.into()),
                }
            }
            Expression::EqComp { op, left, right } => {
                let left = self.build_expression(left, None, local_vars, rng)?;
                let right = self.build_expression(right, None, local_vars, rng)?;
                match op {
                    parser::EqCompOp::Eq => CsExpression::equal_to(left, right),
                    parser::EqCompOp::Neq => CsExpression::equal_to(left, right).map(|expr| !expr),
                }
                .map(CsExpression::Boolean)
                .map_err(anyhow::Error::from)
            }
            Expression::NumComp { op, left, right } => {
                let left = self.build_expression(left, None, local_vars, rng)?;
                let right = self.build_expression(right, None, local_vars, rng)?;

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
                let left = self.build_expression(left, type_hint, local_vars, rng)?;
                let right = self.build_expression(right, type_hint, local_vars, rng)?;
                match op {
                    parser::IntOp::Plus => left + right,
                    parser::IntOp::Minus => left + (-right)?,
                    parser::IntOp::Mult => left * right,
                    parser::IntOp::IntDiv => left / right,
                }
                .map_err(anyhow::Error::from)
            }
            Expression::RealOp { op, left, right } => {
                let left = self.build_expression(left, Some(Type::Float), local_vars, rng)?;
                let right = self.build_expression(right, Some(Type::Float), local_vars, rng)?;
                match op {
                    parser::RealOp::Div => left / right,
                    parser::RealOp::Pow => todo!(),
                    parser::RealOp::Log => todo!(),
                }
                .map_err(anyhow::Error::from)
            }
            Expression::Real2IntOp { op, exp } => {
                let _exp = self.build_expression(exp, None, local_vars, rng)?;
                if matches!(_exp.r#type(), Type::Float) {
                    match op {
                        parser::Real2IntOp::Floor => todo!(),
                        parser::Real2IntOp::Ceil => todo!(),
                    }
                } else {
                    bail!(TypeError::TypeMismatch)
                }
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
                    parser::IntOp::IntDiv => left / right,
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
        }
    }
}
