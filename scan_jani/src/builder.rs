use super::Model;
use crate::parser::{
    self, Automaton, BoolOp, ConstantDeclaration, Edge, Expression, Guard, Location, NumCompOp,
    Sync, VariableDeclaration,
};
use anyhow::{Context, anyhow, bail};
use rand::{Rng, SeedableRng};
use scan_core::{
    CsModel, CsModelBuilder, Integer, Type, TypeError, Val,
    channel_system::{self, ChannelSystemBuilder, CsExpression, PgId, Var},
};
use std::{collections::HashMap, f64, ops::Not};

#[derive(Clone)]
pub struct JaniModelData {}

pub(crate) fn build<R: Rng + SeedableRng + 'static>(
    mut jani_model: Model,
    rng: R,
) -> anyhow::Result<(CsModel<R>, JaniModelData)> {
    let mut builder = JaniBuilder::default();
    normalize(&mut jani_model);
    let cs_model = builder.build(&jani_model, rng)?;
    let data = builder.data();
    Ok((cs_model, data))
}

// An action in JANI doesn not carry effects,
// so we need to duplicate actions until each one has unique effects.
// The modified model is such that:
//
// - Every action has a unique set of assignments.
// - Every edge has a unique location.
// - Syncs are updated with new actions.
fn normalize(jani_model: &mut Model) {
    // index is global so there is no risk of name-clash
    let mut idx = 0;
    for automaton in &mut jani_model.automata {
        let mut new_edges = Vec::new();
        for edge in &mut automaton.edges {
            let mut prob = Expression::ConstantValue(parser::ConstantValue::NumberReal(0f64));
            for dest in &mut edge.destinations {
                // If edge has assignments, create new action
                let action = if dest.assignments.is_empty() {
                    if dest.probability.is_some() && edge.action.is_none() {
                        // Cannot be silent action because needs to randomize
                        edge.action = Some(String::from("__auto_gen__") + &idx.to_string());
                        idx += 1;
                    }
                    // Can be silent action or whatever
                    edge.action.to_owned()
                } else {
                    let new_action =
                        edge.action.clone().unwrap_or_default() + "__auto_gen__" + &idx.to_string();
                    idx += 1;
                    Some(new_action)
                };
                // Add probability to guard
                let mut guard_exp = edge.guard.as_ref().map(|guard| guard.exp.clone());
                if let Some(p) = dest.probability.as_ref() {
                    let lower_bound = Expression::NumComp {
                        op: NumCompOp::Less,
                        left: Box::new(Expression::IntOp {
                            op: parser::IntOp::Mult,
                            left: Box::new(Expression::ConstantValue(
                                parser::ConstantValue::NumberReal(100f64),
                            )),
                            right: Box::new(prob.clone()),
                        }),
                        right: Box::new(Expression::Identifier(String::from("__rng__"))),
                    };
                    prob = Expression::IntOp {
                        op: parser::IntOp::Plus,
                        left: Box::new(prob.clone()),
                        right: Box::new(p.exp.clone()),
                    };
                    let upper_bound = Expression::NumComp {
                        op: NumCompOp::Less,
                        left: Box::new(Expression::Identifier(String::from("__rng__"))),
                        right: Box::new(Expression::IntOp {
                            op: parser::IntOp::Mult,
                            left: Box::new(Expression::ConstantValue(
                                parser::ConstantValue::NumberReal(100f64),
                            )),
                            right: Box::new(prob.clone()),
                        }),
                    };
                    guard_exp = guard_exp.map_or(
                        Some(Expression::Bool {
                            op: BoolOp::And,
                            left: Box::new(lower_bound.clone()),
                            right: Box::new(upper_bound.clone()),
                        }),
                        |g| {
                            Some(Expression::Bool {
                                op: BoolOp::And,
                                left: Box::new(Expression::Bool {
                                    op: BoolOp::And,
                                    left: Box::new(lower_bound),
                                    right: Box::new(upper_bound),
                                }),
                                right: Box::new(g),
                            })
                        },
                    );
                }
                let new_edge = Edge {
                    location: edge.location.clone(),
                    action: action.clone(),
                    guard: guard_exp.map(|exp| Guard {
                        exp,
                        comment: String::new(),
                    }),
                    destinations: vec![dest.clone()],
                    comment: String::new(),
                };
                new_edges.push(new_edge);

                // Update syncs with new action (has to synchronise like original one)
                // NOTE: you cannot synchronise the silent action!
                if let Some(ref orig_action) = edge.action {
                    for (e, _) in jani_model
                        .system
                        .elements
                        .iter()
                        .enumerate()
                        .filter(|(_, e)| e.automaton == automaton.name)
                    {
                        let mut to_add = Vec::new();
                        for sync in &jani_model.system.syncs {
                            if sync.synchronise[e] == Some(orig_action.clone()) {
                                let mut synchronise = sync.synchronise.clone();
                                synchronise[e] = action.clone();
                                // Generate new unique result action
                                let new_result = String::from("__auto_gen__") + &idx.to_string();
                                idx += 1;
                                to_add.push(Sync {
                                    synchronise,
                                    result: Some(new_result),
                                    comment: String::new(),
                                });
                            }
                        }
                        // If original action did not appear in syncs it means that it does not sync between automata.
                        // We still want to keep track of it esplicitely.
                        if to_add.is_empty() {
                            let mut synchronise = vec![None; jani_model.system.elements.len()];
                            synchronise[e] = action.clone();
                            to_add.push(Sync {
                                synchronise,
                                // By taking `action` as result we ensure name is unique
                                result: action.clone(),
                                comment: String::new(),
                            });
                        }
                        // Add generated syncs
                        jani_model.system.syncs.extend(to_add);
                    }
                }
            }
        }
        // Replace edges with new ones
        automaton.edges = new_edges;
    }
}

#[derive(Default)]
struct JaniBuilder {
    cs_locations: HashMap<String, channel_system::Location>,
    system_actions: HashMap<String, channel_system::Action>,
    global_vars: HashMap<String, (Var, Type)>,
    global_constants: HashMap<String, Val>,
    // Maps an action of the system and an automaton's name into the corresponding automaton's action
    // Reconstructed from model.system
    // automata_actions: HashMap<String, Vec<(String, Option<String>)>>,
    // system_epsilon: Vec<(String, Option<String>)>,
    // sync_actions: Vec<channel_system::Action>,
}

impl JaniBuilder {
    pub(crate) fn build<R: Rng + SeedableRng + 'static>(
        &mut self,
        jani_model: &Model,
        rng: R,
    ) -> anyhow::Result<CsModel<R>> {
        let mut csb = ChannelSystemBuilder::new_with_rng(rng);
        let pg_id = csb.new_program_graph();
        let rng = csb
            .new_var(pg_id, CsExpression::RandInt(0, Integer::MAX))
            .expect("variable");
        self.global_vars
            .insert(String::from("__rng__"), (rng, Type::Integer));

        jani_model
            .system
            .syncs
            .iter()
            .flat_map(|sync| &sync.result)
            .for_each(|action| {
                if !self.system_actions.contains_key(action) {
                    let action_id = csb.new_action(pg_id).expect("new action");
                    let prev = self.system_actions.insert(action.clone(), action_id);
                    assert!(prev.is_none(), "checked by above if condition");
                }
            });

        for action in self.system_actions.values() {
            csb.add_effect(pg_id, *action, rng, CsExpression::RandInt(0, 100))
                .expect("add randomizing effect");
        }

        jani_model
            .variables
            .iter()
            .try_for_each(|var| self.add_global_var(&mut csb, pg_id, var))?;
        jani_model
            .constants
            .iter()
            .try_for_each(|c| self.add_global_constant(c))?;

        for (e_idx, element) in jani_model.system.elements.iter().enumerate() {
            let id = &element.automaton;
            let automaton = jani_model
                .automata
                .iter()
                .find(|a| a.name == *id)
                .ok_or(anyhow!("element '{id}' is not a known automaton"))?;
            self.build_automaton(jani_model, &mut csb, pg_id, automaton, e_idx)
                .with_context(|| format!("failed to build automaton '{id}'"))?;
        }

        // Finalize, build and return everything
        let cs = csb.build();
        let cs_model_builder = CsModelBuilder::new(cs);

        // Add properties
        for property in jani_model.properties.iter() {
            let exp = self.build_expression(&property.expression, &HashMap::new())?;
            todo!();
        }

        let cs_model = cs_model_builder.build();
        Ok(cs_model)
    }

    fn add_global_var<R: Rng + 'static>(
        &mut self,
        // jani_model: &Model,
        csb: &mut ChannelSystemBuilder<R>,
        pg_id: PgId,
        var: &VariableDeclaration,
    ) -> anyhow::Result<()> {
        // TODO WARN FIXME: in JANI initial values are random?
        let init = var
            .initial_value
            .as_ref()
            .and_then(|expr| self.build_expression(expr, &HashMap::new()).ok())
            .unwrap_or_else(|| {
                CsExpression::Const(match &var.r#type {
                    parser::Type::Basic(basic_type) => match basic_type {
                        parser::BasicType::Bool => scan_core::Val::Boolean(false),
                        parser::BasicType::Int => scan_core::Val::Integer(0),
                        parser::BasicType::Real => scan_core::Val::Float(0f64),
                    },
                    parser::Type::Bounded(_bounded_type) => todo!(),
                    parser::Type::Clock(_) => todo!(),
                    parser::Type::Continuous(_) => todo!(),
                })
            });
        let t = init.r#type()?;
        let var_id = csb.new_var(pg_id, init)?;
        self.global_vars.insert(var.name.clone(), (var_id, t));
        Ok(())
    }

    fn add_global_constant(&mut self, c: &ConstantDeclaration) -> anyhow::Result<()> {
        // TODO WARN FIXME: in JANI initial values are random?
        let val = c
            .value
            .as_ref()
            .and_then(|expr| self.build_expression(expr, &HashMap::new()).ok())
            .unwrap_or_else(|| {
                CsExpression::Const(match &c.r#type {
                    parser::Type::Basic(basic_type) => match basic_type {
                        parser::BasicType::Bool => scan_core::Val::Boolean(false),
                        parser::BasicType::Int => scan_core::Val::Integer(0),
                        parser::BasicType::Real => scan_core::Val::Float(0f64),
                    },
                    parser::Type::Bounded(_bounded_type) => todo!(),
                    parser::Type::Clock(_) => todo!(),
                    parser::Type::Continuous(_) => todo!(),
                })
            })
            .eval_constant()?;
        self.global_constants.insert(c.name.clone(), val);
        Ok(())
    }

    fn add_local_var<R: Rng + 'static>(
        &self,
        csb: &mut ChannelSystemBuilder<R>,
        pg_id: PgId,
        var: &VariableDeclaration,
        local_vars: &mut HashMap<String, (Var, Type)>,
    ) -> anyhow::Result<()> {
        // TODO WARN FIXME: in JANI initial values are random?
        let init = var
            .initial_value
            .as_ref()
            .and_then(|expr| self.build_expression(expr, local_vars).ok())
            .unwrap_or_else(|| {
                CsExpression::Const(match &var.r#type {
                    parser::Type::Basic(basic_type) => match basic_type {
                        parser::BasicType::Bool => scan_core::Val::Boolean(false),
                        parser::BasicType::Int => scan_core::Val::Integer(0),
                        parser::BasicType::Real => scan_core::Val::Float(0f64),
                    },
                    parser::Type::Bounded(_bounded_type) => todo!(),
                    parser::Type::Clock(_) => todo!(),
                    parser::Type::Continuous(_) => todo!(),
                })
            });
        let t = init.r#type()?;
        let var_id = csb.new_var(pg_id, init)?;
        local_vars.insert(var.name.clone(), (var_id, t));
        Ok(())
    }

    fn data(self) -> JaniModelData {
        JaniModelData {}
    }

    fn build_automaton<R: Rng + 'static>(
        &mut self,
        jani_model: &Model,
        csb: &mut ChannelSystemBuilder<R>,
        pg_id: PgId,
        automaton: &Automaton,
        e_idx: usize,
    ) -> anyhow::Result<()> {
        let mut local_vars: HashMap<String, (Var, Type)> = HashMap::new();
        automaton
            .variables
            .iter()
            .try_for_each(|var| self.add_local_var(csb, pg_id, var, &mut local_vars))
            .context("failed adding local variables")?;
        // Add locations
        for location in &automaton.locations {
            self.build_location(jani_model, csb, pg_id, location, e_idx)
                .with_context(|| format!("failed building location: {}", &location.name))?;
        }
        // Connect initial location of PG with initial location(s) of the JANI model
        let cs_initial = csb
            .new_initial_location(pg_id)
            .expect("pg initial location");
        for initial in &automaton.initial_locations {
            let jani_initial = *self
                .cs_locations
                .get(initial)
                .ok_or_else(|| anyhow!("missing initial location {}", initial))?;
            csb.add_autonomous_transition(pg_id, cs_initial, jani_initial, None)
                .expect("add transition");
        }
        // Add edges
        for edge in &automaton.edges {
            self.build_edge(jani_model, csb, pg_id, edge, e_idx, &local_vars)
                .with_context(|| {
                    format!(
                        "failed building edge for action: {}",
                        edge.action.clone().unwrap_or(String::from("`silent`"))
                    )
                })?;
        }
        Ok(())
    }

    fn build_location<R: Rng + 'static>(
        &mut self,
        jani_model: &Model,
        csb: &mut ChannelSystemBuilder<R>,
        pg_id: PgId,
        location: &Location,
        e_idx: usize,
    ) -> anyhow::Result<()> {
        let loc = csb.new_location(pg_id)?;
        self.cs_locations.insert(location.name.clone(), loc);
        // For every action that is **NOT** synchronised on this automaton,
        // allow action with no change in state.
        for sync in jani_model
            .system
            .syncs
            .iter()
            .filter(|s| s.synchronise[e_idx].is_none())
        {
            if let Some(ref action) = sync.result {
                let action_id = self.system_actions.get(action).unwrap();
                csb.add_transition(pg_id, loc, *action_id, loc, None)
                    .unwrap();
            } else {
                csb.add_autonomous_transition(pg_id, loc, loc, None)
                    .unwrap();
            }
        }
        Ok(())
    }

    fn build_edge<R: Rng + 'static>(
        &mut self,
        jani_model: &Model,
        csb: &mut ChannelSystemBuilder<R>,
        pg_id: PgId,
        edge: &Edge,
        e_idx: usize,
        local_vars: &HashMap<String, (Var, Type)>,
    ) -> anyhow::Result<()> {
        let pre = *self.cs_locations.get(&edge.location).ok_or(anyhow!(
            "pre-transition location {} not found",
            edge.location
        ))?;
        let guard = edge
            .guard
            .as_ref()
            .map(|guard| self.build_expression(&guard.exp, local_vars))
            .transpose()
            .with_context(|| {
                format!(
                    "failed to build guard with expression {:?}",
                    edge.guard.as_ref().map(|g| &g.exp)
                )
            })?;
        // There must be only one destination per edge!
        if let [dest] = edge.destinations.as_slice() {
            let post = &dest.location;
            let post = *self.cs_locations.get(post).ok_or(anyhow!(
                "post-transition location {} not found",
                edge.location
            ))?;
            for sync in jani_model.system.syncs.iter().filter(|s| {
                s.synchronise[e_idx]
                    .as_ref()
                    .is_some_and(|a| edge.action.as_ref().is_some_and(|e| a == e))
            }) {
                if let Some(ref action) = sync.result {
                    let action = self.system_actions.get(action).unwrap();
                    for assignment in dest.assignments.iter() {
                        let (var, _) = local_vars
                            .get(&assignment.r#ref)
                            .or_else(|| self.global_vars.get(&assignment.r#ref))
                            .ok_or_else(|| anyhow!("unknown id `{}`", &assignment.r#ref))?;
                        let expr = self.build_expression(&assignment.value, local_vars)?;
                        csb.add_effect(pg_id, *action, *var, expr)?;
                    }
                    csb.add_transition(pg_id, pre, *action, post, guard.clone())?;
                } else {
                    csb.add_autonomous_transition(pg_id, pre, post, guard.clone())?;
                }
            }
        } else {
            panic!("edges should be normalized");
        }
        Ok(())
    }

    fn build_expression(
        &self,
        expr: &Expression,
        local_vars: &HashMap<String, (Var, Type)>,
    ) -> anyhow::Result<CsExpression> {
        match expr {
            Expression::ConstantValue(constant_value) => match constant_value {
                parser::ConstantValue::Boolean(b) => Ok(CsExpression::from(*b)),
                parser::ConstantValue::Constant(constant) => match constant {
                    parser::Constant::Euler => Ok(CsExpression::from(f64::consts::E)),
                    parser::Constant::Pi => Ok(CsExpression::from(f64::consts::PI)),
                },
                parser::ConstantValue::NumberReal(num) => Ok(CsExpression::from(*num)),
                parser::ConstantValue::NumberInt(num) => Ok(CsExpression::from(*num)),
            },
            Expression::Identifier(id) => local_vars
                .get(id)
                .or_else(|| self.global_vars.get(id))
                .map(|(var, t)| CsExpression::Var(*var, t.clone()))
                .or_else(|| {
                    self.global_constants
                        .get(id)
                        .cloned()
                        .map(CsExpression::Const)
                })
                .ok_or_else(|| anyhow!("unknown id `{id}`")),
            Expression::IfThenElse {
                op,
                r#if,
                then,
                r#else,
            } => {
                let _if = self.build_expression(r#if, local_vars)?;
                let _then = self.build_expression(then, local_vars)?;
                let _else = self.build_expression(r#else, local_vars)?;
                match op {
                    parser::IteOp::Ite => todo!(),
                }
            }
            Expression::Bool { op, left, right } => {
                let left = self.build_expression(left, local_vars)?;
                let right = self.build_expression(right, local_vars)?;
                match op {
                    BoolOp::And => CsExpression::and(vec![left, right]).map_err(|err| err.into()),
                    BoolOp::Or => CsExpression::or(vec![left, right]).map_err(|err| err.into()),
                }
            }
            Expression::Neg { op, exp } => {
                let exp = self.build_expression(exp, local_vars)?;
                match op {
                    parser::NegOp::Neg => CsExpression::not(exp).map_err(|err| err.into()),
                }
            }
            Expression::EqComp { op, left, right } => {
                let left = self.build_expression(left, local_vars)?;
                let right = self.build_expression(right, local_vars)?;
                if left.r#type()? == right.r#type()?
                    || (matches!(left.r#type()?, Type::Integer | Type::Float)
                        && matches!(right.r#type()?, Type::Integer | Type::Float))
                {
                    match op {
                        parser::EqCompOp::Eq => Ok(CsExpression::Equal(Box::new((left, right)))),
                        parser::EqCompOp::Neq => CsExpression::Equal(Box::new((left, right)))
                            .not()
                            .map_err(|err| err.into()),
                    }
                } else {
                    bail!(TypeError::TypeMismatch)
                }
            }
            Expression::NumComp { op, left, right } => {
                let left = self.build_expression(left, local_vars)?;
                let right = self.build_expression(right, local_vars)?;
                if matches!(left.r#type()?, Type::Integer | Type::Float)
                    && matches!(right.r#type()?, Type::Integer | Type::Float)
                {
                    match op {
                        parser::NumCompOp::Less => Ok(CsExpression::Less(Box::new((left, right)))),
                        parser::NumCompOp::Leq => Ok(CsExpression::LessEq(Box::new((left, right)))),
                        parser::NumCompOp::Greater => {
                            Ok(CsExpression::Greater(Box::new((left, right))))
                        }
                        parser::NumCompOp::Geq => {
                            Ok(CsExpression::GreaterEq(Box::new((left, right))))
                        }
                    }
                } else {
                    bail!(TypeError::TypeMismatch)
                }
            }

            Expression::IntOp { op, left, right } => {
                let left = self.build_expression(left, local_vars)?;
                let right = self.build_expression(right, local_vars)?;
                if matches!(left.r#type()?, Type::Integer | Type::Float)
                    && matches!(right.r#type()?, Type::Integer | Type::Float)
                {
                    match op {
                        parser::IntOp::Plus => Ok(CsExpression::Sum(vec![left, right])),
                        parser::IntOp::Minus => Ok(CsExpression::Sum(vec![
                            left,
                            CsExpression::Opposite(Box::new(right)),
                        ])),
                        parser::IntOp::Mult => Ok(CsExpression::Mult(vec![left, right])),
                        parser::IntOp::IntDiv => todo!(),
                    }
                } else {
                    bail!(TypeError::TypeMismatch)
                }
            }
            Expression::RealOp { op, left, right } => {
                let left = self.build_expression(left, local_vars)?;
                let right = self.build_expression(right, local_vars)?;
                if matches!(left.r#type()?, Type::Float) && matches!(right.r#type()?, Type::Float) {
                    match op {
                        parser::RealOp::Div => todo!(),
                        // Ok(CsExpression::Mult(vec![
                        //     left,
                        //     CsExpression::Inv(Box::new(right)),
                        // ])),
                        parser::RealOp::Pow => todo!(),
                        parser::RealOp::Log => todo!(),
                    }
                } else {
                    bail!(TypeError::TypeMismatch)
                }
            }
            Expression::Real2IntOp { op, exp } => {
                let _exp = self.build_expression(exp, local_vars)?;
                if matches!(_exp.r#type()?, Type::Float) {
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
}
