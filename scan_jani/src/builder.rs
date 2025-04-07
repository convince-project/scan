use super::Model;
use crate::{
    parser::{Automaton, Edge, Guard, Location},
    Sync,
};
use anyhow::{anyhow, Context};
use rand::Rng;
use scan_core::{
    channel_system::{self, ChannelSystemBuilder, PgId},
    CsModel, CsModelBuilder,
};
use std::collections::HashMap;

#[derive(Clone)]
pub struct JaniModelData {}

pub(crate) fn build<R: Rng + 'static>(
    mut jani_model: Model,
    rng: R,
) -> anyhow::Result<(CsModel<R>, JaniModelData)> {
    let mut builder = JaniBuilder::default();
    dup_actions(&mut jani_model);
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
fn dup_actions(jani_model: &mut Model) {
    // index is global so there is no risk of name-clash
    let mut idx = 0;
    for automaton in &mut jani_model.automata {
        let mut new_edges = Vec::new();
        for edge in &mut automaton.edges {
            for dest in &mut edge.destinations {
                // If edge has assignments, create new action
                let action = if dest.assignments.is_empty() {
                    // Can be silent action
                    edge.action.to_owned()
                } else {
                    let new_action =
                        edge.action.clone().unwrap_or_default() + "__auto_gen__" + &idx.to_string();
                    idx += 1;
                    Some(new_action)
                };
                let new_edge = Edge {
                    location: edge.location.clone(),
                    action: action.clone(),
                    guard: edge.guard.as_ref().map(|guard| Guard {
                        exp: guard.exp.clone(),
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
    // Maps an action of the system and an automaton's name into the corresponding automaton's action
    // Reconstructed from model.system
    // automata_actions: HashMap<String, Vec<(String, Option<String>)>>,
    // system_epsilon: Vec<(String, Option<String>)>,
    // sync_actions: Vec<channel_system::Action>,
}

impl JaniBuilder {
    pub(crate) fn build<R: Rng + 'static>(
        &mut self,
        jani_model: &Model,
        rng: R,
    ) -> anyhow::Result<CsModel<R>> {
        let mut csb = ChannelSystemBuilder::new_with_rng(rng);
        let pg_id = csb.new_program_graph();

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

        // Build system composition
        // self.build_system(&mut csb, pg_id, &jani_model.system)?;

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
        let cs_model = cs_model_builder.build();
        Ok(cs_model)
    }

    fn data(self) -> JaniModelData {
        JaniModelData {}
    }

    // fn build_system<R: Rng + 'static>(
    //     &mut self,
    //     csb: &mut ChannelSystemBuilder<R>,
    //     pg_id: PgId,
    //     system: &Composition,
    // ) -> anyhow::Result<()> {
    //     self.sync_actions = system
    //         .syncs
    //         .iter()
    //         .map(|_| csb.new_action(pg_id).unwrap())
    //         .collect();
    //     // for sync in system.syncs.iter() {
    //     //     let elements = system.elements.iter().map(|e| &e.automaton);
    //     //     let synchronise = sync.synchronise.iter();
    //     //     if let Some(system_action) = &sync.result {
    //     //         if self.automata_actions.contains_key(system_action) {
    //     //             bail!("action {system_action} listed multiple times");
    //     //         }
    //     //         let prev = self.automata_actions.insert(
    //     //             system_action.clone(),
    //     //             elements
    //     //                 .cloned()
    //     //                 .zip(synchronise.cloned())
    //     //                 .collect::<Vec<_>>(),
    //     //         );
    //     //         assert!(prev.is_none());
    //     //     } else {
    //     //         if !self.system_epsilon.is_empty() {
    //     //             bail!("silent action listed multiple times");
    //     //         }
    //     //         self.system_epsilon = elements
    //     //             .cloned()
    //     //             .zip(synchronise.cloned())
    //     //             .collect::<Vec<_>>();
    //     //     }
    //     // }
    //     Ok(())
    // }

    fn build_automaton<R: Rng + 'static>(
        &mut self,
        jani_model: &Model,
        csb: &mut ChannelSystemBuilder<R>,
        pg_id: PgId,
        automaton: &Automaton,
        e_idx: usize,
    ) -> anyhow::Result<()> {
        // Add locations
        for location in &automaton.locations {
            self.build_location(jani_model, csb, pg_id, location, e_idx)?;
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
            self.build_edge(jani_model, csb, pg_id, edge, e_idx)
                .context("failed building edge")?;
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
    ) -> anyhow::Result<()> {
        let pre = *self.cs_locations.get(&edge.location).ok_or(anyhow!(
            "pre-transition location {} not found",
            edge.location
        ))?;
        // There must be only one destination per edge!
        for dest in &edge.destinations {
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
                    let action_id = self.system_actions.get(action).unwrap();
                    csb.add_transition(pg_id, pre, *action_id, post, None)
                        .unwrap();
                } else {
                    csb.add_autonomous_transition(pg_id, pre, post, None)
                        .unwrap();
                }
            }
        }
        Ok(())
    }
}
