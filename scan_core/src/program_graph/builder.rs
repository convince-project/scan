use std::collections::BTreeMap;

use super::{
    Action, ActionIdx, Clock, EPSILON, Effect, Location, LocationIdx, PgError, PgExpression,
    ProgramGraph, TimeConstraint, Var,
};
use crate::{
    grammar::{BooleanExpr, Type, Val},
    program_graph::PgGuard,
};
use log::info;

pub(crate) type Transition = (Location, Option<BooleanExpr<Var>>, Vec<TimeConstraint>);

type LocationBuilderData = (BTreeMap<Action, Vec<Transition>>, Vec<TimeConstraint>);

/// Defines and builds a PG.
#[derive(Debug, Clone)]
pub struct ProgramGraphBuilder {
    // initial_states: Vec<Location>,
    initial_states: Vec<Location>,
    // Effects are indexed by actions
    effects: Vec<Effect>,
    // Transitions are indexed by locations
    // Time invariants of each location
    locations: Vec<LocationBuilderData>,
    // Local variables with initial value.
    vars: Vec<Val>,
    // Number of clocks
    clocks: u16,
}

impl Default for ProgramGraphBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramGraphBuilder {
    /// Creates a new [`ProgramGraphBuilder`].
    /// At creation, this will only have the initial location with no variables, no actions and no transitions.
    pub fn new() -> Self {
        Self {
            initial_states: Vec::new(),
            effects: Vec::new(),
            vars: Vec::new(),
            locations: Vec::new(),
            clocks: 0,
        }
    }

    // Gets the type of a variable.
    pub(crate) fn var_type(&self, var: Var) -> Result<Type, PgError> {
        self.vars
            .get(var.0 as usize)
            .map(|val| Val::r#type(*val))
            .ok_or(PgError::MissingVar(var))
    }

    /// Adds a new variable with the given initial value (and the inferred type) to the PG.
    ///
    /// ```
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder};
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // Create a new action
    /// let action = pg_builder.new_action();
    ///
    /// // Create a value to assign the expression.
    /// let val = (PgExpression::from(40i64) + PgExpression::from(40i64)).unwrap().eval_constant().unwrap();
    ///
    /// // Create a new variable
    /// let var = pg_builder
    ///     .new_var(val);
    /// ```
    pub fn new_var(&mut self, val: Val) -> Var {
        let idx = self.vars.len();
        self.vars.push(val);
        Var(idx as u16)
    }

    /// Adds a new clock and returns a [`Clock`] id object.
    ///
    /// See also [`crate::channel_system::ChannelSystemBuilder::new_clock`].
    pub fn new_clock(&mut self) -> Clock {
        // We adopt the convention of indexing n clocks from 0 to n-1
        let idx = self.clocks;
        self.clocks += 1;
        Clock(idx)
    }

    /// Adds a new action to the PG.
    ///
    /// ```
    /// # use scan_core::program_graph::{Action, ProgramGraphBuilder};
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // Create a new action
    /// let action: Action = pg_builder.new_action();
    /// ```
    #[inline(always)]
    pub fn new_action(&mut self) -> Action {
        let idx = self.effects.len();
        self.effects.push(Effect::Effects(Vec::new(), Vec::new()));
        Action(idx as ActionIdx)
    }

    /// Associates a clock reset to an action.
    ///
    /// Returns an error if the clock to be reset does not belong to the Program Graph.
    ///
    /// ```
    /// # use scan_core::program_graph::{Clock, ProgramGraphBuilder};
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// # let mut other_pg_builder = ProgramGraphBuilder::new();
    /// let action = pg_builder.new_action();
    /// let clock = other_pg_builder.new_clock();
    /// // Associate action with clock reset
    /// pg_builder
    ///     .add_reset(action, clock)
    ///     .expect_err("the clock does not belong to this PG");
    /// ```
    pub fn add_reset(&mut self, action: Action, clock: Clock) -> Result<(), PgError> {
        if action == EPSILON {
            return Err(PgError::NoEffects);
        }
        if clock.0 >= self.clocks {
            return Err(PgError::MissingClock(clock));
        }
        match self
            .effects
            .get_mut(action.0 as usize)
            .ok_or(PgError::MissingAction(action))?
        {
            Effect::Effects(_, resets) => {
                resets.push(clock);
                Ok(())
            }
            Effect::Send(_) => Err(PgError::EffectOnSend),
            Effect::Receive(_) => Err(PgError::EffectOnReceive),
        }
    }

    /// Adds an effect to the given action.
    /// Requires specifying which variable is assigned the value of which expression whenever the action triggers a transition.
    ///
    /// It fails if the type of the variable and that of the expression do not match.
    ///
    /// ```
    /// # use scan_core::program_graph::{Action, PgExpression, ProgramGraphBuilder, Var};
    /// # use scan_core::Val;
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // Create a new action
    /// let action: Action = pg_builder.new_action();
    ///
    /// // Create a new variable
    /// let var: Var = pg_builder.new_var(Val::from(true)).expect("expression is well-typed");
    ///
    /// // Add an effect to the action
    /// pg_builder
    ///     .add_effect(action, var, PgExpression::from(1i64))
    ///     .expect_err("var is of type bool but expression is of type integer");
    /// pg_builder
    ///     .add_effect(action, var, PgExpression::from(false))
    ///     .expect("var and expression type match");
    /// ```
    pub fn add_effect(
        &mut self,
        action: Action,
        var: Var,
        effect: PgExpression,
    ) -> Result<(), PgError> {
        if action == EPSILON {
            return Err(PgError::NoEffects);
        }
        effect
            .context(&|var| self.vars.get(var.0 as usize).map(|val| Val::r#type(*val)))
            .map_err(PgError::Type)?;
        let var_type = self
            .vars
            .get(var.0 as usize)
            .map(|val| Val::r#type(*val))
            .ok_or_else(|| PgError::MissingVar(var.to_owned()))?;
        if var_type == effect.r#type() {
            match self
                .effects
                .get_mut(action.0 as usize)
                .ok_or(PgError::MissingAction(action))?
            {
                Effect::Effects(effects, _) => {
                    effects.push((var, effect));
                    Ok(())
                }
                Effect::Send(_) => Err(PgError::EffectOnSend),
                Effect::Receive(_) => Err(PgError::EffectOnReceive),
            }
        } else {
            Err(PgError::TypeMismatch)
        }
    }

    pub(crate) fn new_send(&mut self, mut msgs: Vec<PgExpression>) -> Result<Action, PgError> {
        // Check message is well-typed
        msgs.iter()
            .try_for_each(|msg| {
                msg.context(&|var| self.vars.get(var.0 as usize).map(|val| Val::r#type(*val)))
            })
            .map_err(PgError::Type)?;
        // Actions are indexed progressively
        let idx = self.effects.len();
        msgs.shrink_to_fit();
        self.effects.push(Effect::Send(msgs));
        Ok(Action(idx as ActionIdx))
    }

    pub(crate) fn new_receive(&mut self, vars: Vec<Var>) -> Result<Action, PgError> {
        if let Some(var) = vars.iter().find(|var| self.vars.len() as u16 <= var.0) {
            Err(PgError::MissingVar(var.to_owned()))
        } else {
            // Actions are indexed progressively
            let idx = self.effects.len();
            self.effects.push(Effect::Receive(vars));
            Ok(Action(idx as ActionIdx))
        }
    }

    /// Adds a new location to the PG and returns its [`Location`] indexing object.
    #[inline(always)]
    pub fn new_location(&mut self) -> Location {
        self.new_timed_location(Vec::new())
            .expect("new untimed location")
    }

    /// Adds a new location to the PG with the given time invariants,
    /// and returns its [`Location`] indexing object.
    pub fn new_timed_location(
        &mut self,
        mut invariants: Vec<TimeConstraint>,
    ) -> Result<Location, PgError> {
        if let Some((clock, _, _)) = invariants.iter().find(|(c, _, _)| c.0 >= self.clocks) {
            Err(PgError::MissingClock(*clock))
        } else {
            // Locations are indexed progressively
            let idx = self.locations.len();
            invariants.sort_unstable();
            invariants.shrink_to_fit();
            self.locations.push((BTreeMap::new(), invariants));
            Ok(Location(idx as LocationIdx))
        }
    }

    /// Adds a new (synchronous) process to the PG starting from the given [`Location`].
    pub fn new_process(&mut self, location: Location) -> Result<(), PgError> {
        self.locations
            .get(location.0 as usize)
            .ok_or(PgError::MissingLocation(location))?
            .1 // location's time invariants
            .iter()
            .all(|(_, l, u)| {
                // All clocks start at time 0
                l.is_none_or(|l| l == 0) && u.is_none_or(|u| u > 0)
            })
            .then(|| self.initial_states.push(location))
            .ok_or(PgError::Invariant)
    }

    /// Adds a new process starting at a new location to the PG and returns the [`Location`] indexing object.
    #[inline(always)]
    pub fn new_initial_location(&mut self) -> Location {
        self.new_initial_timed_location(Vec::new())
            .expect("new untimed location")
    }

    /// Adds a new process starting at a new location to the PG with the given time invariants,
    /// and returns the [`Location`] indexing object.
    pub fn new_initial_timed_location(
        &mut self,
        mut invariants: Vec<TimeConstraint>,
    ) -> Result<Location, PgError> {
        invariants.sort_unstable();
        invariants.shrink_to_fit();
        let location = self.new_timed_location(invariants)?;
        self.new_process(location)?;
        Ok(location)
    }

    /// Adds a transition to the PG.
    /// Requires specifying:
    ///
    /// - state pre-transition,
    /// - action triggering the transition,
    /// - state post-transition, and
    /// - (optionally) boolean expression guarding the transition.
    ///
    /// Fails if the provided guard is not a boolean expression.
    ///
    /// ```
    /// # use scan_core::program_graph::ProgramGraphBuilder;
    /// # use scan_core::BooleanExpr;
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // The builder is initialized with an initial location
    /// let initial_loc = pg_builder.new_initial_location();
    ///
    /// // Create a new action
    /// let action = pg_builder.new_action();
    ///
    /// // Add a transition
    /// pg_builder
    ///     .add_transition(initial_loc, action, initial_loc, None)
    ///     .expect("this transition can be added");
    /// pg_builder
    ///     .add_transition(initial_loc, action, initial_loc, Some(BooleanExpr::from(false)))
    ///     .expect("this one too");
    /// ```
    #[inline(always)]
    pub fn add_transition(
        &mut self,
        pre: Location,
        action: Action,
        post: Location,
        guard: Option<PgGuard>,
    ) -> Result<(), PgError> {
        self.add_timed_transition(pre, action, post, guard, Vec::new())
    }

    /// Adds a timed transition to the PG under timed constraints.
    /// Requires specifying the same data as [`ProgramGraphBuilder::add_transition`],
    /// plus a slice of time constraints.
    ///
    /// Fails if the provided guard is not a boolean expression.
    ///
    /// ```
    /// # use scan_core::program_graph::ProgramGraphBuilder;
    /// # use scan_core::BooleanExpr;
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // The builder is initialized with an initial location
    /// let initial_loc = pg_builder.new_initial_location();
    ///
    /// // Create a new action
    /// let action = pg_builder.new_action();
    ///
    /// // Add a new clock
    /// let clock = pg_builder.new_clock();
    ///
    /// // Add a timed transition
    /// pg_builder
    ///     .add_timed_transition(initial_loc, action, initial_loc, None, vec![(clock, None, Some(1))])
    ///     .expect("this transition can be added");
    /// pg_builder
    ///     .add_timed_transition(initial_loc, action, initial_loc, Some(BooleanExpr::from(false)), vec![(clock, Some(1), None)])
    ///     .expect("this one too");
    /// ```
    pub fn add_timed_transition(
        &mut self,
        pre: Location,
        action: Action,
        post: Location,
        guard: Option<PgGuard>,
        mut constraints: Vec<TimeConstraint>,
    ) -> Result<(), PgError> {
        // Check 'pre' and 'post' locations exists
        if self.locations.len() as LocationIdx <= pre.0 {
            Err(PgError::MissingLocation(pre))
        } else if self.locations.len() as LocationIdx <= post.0 {
            Err(PgError::MissingLocation(post))
        } else if action != EPSILON && self.effects.len() as ActionIdx <= action.0 {
            // Check 'action' exists
            Err(PgError::MissingAction(action))
        } else if let Some((clock, _, _)) = constraints.iter().find(|(c, _, _)| c.0 >= self.clocks)
        {
            Err(PgError::MissingClock(*clock))
        } else {
            if let Some(ref guard) = guard {
                guard
                    .context(&|var| self.vars.get(var.0 as usize).map(|val| Val::r#type(*val)))
                    .map_err(PgError::Type)?;
            }
            let (transitions, _) = &mut self.locations[pre.0 as usize];
            constraints.sort_unstable();
            constraints.shrink_to_fit();
            let transition = (post, guard, constraints);
            // WARN: Actions have to be inserted in order but insertion has worst-case complexity O(n)
            // so in some cases insertion of all actions could have complexity O(n^2).
            // In practice though this is unlikely to ever be a bottleneck
            match transitions.get_mut(&action) {
                Some(action_transitions) => action_transitions.push(transition),
                None => {
                    let _ = transitions.insert(action, vec![transition]);
                }
            }
            Ok(())
        }
    }

    /// Adds an autonomous transition to the PG, i.e., a transition enabled by the epsilon action.
    /// Requires specifying:
    ///
    /// - state pre-transition,
    /// - state post-transition, and
    /// - (optionally) boolean expression guarding the transition.
    ///
    /// Fails if the provided guard is not a boolean expression.
    ///
    /// ```
    /// # use scan_core::program_graph::ProgramGraphBuilder;
    /// # use scan_core::BooleanExpr;
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // The builder is initialized with an initial location
    /// let initial_loc = pg_builder.new_initial_location();
    ///
    /// // Add a transition
    /// pg_builder
    ///     .add_autonomous_transition(initial_loc, initial_loc, None)
    ///     .expect("this autonomous transition can be added");
    /// pg_builder
    ///     .add_autonomous_transition(initial_loc, initial_loc, Some(BooleanExpr::from(false)))
    ///     .expect("this one too");
    /// ```
    #[inline(always)]
    pub fn add_autonomous_transition(
        &mut self,
        pre: Location,
        post: Location,
        guard: Option<PgGuard>,
    ) -> Result<(), PgError> {
        self.add_transition(pre, EPSILON, post, guard)
    }

    /// Adds an autonomous timed transition to the PG, i.e., a transition enabled by the epsilon action under time constraints.
    /// Requires specifying the same data as [`ProgramGraphBuilder::add_autonomous_transition`],
    /// plus a slice of time constraints.
    ///
    /// Fails if the provided guard is not a boolean expression.
    ///
    /// ```
    /// # use scan_core::program_graph::ProgramGraphBuilder;
    /// # use scan_core::BooleanExpr;
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // The builder is initialized with an initial location
    /// let initial_loc = pg_builder.new_initial_location();
    ///
    /// // Add a new clock
    /// let clock = pg_builder.new_clock();
    ///
    /// // Add an autonomous timed transition
    /// pg_builder
    ///     .add_autonomous_timed_transition(initial_loc, initial_loc, None, vec![(clock, None, Some(1))])
    ///     .expect("this transition can be added");
    /// pg_builder
    ///     .add_autonomous_timed_transition(initial_loc, initial_loc, Some(BooleanExpr::from(false)), vec![(clock, Some(1), None)])
    ///     .expect("this one too");
    /// ```
    #[inline(always)]
    pub fn add_autonomous_timed_transition(
        &mut self,
        pre: Location,
        post: Location,
        guard: Option<PgGuard>,
        constraints: Vec<TimeConstraint>,
    ) -> Result<(), PgError> {
        self.add_timed_transition(pre, EPSILON, post, guard, constraints)
    }

    /// Produces a [`ProgramGraph`] defined by the [`ProgramGraphBuilder`]'s data and consuming it.
    ///
    /// Since the construction of the builder is already checked ad every step,
    /// this method cannot fail.
    pub fn build(mut self) -> ProgramGraph {
        // Since vectors of effects and transitions will become immutable,
        // they should be shrunk to take as little space as possible
        self.initial_states.shrink_to_fit();
        self.vars.shrink_to_fit();
        self.effects.iter_mut().for_each(|effect| {
            if let Effect::Effects(_, resets) = effect {
                resets.sort_unstable();
                resets.shrink_to_fit();
            }
        });
        self.effects.shrink_to_fit();
        self.locations.iter_mut().for_each(|(transitions, _)| {
            transitions.iter_mut().for_each(|(_, loc_transitions)| {
                loc_transitions.sort_unstable_by_key(|(p, ..)| *p);
                loc_transitions.shrink_to_fit();
            });
        });
        let mut locations = self
            .locations
            .into_iter()
            .map(|(transitions, loc_invariants)| {
                let mut transitions = Vec::from_iter(transitions);
                transitions.shrink_to_fit();
                // Actions are assumed to be sorted
                assert!(transitions.is_sorted_by_key(|(action, _)| *action));
                (transitions, loc_invariants)
            })
            .collect::<Vec<_>>();
        locations.shrink_to_fit();
        // Build program graph
        info!(
            "create Program Graph with: {} locations; {} actions; {} vars",
            locations.len(),
            self.effects.len(),
            self.vars.len()
        );
        ProgramGraph {
            initial_states: self.initial_states,
            effects: self.effects,
            locations,
            vars: self.vars,
            clocks: self.clocks,
        }
    }
}
