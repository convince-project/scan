use super::{
    Action, Clock, FnEffect, FnExpression, Location, PgError, PgExpression, ProgramGraph,
    ProgramGraphDef, TimeConstraint, Var, EPSILON,
};
use crate::grammar::{Type, Val};
use log::info;
use rand::{rngs::SmallRng, Rng, SeedableRng};
use std::{collections::BTreeSet, sync::Arc};

#[derive(Debug, Clone)]
enum Effect {
    Effects(Vec<(Var, PgExpression)>, Vec<Clock>),
    Send(PgExpression),
    Receive(Var),
}

impl<R: Rng + 'static> From<Effect> for FnEffect<R> {
    fn from(value: Effect) -> Self {
        match value {
            Effect::Effects(effects, resets) => {
                let mut effects = effects
                    .into_iter()
                    .map(|(var, expr)| -> (Var, FnExpression<Var, _>) {
                        (var, FnExpression::<Var, _>::from(expr))
                    })
                    .collect::<Vec<_>>();
                effects.shrink_to_fit();
                FnEffect::Effects(effects, resets)
            }
            Effect::Send(msg) => FnEffect::Send(msg.into()),
            Effect::Receive(var) => FnEffect::Receive(var),
        }
    }
}

type TransitionBuilder = (Action, Location, Option<PgExpression>, Vec<TimeConstraint>);

/// Defines and builds a PG.
#[derive(Debug, Clone)]
pub struct ProgramGraphBuilder {
    initial_states: Vec<Location>,
    // Effects are indexed by actions
    effects: Vec<Effect>,
    // Transitions are indexed by locations
    // We can assume there is at most one condition by logical disjunction
    locations: Vec<(Vec<TransitionBuilder>, Vec<TimeConstraint>)>,
    // Time invariants of each location
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
    /// At creation, this will only have the inital location with no variables, no actions and no transitions.
    /// The initial location can be retreived by [`ProgramGraphBuilder::initial_location`]
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
            .map(Val::r#type)
            .ok_or(PgError::MissingVar(var))
    }

    /// Adds a new variable with the given initial value (and the inferred type) to the PG.
    /// It creates and uses a default RNG for probabilistic expressions.
    ///
    /// It fails if the expression giving the initial value of the variable is not well-typed.
    ///
    /// ```
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder};
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // Create a new action
    /// let action = pg_builder.new_action();
    ///
    /// // Create a new variable
    /// pg_builder
    ///     .new_var(PgExpression::And(vec![PgExpression::from(0)]))
    ///     .expect_err("expression is badly-typed");
    /// ```
    pub fn new_var(&mut self, init: PgExpression) -> Result<Var, PgError> {
        let mut rng = SmallRng::from_os_rng();
        self.new_var_with_rng(init, &mut rng)
    }

    /// Adds a new variable with the given initial value (and the inferred type) to the PG,
    /// using the given RNG for probabilistic expressions.
    ///
    /// It fails if the expression giving the initial value of the variable is not well-typed.
    ///
    /// ```
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder, Var};
    /// # use rand::rngs::SmallRng;
    /// # use rand::{Rng, SeedableRng};
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // Create RNG using `rand`
    /// let mut rng = SmallRng::from_os_rng();
    ///
    /// // Create a new variable
    /// let var: Var = pg_builder
    ///     .new_var_with_rng(PgExpression::RandBool(0.5), &mut rng)
    ///     .expect("expression is well-typed");
    /// ```
    pub fn new_var_with_rng<R: Rng + 'static>(
        &mut self,
        init: PgExpression,
        rng: &mut R,
    ) -> Result<Var, PgError> {
        let idx = self.vars.len();
        // We check the type to make sure the expression is well-formed
        let _ = init.r#type().map_err(PgError::Type)?;
        init.context(&|var| self.vars.get(var.0 as usize).map(Val::r#type))
            .map_err(PgError::Type)?;
        let val = FnExpression::from(init).eval(&|var| self.vars[var.0 as usize].clone(), rng);
        self.vars.push(val);
        Ok(Var(idx as u16))
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
        Action(idx as u16)
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
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // Create a new action
    /// let action: Action = pg_builder.new_action();
    ///
    /// // Create a new variable
    /// let var: Var = pg_builder.new_var(PgExpression::from(true)).expect("expression is well-typed");
    ///
    /// // Add an effect to the action
    /// pg_builder
    ///     .add_effect(action, var, PgExpression::from(1))
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
            .context(&|var| self.vars.get(var.0 as usize).map(Val::r#type))
            .map_err(PgError::Type)?;
        let var_type = self
            .vars
            .get(var.0 as usize)
            .map(Val::r#type)
            .ok_or_else(|| PgError::MissingVar(var.to_owned()))?;
        if var_type == effect.r#type().map_err(PgError::Type)? {
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

    pub(crate) fn new_send(&mut self, msg: PgExpression) -> Result<Action, PgError> {
        // Check message is well-typed
        msg.context(&|var| self.vars.get(var.0 as usize).map(Val::r#type))
            .map_err(PgError::Type)?;
        let _ = msg.r#type().map_err(PgError::Type)?;
        // Actions are indexed progressively
        let idx = self.effects.len();
        self.effects.push(Effect::Send(msg));
        Ok(Action(idx as u16))
    }

    pub(crate) fn new_receive(&mut self, var: Var) -> Result<Action, PgError> {
        if self.vars.len() as u16 <= var.0 {
            Err(PgError::MissingVar(var.to_owned()))
        } else {
            // Actions are indexed progressively
            let idx = self.effects.len();
            self.effects.push(Effect::Receive(var));
            Ok(Action(idx as u16))
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
        invariants: Vec<TimeConstraint>,
    ) -> Result<Location, PgError> {
        if let Some((clock, _, _)) = invariants.iter().find(|(c, _, _)| c.0 >= self.clocks) {
            Err(PgError::MissingClock(*clock))
        } else {
            // Locations are indexed progressively
            let idx = self.locations.len();
            self.locations.push((Vec::new(), invariants));
            Ok(Location(idx as u16))
        }
    }

    /// Adds a new location to the PG and returns its [`Location`] indexing object.
    #[inline(always)]
    pub fn new_initial_location(&mut self) -> Location {
        assert!(self.initial_states.is_empty());
        self.new_initial_timed_location(Vec::new())
            .expect("new untimed location")
    }

    /// Adds a new location to the PG with the given time invariants,
    /// and returns its [`Location`] indexing object.
    pub fn new_initial_timed_location(
        &mut self,
        invariants: Vec<TimeConstraint>,
    ) -> Result<Location, PgError> {
        if let Some((clock, _, _)) = invariants.iter().find(|(c, _, _)| c.0 >= self.clocks) {
            Err(PgError::MissingClock(*clock))
        } else {
            // Locations are indexed progressively
            let idx = self.locations.len();
            self.locations.push((Vec::new(), invariants));
            let location = Location(idx as u16);
            self.initial_states.push(location);
            Ok(location)
        }
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
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder};
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
    ///     .add_transition(initial_loc, action, initial_loc, Some(PgExpression::from(1)))
    ///     .expect_err("the guard expression is not boolean");
    /// ```
    #[inline(always)]
    pub fn add_transition(
        &mut self,
        pre: Location,
        action: Action,
        post: Location,
        guard: Option<PgExpression>,
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
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder};
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
    ///     .add_timed_transition(initial_loc, action, initial_loc, Some(PgExpression::from(1)), vec![(clock, Some(1), None)])
    ///     .expect_err("the guard expression is not boolean");
    /// ```
    pub fn add_timed_transition(
        &mut self,
        pre: Location,
        action: Action,
        post: Location,
        guard: Option<PgExpression>,
        constraints: Vec<TimeConstraint>,
    ) -> Result<(), PgError> {
        // Check 'pre' and 'post' locations exists
        if self.locations.len() as u16 <= pre.0 {
            Err(PgError::MissingLocation(pre))
        } else if self.locations.len() as u16 <= post.0 {
            Err(PgError::MissingLocation(post))
        } else if action != EPSILON && self.effects.len() as u16 <= action.0 {
            // Check 'action' exists
            Err(PgError::MissingAction(action))
        } else if guard
            .as_ref()
            .is_some_and(|guard| !matches!(guard.r#type(), Ok(Type::Boolean)))
        {
            Err(PgError::TypeMismatch)
        } else if let Some((clock, _, _)) = constraints.iter().find(|(c, _, _)| c.0 >= self.clocks)
        {
            Err(PgError::MissingClock(*clock))
        } else {
            if let Some(ref guard) = guard {
                guard
                    .context(&|var| self.vars.get(var.0 as usize).map(Val::r#type))
                    .map_err(PgError::Type)?;
            }
            let (transitions, _) = &mut self.locations[pre.0 as usize];
            transitions.push((action, post, guard, constraints));
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
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder};
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // The builder is initialized with an initial location
    /// let initial_loc = pg_builder.new_initial_location();
    ///
    /// // Add a transition
    /// pg_builder
    ///     .add_autonomous_transition(initial_loc, initial_loc, None)
    ///     .expect("this autonomous transition can be added");
    /// pg_builder
    ///     .add_autonomous_transition(initial_loc, initial_loc, Some(PgExpression::from(1)))
    ///     .expect_err("the guard expression is not boolean");
    /// ```
    #[inline(always)]
    pub fn add_autonomous_transition(
        &mut self,
        pre: Location,
        post: Location,
        guard: Option<PgExpression>,
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
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder};
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
    ///     .add_autonomous_timed_transition(initial_loc, initial_loc, Some(PgExpression::from(1)), vec![(clock, Some(1), None)])
    ///     .expect_err("the guard expression is not boolean");
    /// ```
    #[inline(always)]
    pub fn add_autonomous_timed_transition(
        &mut self,
        pre: Location,
        post: Location,
        guard: Option<PgExpression>,
        constraints: Vec<TimeConstraint>,
    ) -> Result<(), PgError> {
        self.add_timed_transition(pre, EPSILON, post, guard, constraints)
    }

    /// Produces a [`ProgramGraph`] defined by the [`ProgramGraphBuilder`]'s data and consuming it.
    ///
    /// Since the construction of the builder is already checked ad every step,
    /// this method cannot fail.
    pub fn build<R: Rng + 'static>(mut self) -> ProgramGraph<R> {
        // Since vectors of effects and transitions will become unmutable,
        // they should be shrunk to take as little space as possible
        self.effects.shrink_to_fit();
        // Vars are not going to be unmutable,
        // but their number will be constant anyway
        self.vars.shrink_to_fit();
        let mut locations = self
            .locations
            .into_iter()
            .map(|(transitions, invariants)| {
                let mut transitions = transitions
                    .into_iter()
                    .map(|(a, p, guard, c)| (a, p, guard.map(FnExpression::from), c))
                    .collect::<Vec<_>>();
                transitions.sort_unstable_by_key(|(a, p, ..)| (*a, *p));
                transitions.shrink_to_fit();
                let actions = BTreeSet::from_iter(transitions.iter().map(|(a, ..)| *a));
                (transitions, invariants, actions)
            })
            .collect::<Vec<_>>();
        locations.shrink_to_fit();
        // Build program graph
        info!(
            "create Program Graph with:\n{} locations\n{} actions\n{} vars",
            locations.len(),
            self.effects.len(),
            self.vars.len()
        );
        let def = ProgramGraphDef {
            effects: self.effects.into_iter().map(FnEffect::from).collect(),
            locations,
        };
        self.initial_states.sort_unstable();
        self.initial_states.shrink_to_fit();
        // Initialize buf
        let mut buf = BTreeSet::from_iter(
            (0..self.initial_states.len() as u16)
                .map(Action)
                .chain([EPSILON]),
        );
        for loc in &self.initial_states {
            buf = &buf & &def.locations[loc.0 as usize].2
        }
        ProgramGraph {
            current_states: self.initial_states.into(),
            vars: self.vars,
            def: Arc::new(def),
            clocks: vec![0; self.clocks as usize],
            buf,
        }
    }
}
