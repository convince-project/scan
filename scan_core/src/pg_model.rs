use std::sync::Arc;

use rand::{SeedableRng, rngs::SmallRng};

use crate::{
    DummyRng, FnExpression, TransitionSystem, Val,
    program_graph::{Action, PgError, PgExpression, ProgramGraph, Var},
};

pub struct PgModel {
    pg: ProgramGraph<SmallRng>,
    rng: SmallRng,
    global_vars: Arc<Vec<Var>>,
    predicates: Arc<Vec<FnExpression<Var, DummyRng>>>,
}

impl PgModel {
    pub fn new(
        pg: ProgramGraph<SmallRng>,
        rng: SmallRng,
        global_vars: Vec<Var>,
        predicates: Vec<PgExpression>,
    ) -> Self {
        let predicates = predicates
            .into_iter()
            .map(Into::<FnExpression<Var, DummyRng>>::into)
            .collect();
        let predicates = Arc::new(predicates);
        let global_vars = Arc::new(global_vars);
        Self {
            pg,
            rng,
            global_vars,
            predicates,
        }
    }
}

impl Clone for PgModel {
    fn clone(&self) -> Self {
        Self {
            pg: self.pg.clone(),
            rng: SmallRng::from_os_rng(),
            global_vars: Arc::clone(&self.global_vars),
            predicates: Arc::clone(&self.predicates),
        }
    }
}

impl TransitionSystem<Action> for PgModel {
    type Err = PgError;

    fn transition(&mut self, _duration: crate::Time) -> Result<Option<Action>, PgError> {
        Ok(self.pg.montecarlo(&mut self.rng))
    }

    fn time(&self) -> crate::Time {
        0
    }

    fn labels(&self) -> Vec<bool> {
        self.predicates
            .iter()
            .map(|p| {
                if let Val::Boolean(b) = self.pg.eval(p) {
                    b
                } else {
                    panic!("non-bool pred")
                }
            })
            .collect()
    }

    fn state(&self) -> impl Iterator<Item = &crate::Val> {
        self.global_vars
            .as_ref()
            .iter()
            .map(|var| self.pg.val(*var).expect("value"))
    }
}
