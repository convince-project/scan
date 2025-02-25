use rand::{rngs::SmallRng, SeedableRng};
use scan_core::{program_graph::*, *};

#[test]
fn counter_pg() -> Result<(), PgError> {
    let mut rng = SmallRng::from_seed([0; 32]);
    let mut pg = ProgramGraphBuilder::new();
    let initial = pg.initial_location();
    let action = pg.new_action();
    let var = pg.new_var_with_rng(Expression::Const(Val::Integer(0)), &mut rng)?;
    pg.add_effect(
        action,
        var,
        Expression::Sum(vec![
            Expression::Var(var, Type::Integer),
            Expression::Const(Val::Integer(1)),
        ]),
    )
    .unwrap();
    for counter in 0..10 {
        let guard = Expression::Equal(Box::new((
            Expression::Var(var, Type::Integer),
            Expression::Const(Val::Integer(counter)),
        )));
        pg.add_transition(initial, action, initial, Some(guard))
            .unwrap();
    }
    let mut pg = pg.build();
    while let Some((act, post)) = pg.possible_transitions().last() {
        assert_eq!(post, initial);
        assert_eq!(act, action);
        pg.transition(act, post, &mut rng)?;
    }
    Ok(())
}
