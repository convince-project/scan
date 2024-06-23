use scan_core::{program_graph::*, *};

#[test]
fn counter_pg() -> Result<(), PgError> {
    let mut pg = ProgramGraphBuilder::new();
    let initial = pg.initial_location();
    let action = pg.new_action();
    let var = pg.new_var(Type::Integer);
    pg.add_effect(
        action,
        var,
        Expression::Sum(vec![Expression::Var(var), Expression::Integer(1)]),
    )
    .unwrap();
    for counter in 0..10 {
        let guard = Expression::Equal(Box::new((
            Expression::Var(var),
            Expression::Integer(counter),
        )));
        pg.add_transition(initial, action, initial, Some(guard))
            .unwrap();
    }
    let mut pg = pg.build();
    while let Some((act, post)) = pg.possible_transitions().last() {
        assert_eq!(post, initial);
        assert_eq!(act, action);
        pg.transition(act, post)?;
    }
    Ok(())
}
