use scan_core::{program_graph::*, *};

#[test]
fn counter_pg() -> Result<(), PgError> {
    let mut pg = ProgramGraphBuilder::new();
    let initial = pg.new_initial_location();
    let action = pg.new_action();
    let var = pg.new_var(Expression::from(Val::Integer(0)))?;
    pg.add_effect(
        action,
        var,
        (Expression::from_var(var, Type::Integer) + Expression::from(Val::Integer(1))).unwrap(),
    )
    .unwrap();
    for counter in 0..10 {
        let guard = Expression::equal_to(
            Expression::from_var(var, Type::Integer),
            Expression::from(Val::Integer(counter)),
        )
        .unwrap();
        pg.add_transition(initial, action, initial, Some(guard))
            .unwrap();
    }
    let pg = PgModel::new(pg, Vec::new(), Vec::new());
    let mut pg: PgModelRun = pg.generate();
    while pg.transition().is_some() {}
    Ok(())
}
