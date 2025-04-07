use rand::seq::{IndexedRandom, IteratorRandom};
use std::path::Path;

const MAXSTEP: usize = 1000;

#[test]
fn jani_test() {
    test(Path::new("./tests/test.jani"))
}

#[test]
fn jani_test2() {
    test(Path::new("./tests/test2.jani"))
}

#[test]
fn sync() {
    test(Path::new("./tests/sync.jani"))
}

#[test]
fn die() {
    test(Path::new("./tests/die.jani"))
}

#[test]
fn dining_crypt3() {
    test(Path::new("./tests/dining_crypt3.jani"))
}

#[test]
fn ij_3() {
    test(Path::new("./tests/ij.3.jani"))
}

fn test(path: &Path) {
    let mut model = scan_jani::parse(path).unwrap().0.channel_system().clone();
    let mut steps = 0;
    assert!(model.possible_transitions().count() > 0);
    let mut rng = rand::rng();
    while let Some((pg_id, action, destination)) = model.possible_transitions().choose(&mut rng) {
        let destination = &destination
            .into_iter()
            .map(|d| *d.choose(&mut rng).expect("destination"))
            .collect::<Vec<_>>();
        model.transition(pg_id, action, destination).unwrap();
        steps += 1;
        assert!(steps < MAXSTEP, "step limit reached");
    }
    assert!(steps > 0, "no transitions executed");
}
