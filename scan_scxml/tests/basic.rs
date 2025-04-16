use anyhow::anyhow;
use std::path::Path;

const MAXSTEP: usize = 1000;

#[test]
fn fsm() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_fsm/model.xml"))
}

#[test]
fn datamodel() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_datamodel/model.xml"))
}

#[test]
fn enumdata() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_enumdata/model.xml"))
}

#[test]
fn send() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_send/model.xml"))
}

#[test]
fn send_triangle() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_send_triangle/model.xml"))
}

#[test]
fn send_onentry() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_send_onentry/model.xml"))
}

#[test]
fn origin() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_origin/model.xml"))
}

#[test]
fn origin_location() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_origin_location/model.xml"))
}

#[test]
fn param() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_param/model.xml"))
}

#[test]
fn param_triangle() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_param_triangle/model.xml"))
}

#[test]
fn param_tennis() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_param_tennis/model.xml"))
}

#[test]
fn conditional() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_if/model.xml"))
}

#[test]
fn elif() -> anyhow::Result<()> {
    test(Path::new("./tests/assets/test_elif/model.xml"))
}

fn test(path: &Path) -> anyhow::Result<()> {
    let mut model = scan_fmt_xml::load(path)?.0.channel_system().to_owned();
    let mut steps = 0;
    assert!(model.possible_transitions().count() > 0);
    let mut pg_id;
    let mut post;
    let mut action;
    loop {
        if let Some((pg, a, p)) = model
            .possible_transitions()
            .filter_map(|(pg, a, iter)| {
                iter.into_iter()
                    .map(|mut v| v.next())
                    .collect::<Option<Vec<_>>>()
                    .map(|l| (pg, a, l))
            })
            .next()
        {
            pg_id = pg;
            action = a;
            post = p;
            steps += 1;
            if steps >= MAXSTEP {
                return Err(anyhow!("step limit reached"));
            }
        } else {
            break;
        }
        model.transition(pg_id, action, &post)?;
    }
    Ok(())
}
