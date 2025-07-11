use std::path::Path;

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
    let (scan, ..) = scan_scxml::load(path, &[])?;
    scan.adaptive(0.95, 0.01, 100, true).expect("verification");
    Ok(())
}
