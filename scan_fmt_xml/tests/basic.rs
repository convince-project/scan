use anyhow::anyhow;
use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

const MAXSTEP: usize = 1000;

#[test]
fn fsm() -> anyhow::Result<()> {
    test(&PathBuf::from_str("./tests/assets/test_fsm/model.xml")?)
}

#[test]
fn datamodel() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_datamodel/model.xml",
    )?)
}

#[test]
fn enumdata() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_enumdata/model.xml",
    )?)
}

#[test]
fn send() -> anyhow::Result<()> {
    test(&PathBuf::from_str("./tests/assets/test_send/model.xml")?)
}

#[test]
fn send_triangle() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_send_triangle/model.xml",
    )?)
}

#[test]
fn send_onentry() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_send_onentry/model.xml",
    )?)
}

#[test]
fn origin() -> anyhow::Result<()> {
    test(&PathBuf::from_str("./tests/assets/test_origin/model.xml")?)
}

#[test]
fn origin_location() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_origin_location/model.xml",
    )?)
}

#[test]
fn param() -> anyhow::Result<()> {
    test(&PathBuf::from_str("./tests/assets/test_param/model.xml")?)
}

#[test]
fn param_triangle() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_param_triangle/model.xml",
    )?)
}

#[test]
fn param_tennis() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_param_tennis/model.xml",
    )?)
}

#[test]
fn conditional() -> anyhow::Result<()> {
    test(&PathBuf::from_str("./tests/assets/test_if/model.xml")?)
}

#[test]
fn elif() -> anyhow::Result<()> {
    test(&PathBuf::from_str("./tests/assets/test_elif/model.xml")?)
}

#[test]
fn bt_success() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_bt_success/model.xml",
    )?)
}

#[test]
fn bt_failure() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_bt_failure/model.xml",
    )?)
}

#[test]
fn bt_rseq_success() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_bt_rseq_success/model.xml",
    )?)
}

#[test]
fn bt_rseq_failure() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_bt_rseq_failure/model.xml",
    )?)
}

#[test]
fn bt_rfbk_success() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_bt_rfbk_success/model.xml",
    )?)
}

#[test]
fn bt_rfbk_failure() -> anyhow::Result<()> {
    test(&PathBuf::from_str(
        "./tests/assets/test_bt_rfbk_failure/model.xml",
    )?)
}

fn test(path: &Path) -> anyhow::Result<()> {
    let mut model = scan_fmt_xml::load(path)?.model.channel_system().to_owned();
    let mut steps = 0;
    assert!(model.possible_transitions().count() > 0);
    while let Some((pg_id, act, loc)) = model
        .possible_transitions()
        .take(1)
        .collect::<Vec<_>>()
        .pop()
    {
        model.transition(pg_id, act, loc)?;
        steps += 1;
        if steps >= MAXSTEP {
            return Err(anyhow!("step limit reached"));
        }
    }
    Ok(())
}
