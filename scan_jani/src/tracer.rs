use super::JaniModelData;
use scan_core::{
    Time, Tracer, Val,
    channel_system::{Action, Event},
};
use std::io::Write;

pub struct TracePrinter<W: Write> {
    writer: csv::Writer<W>,
    // model: &'a JaniModelData,
}

impl<W: Write> TracePrinter<W> {
    const HEADER: [&'static str; 2] = ["Time", "Action"];
    const UNKNOWN_ACTION: &'static str = "unknown action";
}

impl<W: Write> Tracer<W> for TracePrinter<W> {
    const EXTENSION: &'static str = "csv";

    type ModelData = JaniModelData;

    fn init(writer: W, data: &Self::ModelData) -> Self {
        let mut writer = csv::Writer::from_writer(writer);
        writer
            .write_record(
                Self::HEADER
                    .into_iter()
                    .map(String::from)
                    .chain(data.ports.iter().map(|(name, t)| format!("{name}: {t:?}"))),
            )
            .expect("write header");

        Self { writer }
    }

    fn trace(
        &mut self,
        data: &Self::ModelData,
        action: Action,
        _event: &Event,
        time: Time,
        ports: &[Vec<Val>],
    ) {
        let time = time.to_string();
        let action_name = data
            .actions
            .get(&action)
            .cloned()
            .unwrap_or(Self::UNKNOWN_ACTION.to_string());
        // self.model.actions.get(event).cloned().unwrap_or_default();

        self.writer
            .write_record(
                [time, action_name]
                    .into_iter()
                    .chain(ports.first().unwrap().iter().copied().map(format_val)),
            )
            .expect("write record");
    }
}

fn format_val(val: Val) -> String {
    match val {
        Val::Boolean(true) => "true".to_string(),
        Val::Boolean(false) => "false".to_string(),
        Val::Integer(i) => i.to_string(),
        Val::Float(ordered_float) => ordered_float.to_string(),
        Val::Natural(n) => n.to_string(),
    }
}
