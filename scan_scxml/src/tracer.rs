use crate::parser::{OmgBaseType, OmgType, OmgTypeDef};

use super::ScxmlModel;
use scan_core::channel_system::{Action, Channel, Event, EventType};
use scan_core::{Time, TraceWriter, Tracer, Val};

#[derive(Debug)]
pub struct TracePrinter<'a> {
    writer: csv::Writer<TraceWriter>,
    data: &'a ScxmlModel,
}

impl Drop for TracePrinter<'_> {
    fn drop(&mut self) {
        self.writer.flush().expect("flush writer");
    }
}

impl<'a> TracePrinter<'a> {
    const HEADER: [&'static str; 5] = ["Time", "Event", "Origin", "Target", "Params"];

    fn format_state<'b>(
        &self,
        event: Event,
        ports: &'b impl Fn(Channel) -> Option<&'b [Val]>,
    ) -> Vec<String> {
        // Assumes model.port_vals are ordered
        self.data
            .port_vars
            .iter()
            .map(move |(_, omg_type, exprs)| {
                self.format_val(
                    exprs
                        .iter()
                        .map(|expr| {
                            expr.eval_deterministic(&|atom| match atom {
                                scan_core::Atom::State(channel, i) => ports(channel).unwrap()[i],
                                scan_core::Atom::Event(channel) => {
                                    if event.channel == channel
                                        && let EventType::Send(_) = event.event_type
                                    {
                                        Val::from(true)
                                    } else {
                                        Val::from(false)
                                    }
                                }
                            })
                        })
                        .collect::<Vec<_>>()
                        .as_slice(),
                    omg_type,
                )
            })
            .collect()
    }

    fn format_val(&self, vals: &[Val], omg_type: &OmgType) -> String {
        match omg_type {
            OmgType::Base(OmgBaseType::String) => {
                if let Val::Natural(index) = vals
                    .first()
                    .expect("strings should be encoded as exactly one natural number variable")
                {
                    format!(
                        "'{}'",
                        self.data
                            .omg_types
                            .get_string(*index as usize)
                            .expect("all string codes should correspond to a string")
                    )
                } else {
                    panic!("string not encoded as a natural number")
                }
            }
            OmgType::Base(_omg_base_type) => format_base_val(vals[0]),
            OmgType::Array(_omg_base_type, _len) => format!(
                "{:?}",
                vals.iter()
                    .map(|val: &Val| format_base_val(*val))
                    .collect::<Vec<String>>()
            )
            .replace("\"", ""),
            OmgType::Custom(omg_name) => format!(
                "{omg_name}: {}",
                self.format_val_from_def(
                    vals,
                    self.data
                        .omg_types
                        .type_defs
                        .get(omg_name)
                        .expect("type def"),
                    false
                )
            ),
        }
    }

    fn format_val_from_def(
        &self,
        vals: &[Val],
        omg_type: &OmgTypeDef,
        spread_structs: bool,
    ) -> String {
        match omg_type {
            OmgTypeDef::Enumeration(items) => {
                if let Val::Natural(int) = vals[0] {
                    items[int as usize].clone()
                } else {
                    panic!("enumeration is not represented as Natural")
                }
            }
            OmgTypeDef::Structure(btree_map) => {
                let mut prev_size_acc = 0;
                let mut size_acc = 0;
                let fields = btree_map
                    .iter()
                    .map(|(name, omg_type)| {
                        let size = omg_type.size(&self.data.omg_types).unwrap();
                        prev_size_acc = size_acc;
                        size_acc += size;
                        let field = self.format_val(&vals[prev_size_acc..size_acc], omg_type);
                        format!("{name}: {field}")
                    })
                    .collect::<Vec<_>>();
                if spread_structs {
                    // print on multiple lines
                    fields.join("\n")
                } else {
                    format!("{fields:?}")
                }
                .replace("\"", "")
            }
        }
    }
}

impl<'a> Tracer<'a> for TracePrinter<'a> {
    const EXTENSION: &'static str = "csv";

    type ModelData = ScxmlModel;

    fn init(writer: TraceWriter, data: &'a ScxmlModel) -> Self {
        let mut writer = csv::Writer::from_writer(writer);
        writer
            .write_record(
                Self::HEADER.into_iter().map(String::from).chain(
                    data.port_vars.iter().map(|(name, omg_type, _)| {
                        format!("{name}: {}", format_omg_type(omg_type))
                    }),
                ),
            )
            .expect("write header");

        Self { writer, data }
    }

    fn trace<'b>(
        &mut self,
        _action: Action,
        event: Event<'_>,
        time: Time,
        ports: &'b impl Fn(Channel) -> Option<&'b [Val]>,
    ) {
        let mut fields = Vec::new();
        let time = time.to_string();
        let origin_name;
        let target_name;
        let event_name;
        let param_types;
        let mut params = String::new();
        fields.push(time.as_str());

        if let Some((src, trg, event_idx)) = self.data.parameters.get(&event.channel) {
            origin_name = self.data.fsm_names.get(&(*src).into()).unwrap().to_owned();
            target_name = self.data.fsm_names.get(&(*trg).into()).unwrap().to_owned();
            (event_name, param_types) = self.data.events.get(*event_idx).unwrap().clone();
            if let EventType::Send(vals) = event.event_type {
                params = self.format_val_from_def(vals, param_types.as_ref().unwrap(), true);
            } else {
                return;
            }
        } else if let Some(trg) = self.data.ext_queues.get(&event.channel) {
            target_name = self.data.fsm_names.get(&(*trg).into()).unwrap().to_owned();
            if let EventType::Send(vals) = event.event_type {
                if let (Val::Natural(sent_event), Val::Natural(origin)) = (vals[0], vals[1]) {
                    origin_name = self
                        .data
                        .fsm_names
                        .get(&(origin as u16))
                        .unwrap()
                        .to_owned();
                    (event_name, param_types) = self.data.events[sent_event as usize].clone();
                    if param_types.is_some() {
                        // No need to trace this as parameters event already traced
                        return;
                    }
                } else {
                    panic!("events should be pairs");
                }
            } else {
                return;
            }
        } else if self.data.int_queues.contains(&event.channel) {
            origin_name = self
                .data
                .fsm_names
                .get(&event.pg_id.into())
                .unwrap()
                .to_owned();
            target_name = origin_name.clone();
            if let EventType::Send(vals) = event.event_type {
                if let Val::Natural(sent_event) = vals[0] {
                    (event_name, param_types) = self.data.events[sent_event as usize].clone();
                    if param_types.is_some() {
                        // No need to trace this as parameters event already traced
                        return;
                    }
                } else {
                    panic!("events should be indexed by natural");
                }
            } else {
                return;
            }
        } else {
            panic!("Events should all be either internal or external events");
        }

        let state = self.format_state(event, ports);
        self.writer
            .write_record(
                [time, event_name, origin_name, target_name, params]
                    .into_iter()
                    .chain(state),
            )
            .expect("write record");
    }
}

fn format_omg_type(omg_type: &OmgType) -> String {
    match omg_type {
        OmgType::Base(omg_base_type) => format_omg_base_type(*omg_base_type).to_string(),
        OmgType::Array(omg_base_type, _) => {
            format!("[{}]", format_omg_base_type(*omg_base_type))
        }
        OmgType::Custom(name) => name.clone(),
    }
}

fn format_omg_base_type(omg_base_type: OmgBaseType) -> &'static str {
    match omg_base_type {
        OmgBaseType::Boolean => "bool",
        OmgBaseType::Int64 => "int32",
        OmgBaseType::F64 => "float64",
        OmgBaseType::Uri => "uri",
        OmgBaseType::String => "string",
        OmgBaseType::Uint64 => "uint64",
    }
}

fn format_base_val(val: Val) -> String {
    match val {
        Val::Boolean(true) => "true".to_string(),
        Val::Boolean(false) => "false".to_string(),
        Val::Integer(i) => i.to_string(),
        Val::Float(ordered_float) => ordered_float.to_string(),
        Val::Natural(n) => n.to_string(),
    }
}
