use crate::parser::{OmgBaseType, OmgType, OmgTypeDef, OmgTypes};

use super::ScxmlModel;
use scan_core::channel_system::{Action, Event, EventType};
use scan_core::{Time, TraceWriter, Tracer, Val};

#[derive(Debug)]
pub struct TracePrinter {
    writer: csv::Writer<TraceWriter>,
}

impl Drop for TracePrinter {
    fn drop(&mut self) {
        self.writer.flush().expect("flush writer");
    }
}

impl TracePrinter {
    const HEADER: [&'static str; 5] = ["Time", "Event", "Origin", "Target", "Values"];

    fn format_state(&self, model: &ScxmlModel, event: &Event, ports: &[Vec<Val>]) -> Vec<String> {
        // Assumes model.port_vals are ordered
        model
            .port_vars
            .iter()
            .map(move |(_, omg_type, exprs)| {
                format_val(
                    exprs
                        .iter()
                        .map(|expr| {
                            expr.eval_deterministic(&|atom| match atom {
                                scan_core::Atom::State(channel, i) => ports
                                    .get(model.ports.binary_search(&channel).unwrap())
                                    .unwrap()[i],
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
                    &model.omg_types,
                )
            })
            .collect()
    }
}

impl Tracer for TracePrinter {
    const EXTENSION: &'static str = "csv";

    type ModelData = ScxmlModel;

    fn init(writer: TraceWriter, data: &ScxmlModel) -> Self {
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

        Self { writer }
    }

    fn trace(
        &mut self,
        data: &ScxmlModel,
        _action: Action,
        event: &Event,
        time: Time,
        ports: &[Vec<Val>],
    ) {
        let mut fields = Vec::new();
        let time = time.to_string();
        let origin_name;
        let target_name;
        let event_name;
        let param_types;
        let mut params = String::new();
        fields.push(time.as_str());

        if let Some((src, trg, event_idx)) = data.parameters.get(&event.channel) {
            origin_name = data.fsm_names.get(&(*src).into()).unwrap().to_owned();
            target_name = data.fsm_names.get(&(*trg).into()).unwrap().to_owned();
            (event_name, param_types) = data.events.get(*event_idx).unwrap().clone();
            if let EventType::Send(ref vals) = event.event_type {
                params =
                    format_val_from_def(vals, param_types.as_ref().unwrap(), &data.omg_types, true);
            } else {
                return;
            }
        } else if let Some(trg) = data.ext_queues.get(&event.channel) {
            target_name = data.fsm_names.get(&(*trg).into()).unwrap().to_owned();
            if let EventType::Send(ref vals) = event.event_type {
                if let (Val::Natural(sent_event), Val::Natural(origin)) = (vals[0], vals[1]) {
                    origin_name = data.fsm_names.get(&(origin as u16)).unwrap().to_owned();
                    (event_name, param_types) = data.events[sent_event as usize].clone();
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
        } else if data.int_queues.contains(&event.channel) {
            origin_name = data.fsm_names.get(&event.pg_id.into()).unwrap().to_owned();
            target_name = origin_name.clone();
            if let EventType::Send(ref vals) = event.event_type {
                if let Val::Natural(sent_event) = vals[0] {
                    (event_name, param_types) = data.events[sent_event as usize].clone();
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

        let state = self.format_state(data, event, ports);
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

fn format_val(vals: &[Val], omg_type: &OmgType, omg_types: &OmgTypes) -> String {
    match omg_type {
        OmgType::Base(OmgBaseType::String) => {
            if let Val::Natural(index) = vals
                .first()
                .expect("strings should be encoded as exactly one natural number variable")
            {
                format!(
                    "'{}'",
                    omg_types
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
            format_val_from_def(
                vals,
                omg_types.type_defs.get(omg_name).expect("type def"),
                omg_types,
                false
            )
        ),
    }
}

fn format_val_from_def(
    vals: &[Val],
    omg_type: &OmgTypeDef,
    omg_types: &OmgTypes,
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
                    let size = omg_type.size(omg_types).unwrap();
                    prev_size_acc = size_acc;
                    size_acc += size;
                    let field = format_val(&vals[prev_size_acc..size_acc], omg_type, omg_types);
                    format!("{name}: {field}")
                })
                .collect::<Vec<_>>();
            if spread_structs {
                // print on multiple lines
                format!("{fields:#?}")
            } else {
                format!("{fields:?}")
            }
            .replace("\"", "")
        }
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
