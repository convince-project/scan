use crate::parser::{OmgBaseType, OmgType, OmgTypeDef, OmgTypes};

use super::ScxmlModel;
use scan_core::channel_system::{Event, EventType};
use scan_core::{RunOutcome, Time, Tracer, Val};
use std::{
    env::current_dir,
    fs::{File, create_dir, create_dir_all, exists, remove_file, rename},
    path::PathBuf,
    sync::{Arc, atomic::AtomicU32},
};

#[derive(Debug)]
pub struct TracePrinter<'a> {
    index: Arc<AtomicU32>,
    path: PathBuf,
    writer: Option<csv::Writer<flate2::write::GzEncoder<File>>>,
    model: &'a ScxmlModel,
}

impl<'a> TracePrinter<'a> {
    const FOLDER: &'static str = "traces";
    const TEMP: &'static str = ".temp";
    const SUCCESSES: &'static str = "successes";
    const FAILURES: &'static str = "failures";
    const HEADER: [&'static str; 5] = ["Time", "Origin", "Target", "Event", "Values"];

    pub fn new(model: &'a ScxmlModel) -> Self {
        let mut path = current_dir().expect("current dir");
        for i in 0.. {
            path.push(format!("{}_{i:02}", Self::FOLDER));
            if std::fs::create_dir(&path).is_ok() {
                path.push(Self::TEMP);
                create_dir(&path).expect("create temp dir");
                assert!(path.pop());
                path.push(Self::SUCCESSES);
                create_dir(&path).expect("create temp dir");
                assert!(path.pop());
                path.push(Self::FAILURES);
                create_dir(&path).expect("create temp dir");
                assert!(path.pop());
                break;
            } else {
                assert!(path.pop());
            }
        }

        Self {
            index: Arc::new(AtomicU32::new(0)),
            path,
            writer: None,
            model,
        }
    }

    fn format_state<I: IntoIterator<Item = Val>>(&self, ports: I) -> Vec<String> {
        let mut iter = ports.into_iter();
        self.model
            .ports
            .iter()
            .map(move |(_, omg_type, types)| {
                format_val(
                    iter.by_ref()
                        .take(types.len())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    omg_type,
                    &self.model.omg_types,
                )
            })
            .collect()
    }
}

impl<'a> Clone for TracePrinter<'a> {
    fn clone(&self) -> Self {
        // Get the temp folder
        let mut path = self.path.clone();
        if path.is_file() {
            path.pop();
        }
        Self {
            index: Arc::clone(&self.index),
            path,
            writer: None,
            model: self.model,
        }
    }
}

impl<'a> Tracer<Event> for TracePrinter<'a> {
    fn init(&mut self) {
        let idx = self
            .index
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let filename = format!("{idx:04}");
        self.path.push(Self::TEMP);
        self.path.push(&filename);
        self.path.add_extension("gz");
        let file = File::create_new(&self.path).expect("create file");
        let enc = flate2::GzBuilder::new()
            .filename(filename + ".csv")
            .comment("Scan-generated execution trace")
            .write(file, flate2::Compression::best());
        let mut writer = csv::WriterBuilder::new().from_writer(enc);
        writer
            .write_record(
                Self::HEADER.into_iter().map(String::from).chain(
                    self.model.ports.iter().map(|(name, omg_type, _)| {
                        format!("{name}: {}", format_omg_type(omg_type))
                    }),
                ),
            )
            .expect("write header");
        self.writer = Some(writer);
    }

    fn trace<I: IntoIterator<Item = Val>>(&mut self, event: &Event, time: Time, ports: I) {
        let mut fields = Vec::new();
        let time = time.to_string();
        let origin_name;
        let target_name;
        let event_name;
        let param_types;
        let mut params = String::new();
        fields.push(time.as_str());

        if let Some((src, trg, event_idx)) = self.model.parameters.get(&event.channel) {
            origin_name = self.model.fsm_names.get(&(*src).into()).unwrap().to_owned();
            target_name = self.model.fsm_names.get(&(*trg).into()).unwrap().to_owned();
            (event_name, param_types) = self.model.events.get(*event_idx).unwrap().clone();
            if let EventType::Send(ref vals) = event.event_type {
                params = format_val_from_def(
                    vals,
                    param_types.as_ref().unwrap(),
                    &self.model.omg_types,
                    true,
                );
            } else {
                return;
            }
        } else if let Some(trg) = self.model.ext_queues.get(&event.channel) {
            target_name = self.model.fsm_names.get(&(*trg).into()).unwrap().to_owned();
            if let EventType::Send(ref vals) = event.event_type {
                if let (Val::Integer(sent_event), Val::Integer(origin)) = (vals[0], vals[1]) {
                    origin_name = self
                        .model
                        .fsm_names
                        .get(&(origin as u16))
                        .unwrap()
                        .to_owned();
                    (event_name, param_types) = self.model.events[sent_event as usize].clone();
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
        } else if self.model.int_queues.contains(&event.channel) {
            origin_name = self
                .model
                .fsm_names
                .get(&event.pg_id.into())
                .unwrap()
                .to_owned();
            target_name = origin_name.clone();
            if let EventType::Send(ref vals) = event.event_type {
                if let Val::Integer(sent_event) = vals[0] {
                    (event_name, param_types) = self.model.events[sent_event as usize].clone();
                    if param_types.is_some() {
                        // No need to trace this as parameters event already traced
                        return;
                    }
                } else {
                    panic!("events should be indexed by integer");
                }
            } else {
                return;
            }
        } else {
            panic!("Events should all be either internal or external events");
        }

        let state = self.format_state(ports);
        self.writer
            .as_mut()
            .unwrap()
            .write_record(
                [time, origin_name, target_name, event_name, params]
                    .into_iter()
                    .chain(state),
            )
            .expect("write record");
    }

    fn finalize(self, outcome: &RunOutcome) {
        let mut writer = self.writer.unwrap();
        writer.flush().expect("flush csv content");
        writer
            .into_inner()
            .expect("encoder")
            .try_finish()
            .expect("finish");

        let mut new_path = self.path.clone();
        // pop file name
        new_path.pop();
        // pop temp folder
        new_path.pop();
        match outcome {
            RunOutcome::Verified(verified) => {
                if verified.iter().all(|b| *b) {
                    new_path.push(Self::SUCCESSES);
                } else {
                    new_path.push(Self::FAILURES);
                    // This path might not exist yet
                    if !exists(new_path.as_path()).expect("check folder") {
                        create_dir_all(new_path.clone()).expect("create missing folder");
                    }
                }
            }
            RunOutcome::Incomplete => {
                remove_file(&self.path).expect("delete file");
                return;
            }
        }

        new_path.push(self.path.file_name().expect("file name"));
        rename(&self.path, new_path).expect("renaming");
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
            if let Val::Integer(int) = vals[0] {
                items[int as usize].clone()
            } else {
                panic!("enumeration is not represented as Integer")
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
