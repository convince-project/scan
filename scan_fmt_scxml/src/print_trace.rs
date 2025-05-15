/*This file is not part of our library, we added it to integrate our library with scan. */
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
pub struct TracePrinter {
    index: Arc<AtomicU32>,
    path: PathBuf,
    writer: Option<csv::Writer<flate2::write::GzEncoder<File>>>,
    model: Arc<ScxmlModel>,
}

impl TracePrinter {
    const FOLDER: &str = "traces";
    const TEMP: &str = ".temp";
    const SUCCESSES: &str = "successes";
    const FAILURES: &str = "failures";
    const HEADER: [&str; 7] = [
        "Time",
        "Send/Receive",
        "Origin",
        "Target",
        "Event",
        "Message",
        "Value",
    ];

    pub fn new(model: Arc<ScxmlModel>) -> Self {
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
}

impl Clone for TracePrinter {
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
            model: Arc::clone(&self.model),
        }
    }
}

impl Tracer<Event> for TracePrinter {
    fn init(&mut self) {
        let idx = self
            .index
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let filename = format!("{idx:04}.csv.gz");
        self.path.push(Self::TEMP);
        self.path.push(&filename);
        let file = File::create_new(&self.path).expect("create file");
        let enc = flate2::GzBuilder::new()
            .filename(filename)
            .write(file, flate2::Compression::fast());
        let mut writer = csv::WriterBuilder::new().from_writer(enc);
        writer
            .write_record(
                Self::HEADER.into_iter().map(String::from).chain(
                    self.model
                        .ports
                        .iter()
                        .map(|(name, t)| format!("{name}: {t:?}")),
                ),
            )
            .expect("write header");
        self.writer = Some(writer);
    }

    fn trace<'a, I: IntoIterator<Item = &'a Val>>(&mut self, event: &Event, time: Time, ports: I) {
        let mut fields = Vec::new();
        let time = time.to_string();
        let mut action = String::new();
        let origin_name;
        let target_name;
        let event_name;
        let mut param_name = String::new();
        let mut param_value = String::new();
        fields.push(time.as_str());

        if let Some((src, trg, event_idx, param)) = self.model.parameters.get(&event.channel) {
            origin_name = self.model.fsm_names.get(src).unwrap().to_owned();
            target_name = self.model.fsm_names.get(trg).unwrap().to_owned();
            event_name = self.model.events.get(*event_idx).unwrap().clone();
            param_name = param.to_owned();
            match event.event_type {
                EventType::Send(ref val) => {
                    action = "S".to_string();
                    param_value = format!("{val:?}");
                }
                EventType::Receive(ref val) => {
                    action = "R".to_string();
                    param_value = format!("{val:?}");
                }
                EventType::ProbeEmptyQueue | EventType::ProbeFullQueue => return,
            }
        } else if let Some(trg) = self.model.ext_queues.get(&event.channel) {
            target_name = self.model.fsm_names.get(trg).unwrap().to_owned();
            match event.event_type {
                EventType::Send(ref val) => {
                    action = "S".to_string();
                    if let Val::Tuple(e) = val {
                        if let (Val::Integer(sent_event), Val::Integer(origin)) = (&e[0], &e[1]) {
                            origin_name = self
                                .model
                                .fsm_indexes
                                .get(&(*origin as usize))
                                .unwrap()
                                .to_owned();
                            event_name =
                                self.model.events.get(*sent_event as usize).unwrap().clone();
                        } else {
                            panic!("events should be pairs");
                        }
                    } else {
                        panic!("events should be pairs");
                    }
                }
                EventType::Receive(ref val) => {
                    action = "R".to_string();
                    if let Val::Tuple(e) = val {
                        if let (Val::Integer(sent_event), Val::Integer(origin)) = (&e[0], &e[1]) {
                            origin_name = self
                                .model
                                .fsm_indexes
                                .get(&(*origin as usize))
                                .unwrap()
                                .to_owned();
                            event_name =
                                self.model.events.get(*sent_event as usize).unwrap().clone();
                        } else {
                            panic!("events should be pairs");
                        }
                    } else {
                        panic!("events should be pairs");
                    }
                }
                EventType::ProbeEmptyQueue | EventType::ProbeFullQueue => return,
            }
        } else if self.model.int_queues.contains(&event.channel) {
            origin_name = self.model.fsm_names.get(&event.pg_id).unwrap().to_owned();
            target_name = origin_name.clone();
            match event.event_type {
                EventType::Send(ref val) => {
                    action = "S".to_string();
                    if let Val::Integer(sent_event) = val {
                        event_name = self.model.events.get(*sent_event as usize).unwrap().clone();
                    } else {
                        panic!("events should be indexed by integer");
                    }
                }
                EventType::Receive(ref val) => {
                    action = "R".to_string();
                    if let Val::Integer(sent_event) = val {
                        event_name = self.model.events.get(*sent_event as usize).unwrap().clone();
                    } else {
                        panic!("events should be indexed by integer");
                    }
                }
                EventType::ProbeEmptyQueue | EventType::ProbeFullQueue => return,
            }
        } else {
            event_name = String::new();
            param_name = String::new();
            match event.event_type {
                EventType::Send(ref val) => {
                    origin_name = self.model.fsm_names.get(&event.pg_id).unwrap().to_owned();
                    target_name = format!("{:?}", event.channel);
                    param_value = format!("{val:?}");
                }
                EventType::Receive(ref val) => {
                    origin_name = format!("{:?}", event.channel);
                    target_name = self.model.fsm_names.get(&event.pg_id).unwrap().to_owned();
                    param_value = format!("{val:?}");
                }
                EventType::ProbeEmptyQueue | EventType::ProbeFullQueue => return,
            }
        }

        self.writer
            .as_mut()
            .unwrap()
            .write_record(
                [
                    time,
                    action.to_owned(),
                    origin_name,
                    target_name,
                    event_name,
                    param_name,
                    param_value,
                ]
                .into_iter()
                .chain(ports.into_iter().map(format_val)),
            )
            .expect("write record");
    }

    fn finalize(self, outcome: RunOutcome) {
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
            RunOutcome::Success => new_path.push(Self::SUCCESSES),
            RunOutcome::Fail(violation) => {
                new_path.push(Self::FAILURES);
                new_path.push(self.model.guarantees.get(violation).unwrap());
                // This path might not exist yet
                if !exists(new_path.as_path()).expect("check folder") {
                    create_dir_all(new_path.clone()).expect("create missing folder");
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

fn format_val(val: &Val) -> String {
    match val {
        Val::Boolean(true) => "true".to_string(),
        Val::Boolean(false) => "false".to_string(),
        Val::Integer(i) => i.to_string(),
        Val::Float(ordered_float) => ordered_float.to_string(),
        Val::Tuple(vec) => {
            vec.iter()
                .fold("(".to_string(), |acc, v| acc + format_val(v).as_str())
                + ")"
        }
        Val::List(_, vec) => {
            vec.iter()
                .fold("[".to_string(), |acc, v| acc + format_val(v).as_str())
                + "]"
        }
    }
}
