use std::{
    collections::{HashMap, HashSet},
    fs::File,
    path::PathBuf,
    str::FromStr,
    sync::{atomic::AtomicU32, Arc},
};

use scan_fmt_xml::{
    scan_core::{
        channel_system::{self, Channel, Event, PgId},
        Publisher, Time, Val,
    },
    ScxmlModel,
};

#[derive(Debug)]
pub struct PrintTrace {
    index: Arc<AtomicU32>,
    path: PathBuf,
    writer: Option<csv::Writer<flate2::write::GzEncoder<File>>>,
    predicates: Arc<Vec<String>>,
    // guarantees: Arc<Vec<Pmtl<Atom<Event>>>>,
    // assumes: Arc<Vec<Pmtl<Atom<Event>>>>,
    fsm_names: Arc<HashMap<PgId, String>>,
    fsm_indexes: Arc<HashMap<usize, String>>,
    parameters: Arc<HashMap<Channel, (PgId, PgId, usize, String)>>,
    int_queues: Arc<HashSet<Channel>>,
    ext_queues: Arc<HashMap<Channel, PgId>>,
    events: Arc<HashMap<usize, String>>,
}

impl PrintTrace {
    const HEADER: [&str; 7] = [
        "Time",
        "Send/Receive",
        "Origin",
        "Target",
        "Event",
        "Message",
        "Value",
    ];

    pub fn new(model: &ScxmlModel) -> Self {
        Self {
            index: Arc::new(AtomicU32::new(0)),
            path: PathBuf::new(),
            writer: None,
            predicates: Arc::new(model.predicates.to_owned()),
            fsm_names: Arc::new(model.fsm_names.to_owned()),
            fsm_indexes: Arc::new(model.fsm_indexes.to_owned()),
            parameters: Arc::new(model.parameters.to_owned()),
            int_queues: Arc::new(model.int_queues.to_owned()),
            ext_queues: Arc::new(model.ext_queues.to_owned()),
            events: Arc::new(model.events.to_owned()),
        }
    }
}

impl Clone for PrintTrace {
    fn clone(&self) -> Self {
        Self {
            index: self.index.clone(),
            path: PathBuf::new(),
            writer: None,
            predicates: self.predicates.clone(),
            fsm_names: self.fsm_names.clone(),
            fsm_indexes: self.fsm_indexes.clone(),
            parameters: self.parameters.clone(),
            int_queues: self.int_queues.clone(),
            ext_queues: self.ext_queues.clone(),
            events: self.events.clone(),
        }
    }
}

impl Publisher<Event> for PrintTrace {
    fn init(&mut self) {
        let idx = self
            .index
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.path =
            PathBuf::from_str(format!("traces/.temp/{idx:04}.csv.gz").as_str()).expect("file path");
        let file = File::create_new(&self.path).expect("create file");
        let enc = flate2::GzBuilder::new()
            .filename(format!("{idx:04}.csv"))
            .write(file, flate2::Compression::fast());
        let mut writer = csv::WriterBuilder::new().from_writer(enc);
        writer
            .write_record(
                Self::HEADER
                    .into_iter()
                    .chain(self.predicates.iter().map(String::as_str)),
            )
            .expect("write header");
        self.writer = Some(writer);
    }

    fn publish(&mut self, event: &Event, time: Time, state: &[bool]) {
        let mut fields = Vec::new();
        let time = time.to_string();
        let mut action = String::new();
        let origin_name;
        let target_name;
        let event_name;
        let mut param_name = String::new();
        let mut param_value = String::new();
        fields.push(time.as_str());

        if let Some((src, trg, event_idx, param)) = self.parameters.get(&event.channel) {
            origin_name = self.fsm_names.get(src).unwrap().to_owned();
            target_name = self.fsm_names.get(trg).unwrap().to_owned();
            event_name = self.events.get(event_idx).unwrap().to_owned();
            param_name = param.to_owned();
            match event.event_type {
                channel_system::EventType::Send(ref val) => {
                    action = "S".to_string();
                    param_value = format!("{val:?}");
                }
                channel_system::EventType::Receive(ref val) => {
                    action = "R".to_string();
                    param_value = format!("{val:?}");
                }
                channel_system::EventType::ProbeEmptyQueue
                | channel_system::EventType::ProbeFullQueue => return,
            }
        } else if let Some(trg) = self.ext_queues.get(&event.channel) {
            target_name = self.fsm_names.get(trg).unwrap().to_owned();
            match event.event_type {
                channel_system::EventType::Send(ref val) => {
                    action = "S".to_string();
                    if let Val::Tuple(e) = val {
                        if let (Val::Integer(sent_event), Val::Integer(origin)) = (&e[0], &e[1]) {
                            origin_name = self
                                .fsm_indexes
                                .get(&(*origin as usize))
                                .unwrap()
                                .to_owned();
                            event_name =
                                self.events.get(&(*sent_event as usize)).unwrap().to_owned();
                        } else {
                            panic!("events should be pairs");
                        }
                    } else {
                        panic!("events should be pairs");
                    }
                }
                channel_system::EventType::Receive(ref val) => {
                    action = "R".to_string();
                    if let Val::Tuple(e) = val {
                        if let (Val::Integer(sent_event), Val::Integer(origin)) = (&e[0], &e[1]) {
                            origin_name = self
                                .fsm_indexes
                                .get(&(*origin as usize))
                                .unwrap()
                                .to_owned();
                            event_name =
                                self.events.get(&(*sent_event as usize)).unwrap().to_owned();
                        } else {
                            panic!("events should be pairs");
                        }
                    } else {
                        panic!("events should be pairs");
                    }
                }
                channel_system::EventType::ProbeEmptyQueue
                | channel_system::EventType::ProbeFullQueue => return,
            }
        } else if self.int_queues.contains(&event.channel) {
            origin_name = self.fsm_names.get(&event.pg_id).unwrap().to_owned();
            target_name = origin_name.clone();
            match event.event_type {
                channel_system::EventType::Send(ref val) => {
                    action = "S".to_string();
                    if let Val::Integer(sent_event) = val {
                        event_name = self.events.get(&(*sent_event as usize)).unwrap().to_owned();
                    } else {
                        panic!("events should be indexed by integer");
                    }
                }
                channel_system::EventType::Receive(ref val) => {
                    action = "R".to_string();
                    if let Val::Integer(sent_event) = val {
                        event_name = self.events.get(&(*sent_event as usize)).unwrap().to_owned();
                    } else {
                        panic!("events should be indexed by integer");
                    }
                }
                channel_system::EventType::ProbeEmptyQueue
                | channel_system::EventType::ProbeFullQueue => return,
            }
        } else {
            event_name = String::new();
            param_name = String::new();
            match event.event_type {
                channel_system::EventType::Send(ref val) => {
                    origin_name = self.fsm_names.get(&event.pg_id).unwrap().to_owned();
                    target_name = format!("{:?}", event.channel);
                    param_value = format!("{val:?}");
                }
                channel_system::EventType::Receive(ref val) => {
                    origin_name = format!("{:?}", event.channel);
                    target_name = self.fsm_names.get(&event.pg_id).unwrap().to_owned();
                    param_value = format!("{val:?}");
                }
                channel_system::EventType::ProbeEmptyQueue
                | channel_system::EventType::ProbeFullQueue => return,
            }
        }

        if let Some(writer) = self.writer.as_mut() {
            writer
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
                    .chain(
                        state
                            .iter()
                            .map(|b| String::from(if *b { "T" } else { "F" })),
                    ),
                )
                .expect("write record");
        } else {
            panic!("missing writer");
        }
    }

    fn finalize(&mut self, success: Option<bool>) {
        if let Some(writer) = self.writer.as_mut() {
            writer.flush().expect("flush csv content");
        }

        let folder = match success {
            Some(true) => "./traces/success/",
            Some(false) => "./traces/failure/",
            None => "./traces/undetermined/",
        };

        let mut new_path = PathBuf::from_str(folder).expect("create folder");
        new_path.push(self.path.file_name().expect("file name"));
        std::fs::rename(&self.path, new_path).expect("renaming");
    }
}
