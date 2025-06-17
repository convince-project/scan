use super::JaniModelData;
use scan_core::{RunOutcome, Time, Tracer, Val, program_graph::Action};
use std::{
    env::current_dir,
    fs::{File, create_dir, create_dir_all, exists, remove_file, rename},
    path::PathBuf,
    sync::{Arc, atomic::AtomicU32},
};

pub struct TracePrinter {
    index: Arc<AtomicU32>,
    path: PathBuf,
    writer: Option<csv::Writer<flate2::write::GzEncoder<File>>>,
    model: Arc<JaniModelData>,
}

impl TracePrinter {
    const FOLDER: &str = "traces";
    const TEMP: &str = ".temp";
    const SUCCESSES: &str = "successes";
    const FAILURES: &str = "failures";
    const HEADER: [&str; 2] = ["Time", "Action"];

    pub fn new(model: Arc<JaniModelData>) -> Self {
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

impl Tracer<Action> for TracePrinter {
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

    fn trace<'a, I: IntoIterator<Item = &'a Val>>(
        &mut self,
        action: &Action,
        time: Time,
        ports: I,
    ) {
        let time = time.to_string();
        let action_name = self.model.actions.get(action).cloned().unwrap_or_default();

        self.writer
            .as_mut()
            .unwrap()
            .write_record(
                [time, action_name]
                    .into_iter()
                    .chain(ports.into_iter().map(format_val)),
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
                    // new_path.push(self.model.guarantees.get(violation).unwrap());
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
