//! Parser for SCAN's XML specification format.

mod fsm;
mod omg_types;
mod property;
mod vocabulary;

pub use self::fsm::*;
pub use self::omg_types::*;
pub use self::property::*;
pub use self::vocabulary::*;
use anyhow::{anyhow, bail, Context};
use boa_interner::Interner;
use log::warn;
use log::{error, info, trace};
use quick_xml::events::Event;
use quick_xml::Reader;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::Seek;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("unknown or unexpected empty tag `{0}`")]
    UnexpectedTag(String),
    #[error("unknown or unexpected start tag `{0}`")]
    UnexpectedStartTag(String),
    #[error("unknown or unexpected end tag `{0}`")]
    UnexpectedEndTag(String),
    #[error("missing required attribute `{0}`")]
    MissingAttr(String),
    #[error("unknown or unexpected attribute key `{0}`")]
    UnknownAttrKey(String),
    #[error("missing `expr` attribute")]
    MissingExpr,
    #[error("open tags have not been closed")]
    UnclosedTags,
    #[error("error parsing EcmaScript code")]
    EcmaScriptParsing,
    #[error("type annotation missing")]
    NoTypeAnnotation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ConvinceTag {
    Specification,
    Model,
    ProcessList,
    DataTypeList,
    Enumeration(String),
    Structure(String),
}

impl From<ConvinceTag> for &'static str {
    fn from(value: ConvinceTag) -> Self {
        match value {
            ConvinceTag::Specification => TAG_SPECIFICATION,
            ConvinceTag::Model => TAG_MODEL,
            ConvinceTag::ProcessList => TAG_PROCESS_LIST,
            ConvinceTag::DataTypeList => TAG_DATA_TYPE_LIST,
            ConvinceTag::Enumeration(_) => TAG_ENUMERATION,
            ConvinceTag::Structure(_) => TAG_STRUCT,
        }
    }
}

fn attrs(
    tag: quick_xml::events::BytesStart<'_>,
    keys: &[&str],
    opt_keys: &[&str],
) -> anyhow::Result<HashMap<String, String>> {
    let mut attrs = HashMap::new();
    for attr in tag.attributes() {
        let attr = attr?;
        let key = String::from_utf8(attr.key.into_inner().to_vec())?;
        if keys.contains(&key.as_str()) || opt_keys.contains(&key.as_str()) {
            let val = String::from_utf8(attr.value.into_owned())?;
            attrs.insert(key, val);
        } else {
            error!(target: "parsing", "found unknown attribute '{key}'");
            bail!(ParserError::UnknownAttrKey(key.to_string()));
        }
    }
    for key in keys {
        if !attrs.contains_key(*key) {
            error!(target: "parsing", "missing required attribute '{key}'");
            bail!(ParserError::MissingAttr(key.to_string()));
        }
    }
    Ok(attrs)
}

fn count_lines<R: BufRead + Seek>(mut reader: Reader<R>) -> usize {
    let end_pos = reader.buffer_position();
    reader.get_mut().rewind().unwrap();
    reader.into_inner().take(end_pos).lines().count()
}

/// Represents a model specified in the CONVINCE-XML format.
#[derive(Debug)]
pub struct Parser {
    pub(crate) process_list: HashMap<String, Scxml>,
    pub(crate) types: OmgTypes,
    pub(crate) properties: Properties,
    pub(crate) interner: Interner,
    pub(crate) scope: boa_ast::scope::Scope,
}

impl Parser {
    /// Builds a [`Parser`] representation by parsing the given main file of a model specification in the CONVINCE-XML format,
    /// or a folder containing the required source files.
    ///
    /// Fails if the parsed content contains syntactic errors.
    pub fn parse(path: &Path) -> anyhow::Result<Self> {
        info!(target: "parsing", "creating parser");
        let mut parser = Parser {
            process_list: HashMap::new(),
            types: OmgTypes::new(),
            properties: Properties::new(),
            interner: Interner::new(),
            scope: boa_ast::scope::Scope::new_global(),
        };
        if path.is_dir() {
            info!(target: "parsing", "parsing directory '{}'", path.display());
            parser.parse_directory(path)?;
        } else {
            info!(target: "parsing", "parsing main model file '{}'", path.display());
            let mut reader = Reader::from_file(path).with_context(|| {
                format!("failed to create reader from file '{}'", path.display())
            })?;
            let parent = path.parent().ok_or(anyhow!(
                "failed to take parent directory of '{}'",
                path.display()
            ))?;
            parser.parse_main(&mut reader, parent).with_context(|| {
                format!(
                    "failed to parse model specification at line {} in '{}'",
                    count_lines(reader),
                    path.display(),
                )
            })?;
        }
        Ok(parser)
    }

    fn parse_directory(&mut self, path: &Path) -> anyhow::Result<()> {
        for entry in std::fs::read_dir(path)
            .with_context(|| format!("failed to read directory '{}'", path.display()))?
        {
            let path = entry.context("failed to read directory entry")?.path();
            if path.is_dir() {
                self.parse_directory(&path)?;
            } else {
                self.parse_file(&path)?;
            }
        }
        Ok(())
    }

    fn parse_file(&mut self, path: &Path) -> anyhow::Result<()> {
        if path.is_dir() {
            bail!("path '{}' is a directory", path.display());
        } else if let Some(ext) = path.extension() {
            let ext = ext
                .to_str()
                .ok_or(anyhow!("failed file extension conversion to string"))?;
            match ext {
                "scxml" => {
                    info!("creating reader from file '{}'", path.display());
                    let mut reader = Reader::from_file(path).with_context(|| {
                        format!("failed to create reader from file '{}'", path.display())
                    })?;
                    let fsm = Scxml::parse(&mut reader, &mut self.interner, &self.scope)
                        .with_context(|| {
                            format!(
                                "failed to parse fsm at line {} in '{}'",
                                count_lines(reader),
                                path.display(),
                            )
                        })?;
                    self.process_list.insert(fsm.id.to_owned(), fsm);
                }
                "xml" => {
                    info!("creating reader from file '{}'", path.display());
                    let mut reader = Reader::from_file(path).with_context(|| {
                        format!("failed to create reader from file '{}'", path.display())
                    })?;
                    self.properties =
                        Properties::parse(&mut reader, &self.scope, &mut self.interner)
                            .with_context(|| {
                                format!(
                                    "failed to parse properties at line {} in '{}'",
                                    count_lines(reader),
                                    path.display(),
                                )
                            })?;
                }
                _ => {
                    warn!(target: "parsing", "unknown file extension '{}'", ext);
                }
            }
        }
        Ok(())
    }

    fn parse_main<R: BufRead>(
        &mut self,
        reader: &mut Reader<R>,
        parent: &Path,
    ) -> anyhow::Result<()> {
        let mut buf = Vec::new();
        let mut stack = Vec::new();
        loop {
            match reader
                .read_event_into(&mut buf)
                .context("failed reading event")?
            {
                Event::Start(tag) => {
                    let key = &*reader.decoder().decode(tag.name().into_inner())?;
                    trace!(target: "parsing", "start tag '{key}'");
                    match key {
                        TAG_SPECIFICATION if stack.is_empty() => {
                            stack.push(ConvinceTag::Specification);
                        }
                        TAG_MODEL if stack.last().is_some_and(|e| *e == ConvinceTag::Specification) => {
                            stack.push(ConvinceTag::Model);
                        }
                        TAG_PROCESS_LIST if stack.last().is_some_and(|e| *e == ConvinceTag::Model) => {
                            stack.push(ConvinceTag::ProcessList);
                        }
                        _ => {
                            error!(target: "parsing", "unknown or unexpected start tag '{key}'");
                            bail!(ParserError::UnexpectedStartTag(key.to_string()));
                        }
                    }
                }
                Event::End(tag) => {
                    let key = &*reader.decoder().decode(tag.name().into_inner())?;
                    if stack.pop().is_some_and(|state| Into::<&str>::into(state) == key) {
                        trace!(target: "parsing", "end tag '{}'", key);
                    } else {
                        error!(target: "parsing", "unknown or unexpected end tag '{key}'");
                        bail!(ParserError::UnexpectedEndTag(key.to_string()));
                    }
                }
                Event::Empty(tag) => {
                    let key = &*reader.decoder().decode(tag.name().into_inner())?;
                    trace!(target: "parsing", "empty tag '{key}'");
                    match key {
                        TAG_TYPES if stack.last().is_some_and(|e| *e == ConvinceTag::Specification) => {
                            let attrs = attrs(
                                tag,
                                &[ATTR_PATH],
                                &[],
                            )
                            .context("failed to parse 'types' tag attributes")?;
                            let mut path = parent.to_owned();
                            path.extend(&PathBuf::from(attrs.get(ATTR_PATH).unwrap()));
                            info!(
                                "creating reader from file '{}'",
                                path.display()
                            );
                            let mut reader = Reader::from_file(path.clone())?;
                            self.types.parse(&mut reader)
                                .with_context(|| format!("failed to parse types specification at line {} in '{}'", count_lines(reader), path.display()))?;
                        }
                        TAG_PROPERTIES if stack.last().is_some_and(|e| *e == ConvinceTag::Specification) => {
                            let attrs = attrs(tag, &[ATTR_PATH], &[])
                                .context("failed to parse 'properties' tag attributes")?;
                            let mut path = parent.to_owned();
                            path.extend(&PathBuf::from(attrs.get(ATTR_PATH).unwrap()));
                            info!("creating reader from file '{}'", path.display());
                            let mut reader = Reader::from_file(&path).with_context(|| {
                                format!("failed to create reader from file '{}'", path.display())
                            })?;
                            self.properties =
                                Properties::parse(&mut reader, &self.scope, &mut self.interner)
                                    .with_context(|| {
                                        format!(
                                            "failed to parse properties at line {} in '{}'",
                                            count_lines(reader),
                                            path.display(),
                                        )
                                    })?;
                        }
                        TAG_PROCESS if stack.last().is_some_and(|e| *e == ConvinceTag::ProcessList) => {
                            let attrs = attrs(
                                tag,
                                &[ATTR_ID, ATTR_PATH],
                                &[ATTR_MOC],
                            )
                            .context("failed to parse 'process' tag attributes")?;
                            if let Some(moc) = attrs.get(ATTR_MOC) {
                                if moc != "fsm" {
                                    bail!("unknown moc {moc}");
                                }
                            }
                            let process_id = attrs.get(ATTR_ID).unwrap().clone();
                            if self.process_list.contains_key(&process_id) {
                                bail!("process '{process_id}' declared multiple times");
                            }
                            let mut path = parent.to_owned();
                            path.extend(&PathBuf::from(attrs.get(ATTR_PATH).unwrap()));
                            info!(
                                "creating reader from file '{}' for fsm '{process_id}'",
                                path.display()
                            );
                            let mut reader = Reader::from_file(path.clone())?;
                            let fsm = Scxml::parse(&mut reader, &mut self.interner, &self.scope)
                                .with_context(|| format!("failed to parse fsm at line {} in '{}'", count_lines(reader), path.display()))?;
                            // Add process to list and check that no process was already in the list under the same name
                            if self.process_list.insert(process_id.clone(), fsm).is_some() {
                                panic!("process added to list multiple times");
                            }
                        }
                        _ => {
                            error!(target: "parsing", "unknown or unexpected empty tag '{key}'");
                            bail!(ParserError::UnexpectedTag(key.to_string()));
                        }
                    }
                }
                // Ignore comments
                Event::Comment(_)
                // Ignore XML declaration
                | Event::Decl(_) => continue,
                // Ignore text between tags
                Event::Text(t) => {
                    let text = &*reader.decoder().decode(t.as_ref())?;
                    if !text.trim().is_empty() {
                        error!(target: "parsing", "text content not supported");
                        bail!("text content not supported");
                    }
                }
                Event::CData(_) => {
                    error!(target: "parsing", "CData not supported");
                    bail!("CData not supported");
                }
                Event::PI(_) => {
                    error!(target: "parsing", "Processing Instructions not supported");
                    bail!("Processing Instructions not supported");
                }
                Event::DocType(_) => {
                    error!(target: "parsing", "DocType not supported");
                    bail!("DocType not supported");
                }
                // exits the loop when reaching end of file
                Event::Eof => {
                    info!(target: "parsing", "parsing completed");
                    break;
                }
            }
            // if we don't keep a borrow elsewhere, we can clear the buffer to keep memory usage low
            buf.clear();
        }
        Ok(())
    }
}
