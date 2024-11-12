//! Parser for SCAN's XML specification format.

mod bt;
mod fsm;
mod omg_types;
mod property;
mod vocabulary;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::str::Utf8Error;

use anyhow::{anyhow, Context};
use log::{error, info, trace, warn};
use quick_xml::events::attributes::{AttrError, Attribute};
use quick_xml::events::Event;
use quick_xml::{Error as XmlError, Reader};
use thiserror::Error;

pub use self::bt::*;
pub use self::fsm::*;
pub use self::omg_types::*;
pub use self::property::*;
pub use self::vocabulary::*;
use scan_core::channel_system::*;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("reader failed")]
    Reader(#[from] XmlError),
    #[error("error from an attribute")]
    Attr(#[from] AttrError),
    #[error("unknown key: `{0}`")]
    UnknownKey(String),
    #[error("utf8 error")]
    Utf8(#[from] Utf8Error),
    #[error("channel system error")]
    Cs(#[from] CsError),
    #[error("unexpected end tag: `{0}`")]
    UnexpectedEndTag(String),
    #[error("missing `expr` attribute")]
    MissingExpr,
    #[error("missing attribute `{0}`")]
    MissingAttr(String),
    #[error("open tags have not been closed")]
    UnclosedTags,
    #[error("`{0}` has already been declared")]
    AlreadyDeclared(String),
    #[error("unknown model of computation: `{0}`")]
    UnknownMoC(String),
    #[error("behavior tree missing root node")]
    MissingBtRootNode,
    #[error("error parsing EcmaScript code")]
    EcmaScriptParsing,
    #[error("type annotation missing")]
    NoTypeAnnotation,
    #[error("provided path is not a file")]
    NotAFile,
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

#[derive(Debug)]
pub(crate) enum MoC {
    Fsm(Box<Fsm>),
    Bt(Bt),
}

#[derive(Debug)]
pub(crate) struct Process {
    pub(crate) moc: MoC,
}

/// Represents a model specified in the CONVINCE-XML format.
#[derive(Debug)]
pub struct Parser {
    root_folder: PathBuf,
    pub(crate) process_list: HashMap<String, Process>,
    pub(crate) types: OmgTypes,
    pub(crate) properties: Properties,
}

impl Parser {
    pub fn parse_folder(path: &Path) -> anyhow::Result<Parser> {
        let mut process_list = HashMap::new();
        let mut properties = Properties::new();
        if path.is_dir() {
            for entry in std::fs::read_dir(path)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_dir() {
                    // visit_dirs(&path, cb)?;
                } else if path
                    .extension()
                    .is_some_and(|ext| ext.to_str().unwrap() == "scxml")
                {
                    info!("creating reader from file {0}", path.display());
                    let mut reader = Reader::from_file(path)?;
                    let fsm = Fsm::parse(&mut reader)?;
                    process_list.insert(
                        fsm.scxml.id.to_owned(),
                        Process {
                            moc: MoC::Fsm(Box::new(fsm)),
                        },
                    );
                }
            }
            for entry in std::fs::read_dir(path)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_dir() {
                    // visit_dirs(&path, cb)?;
                } else if path
                    .extension()
                    .is_some_and(|ext| ext.to_str().unwrap() == "xml")
                {
                    info!("creating reader from file {0}", path.display());
                    let mut reader = Reader::from_file(path)?;
                    properties = Properties::parse(&mut reader)?;
                }
            }
        }
        Ok(Parser {
            root_folder: path.to_path_buf(),
            process_list,
            types: OmgTypes::new(),
            properties,
        })
    }

    /// Builds a [`Parser`] representation by parsing the given main file of a model specification in the CONVINCE-XML format.
    ///
    /// Fails if the parsed content contains syntactic errors.
    pub fn parse(path: &Path) -> anyhow::Result<Parser> {
        let mut reader = Reader::from_file(path)?;
        let root_folder = path.parent().ok_or(ParserError::NotAFile)?.to_path_buf();
        let mut spec = Parser {
            root_folder,
            process_list: HashMap::new(),
            types: OmgTypes::new(),
            properties: Properties::new(),
        };
        let mut buf = Vec::new();
        let mut stack = Vec::new();
        info!(target: "parsing", "begin parsing");
        info!(target: "parsing", "parsing main model file: {:?}", path.as_os_str());
        loop {
            match reader
                .read_event_into(&mut buf)
                .with_context(|| format!("parsing {:?}", path.as_os_str()))
                .with_context(|| format!("parser position {}", reader.error_position()))?
            {
                Event::Start(tag) => {
                    let tag_name = tag.name();
                    let tag_name = &*reader.decoder().decode(tag_name.as_ref())?;
                    trace!(target: "parsing", "open tag: '{tag_name}'");
                    match tag_name {
                        TAG_SPECIFICATION if stack.is_empty() => {
                            stack.push(ConvinceTag::Specification);
                        }
                        TAG_MODEL
                            if stack
                                .last()
                                .is_some_and(|tag| *tag == ConvinceTag::Specification) =>
                        {
                            stack.push(ConvinceTag::Model);
                        }
                        TAG_PROCESS_LIST
                            if stack.last().is_some_and(|tag| *tag == ConvinceTag::Model) =>
                        {
                            stack.push(ConvinceTag::ProcessList);
                        }
                        // Unknown tag: skip till maching end tag
                        _ => {
                            error!(target: "parsing", "unknown or unexpected tag {tag_name}, skipping");
                            reader
                                .read_to_end_into(tag.to_end().into_owned().name(), &mut buf)
                                .with_context(|| {
                                    format!("parser position {}", reader.error_position())
                                })?;
                        }
                    }
                }
                Event::End(tag) => {
                    let tag_name = tag.name();
                    let tag_name = &*reader.decoder().decode(tag_name.as_ref())?;
                    if stack.pop().is_some_and(|tag| <&str>::from(tag) == tag_name) {
                        trace!(target: "parsing", "end tag: '{tag_name}'");
                    } else {
                        error!(target: "parsing", "unexpected end tag {tag_name}");
                        return Err(anyhow::Error::new(ParserError::UnexpectedEndTag(
                            tag_name.to_string(),
                        )))
                        .with_context(|| format!("parser position {}", reader.buffer_position()));
                    }
                }
                Event::Empty(tag) => {
                    let tag_name = tag.name();
                    let tag_name = std::str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' empty tag");
                    // let tag_name = ConvinceTag::from(tag_name.as_str());
                    match tag_name {
                        TAG_PROCESS
                            if stack
                                .last()
                                .is_some_and(|tag| *tag == ConvinceTag::ProcessList) =>
                        {
                            spec.parse_process(tag).with_context(|| {
                                format!("parser position {}", reader.buffer_position())
                            })?;
                        }
                        TAG_TYPES
                            if stack
                                .last()
                                .is_some_and(|tag| *tag == ConvinceTag::Specification) =>
                        {
                            spec.parse_types(tag).with_context(|| {
                                format!("parser position {}", reader.buffer_position())
                            })?;
                        }
                        TAG_PROPERTIES
                            if stack
                                .last()
                                .is_some_and(|tag| *tag == ConvinceTag::Specification) =>
                        {
                            spec.parse_properties(tag).with_context(|| {
                                format!("parser position {}", reader.buffer_position())
                            })?;
                        }
                        // Unknown tag: skip till maching end tag
                        _ => {
                            warn!("unknown or unexpected tag {tag_name:?}, skipping");
                            continue;
                        }
                    }
                }
                // Ignore text between tags
                Event::Text(_) => continue,
                // Ignore comments
                Event::Comment(_) => continue,
                Event::CData(_) => {
                    return Err(anyhow!("CData not supported"))
                        .with_context(|| format!("parser position {}", reader.buffer_position()));
                }
                // Ignore XML declaration
                Event::Decl(_) => continue,
                Event::PI(_) => {
                    return Err(anyhow!("Processing Instructions not supported"))
                        .with_context(|| format!("parser position {}", reader.buffer_position()));
                }
                Event::DocType(_) => {
                    return Err(anyhow!("DocType not supported"))
                        .with_context(|| format!("parser position {}", reader.buffer_position()));
                }
                // exits the loop when reaching end of file
                Event::Eof => {
                    info!(target: "parsing", "parsing completed");
                    if !stack.is_empty() {
                        return Err(anyhow!(ParserError::UnclosedTags)).with_context(|| {
                            format!("parser position {}", reader.buffer_position())
                        });
                    }
                    break;
                }
            }
            // if we don't keep a borrow elsewhere, we can clear the buffer to keep memory usage low
            buf.clear();
        }
        Ok(spec)
    }

    fn parse_process(&mut self, tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<()> {
        let mut process_id: Option<String> = None;
        let mut moc: Option<String> = None;
        let mut path: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match std::str::from_utf8(attr.key.as_ref())? {
                ATTR_ID => {
                    process_id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_MOC => {
                    moc = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_PATH => {
                    path = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }
        let process_id =
            process_id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_ID.to_string())))?;
        let path = path.ok_or(anyhow!(ParserError::MissingAttr(ATTR_PATH.to_string())))?;
        let mut root_path = self.root_folder.clone();
        root_path.extend(&PathBuf::from(path));
        let moc = moc.ok_or(anyhow!(ParserError::MissingAttr(ATTR_MOC.to_string())))?;
        let moc = match moc.as_str() {
            "fsm" => {
                info!("creating reader from file {0}", root_path.display());
                let mut reader = Reader::from_file(root_path)?;
                let fsm = Fsm::parse(&mut reader)?;
                MoC::Fsm(Box::new(fsm))
            }
            "bt" => {
                info!("creating reader from file {0}", root_path.display());
                let mut reader = Reader::from_file(root_path)?;
                let bt = Bt::parse_skill(&mut reader)?.pop().unwrap();
                MoC::Bt(bt)
            }
            moc => {
                return Err(anyhow!(ParserError::UnknownMoC(moc.to_string())));
            }
        };
        let process = Process { moc };
        // Add process to list and check that no process was already in the list under the same name
        if self
            .process_list
            .insert(process_id.to_owned(), process)
            .is_none()
        {
            Ok(())
        } else {
            Err(anyhow!(ParserError::AlreadyDeclared(process_id)))
        }
    }

    fn parse_types(&mut self, tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<()> {
        let mut path: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match std::str::from_utf8(attr.key.as_ref())? {
                ATTR_PATH => {
                    path = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned()),));
                }
            }
        }
        let path = path.ok_or(anyhow!(ParserError::MissingAttr(ATTR_PATH.to_string())))?;
        let mut root_path = self.root_folder.clone();
        root_path.extend(&PathBuf::from(path));
        info!("creating reader from file {0}", root_path.display());
        let mut reader = Reader::from_file(root_path)?;
        self.types.parse(&mut reader)?;
        Ok(())
    }

    fn parse_properties(&mut self, tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<()> {
        let mut path: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match std::str::from_utf8(attr.key.as_ref())? {
                ATTR_PATH => {
                    path = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned()),));
                }
            }
        }
        let path = path.ok_or(anyhow!(ParserError::MissingAttr(ATTR_PATH.to_string())))?;
        let mut root_path = self.root_folder.clone();
        root_path.extend(&PathBuf::from(path));
        info!("creating reader from file {0}", root_path.display());
        let mut reader = Reader::from_file(root_path)?;
        self.properties = Properties::parse(&mut reader)?;
        Ok(())
    }
}
