use std::{
    collections::{BTreeMap, HashMap},
    io::{BufRead, Read},
};

use anyhow::{Context, anyhow, bail};
use log::{error, info, trace, warn};
use quick_xml::{
    Reader,
    events::{
        self, Event,
        attributes::{AttrError, Attribute},
    },
};
use scan_core::Type;

use crate::parser::{ATTR_ID, TAG_DATA_TYPE_LIST, TAG_ENUMERATION, TAG_LABEL, TAG_STRUCT};
use crate::parser::{ATTR_TYPE, ConvinceTag, ParserError, TAG_FIELD};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OmgBaseType {
    Boolean,
    Int32,
    F64,
    Uri,
    String,
}

impl TryFrom<&str> for OmgBaseType {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "boolean" => Ok(OmgBaseType::Boolean),
            "bool" => Ok(OmgBaseType::Boolean),
            "int8" => Ok(OmgBaseType::Int32),
            "int16" => Ok(OmgBaseType::Int32),
            "int32" => Ok(OmgBaseType::Int32),
            "int64" => Ok(OmgBaseType::Int32),
            "uint8" => Ok(OmgBaseType::Int32),
            "uint16" => Ok(OmgBaseType::Int32),
            "uint32" => Ok(OmgBaseType::Int32),
            "uint64" => Ok(OmgBaseType::Int32),
            "string" => Ok(OmgBaseType::String),
            "float32" => Ok(OmgBaseType::F64),
            "float64" => Ok(OmgBaseType::F64),
            "URI" => Ok(OmgBaseType::Uri),
            _ => Err(anyhow!("unknown base type {value}")),
        }
    }
}

impl From<OmgBaseType> for Type {
    fn from(value: OmgBaseType) -> Self {
        match value {
            OmgBaseType::Boolean => Type::Boolean,
            OmgBaseType::Int32 => Type::Integer,
            OmgBaseType::F64 => Type::Float,
            OmgBaseType::Uri => Type::Integer,
            OmgBaseType::String => Type::Integer,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OmgType {
    Base(OmgBaseType),
    Array(OmgBaseType, Option<usize>),
    Custom(String),
}

impl OmgType {
    pub fn size(&self, omg_types: &OmgTypes) -> anyhow::Result<usize> {
        match self {
            OmgType::Base(_omg_base_type) => Ok(1),
            OmgType::Array(_omg_base_type, len) => len.ok_or(anyhow!("array length unknown")),
            OmgType::Custom(omg_name) => omg_types.size(omg_name),
        }
    }

    pub fn to_scan_types(&self, omg_types: &OmgTypes) -> anyhow::Result<Vec<Type>> {
        match self {
            OmgType::Base(omg_base_type) => Ok(vec![(*omg_base_type).into()]),
            OmgType::Array(omg_base_type, len) => len
                .ok_or(anyhow!("array length unknown"))
                .map(|len| vec![(*omg_base_type).into(); len]),
            OmgType::Custom(omg_name) => match omg_types
                .type_defs
                .get(omg_name)
                .ok_or_else(|| anyhow!("type {omg_name} unknown"))?
            {
                OmgTypeDef::Enumeration(_items) => Ok(vec![Type::Integer]),
                OmgTypeDef::Structure(fields) => fields
                    .values()
                    .map(|field| field.to_scan_types(omg_types))
                    .collect::<anyhow::Result<Vec<Vec<Type>>>>()
                    .map(|s| s.into_iter().flatten().collect()),
            },
        }
    }
}

impl TryFrom<&str> for OmgType {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.strip_suffix("[]").map_or_else(
            || OmgBaseType::try_from(value).map(OmgType::Base),
            |value| OmgBaseType::try_from(value).map(|t| OmgType::Array(t, None)),
        )
    }
}

#[derive(Debug, Clone)]
pub enum OmgTypeDef {
    Enumeration(Vec<String>),
    Structure(BTreeMap<String, OmgType>),
}

impl From<OmgBaseType> for OmgType {
    fn from(value: OmgBaseType) -> Self {
        OmgType::Base(value)
    }
}

#[derive(Debug, Clone)]
pub struct OmgTypes {
    pub(crate) type_defs: HashMap<String, OmgTypeDef>,
}

impl OmgTypes {
    pub fn new() -> Self {
        Self {
            type_defs: HashMap::new(),
        }
    }

    pub fn size(&self, omg_name: &str) -> anyhow::Result<usize> {
        match self.find_type(omg_name)? {
            OmgType::Base(_omg_base_type) => Ok(1),
            OmgType::Array(_omg_base_type, len) => len.ok_or(anyhow!("array of unknown len")),
            OmgType::Custom(_) => match self.type_defs.get(omg_name).unwrap() {
                OmgTypeDef::Enumeration(_items) => Ok(1),
                OmgTypeDef::Structure(fields) => {
                    fields.values().map(|omg_type| omg_type.size(self)).sum()
                }
            },
        }
    }

    pub fn find_type(&self, omg_name: &str) -> anyhow::Result<OmgType> {
        omg_name
            .try_into()
            .ok()
            .or_else(|| {
                self.type_defs
                    .get(omg_name)
                    .map(|_def| OmgType::Custom(omg_name.to_string()))
            })
            .ok_or_else(|| anyhow!("type {} not known", omg_name))
    }

    // pub fn to_scan_types(&self, omg_name: &str) -> anyhow::Result<Vec<Type>> {
    //     self.find_type(omg_name)
    //         .and_then(|omg_type| omg_type.to_scan_types(self))
    // }

    pub fn parse<R: BufRead>(&mut self, reader: &mut Reader<R>) -> anyhow::Result<()> {
        let mut buf = Vec::new();
        let mut stack = Vec::new();
        info!("begin parsing");
        loop {
            match reader.read_event_into(&mut buf)? {
                Event::Start(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' open tag");
                    match tag_name {
                        TAG_DATA_TYPE_LIST if stack.is_empty() => {
                            stack.push(ConvinceTag::DataTypeList);
                        }
                        TAG_ENUMERATION
                            if stack
                                .last()
                                .is_some_and(|tag| *tag == ConvinceTag::DataTypeList) =>
                        {
                            let id = self.parse_id(tag)?;
                            self.type_defs
                                .insert(id.to_owned(), OmgTypeDef::Enumeration(Vec::new()));
                            stack.push(ConvinceTag::Enumeration(id));
                        }
                        TAG_STRUCT
                            if stack
                                .last()
                                .is_some_and(|tag| *tag == ConvinceTag::DataTypeList) =>
                        {
                            let id = self.parse_id(tag)?;
                            self.type_defs
                                .insert(id.to_owned(), OmgTypeDef::Structure(BTreeMap::new()));
                            stack.push(ConvinceTag::Structure(id));
                        }
                        // Unknown tag: skip till matching end tag
                        _ => {
                            warn!("unknown or unexpected tag {tag_name}, skipping");
                            reader.read_to_end_into(tag.to_end().into_owned().name(), &mut buf)?;
                        }
                    }
                }
                Event::End(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    if stack.pop().is_some_and(|tag| <&str>::from(tag) == tag_name) {
                        trace!("'{tag_name}' end tag");
                    } else {
                        error!("unexpected end tag {tag_name}");
                        return Err(anyhow::Error::new(ParserError::UnexpectedEndTag(
                            tag_name.to_string(),
                        )));
                    }
                }
                Event::Empty(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' empty tag");
                    match tag_name {
                        TAG_LABEL
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ConvinceTag::Enumeration(_))) =>
                        {
                            if let Some(ConvinceTag::Enumeration(id)) = stack.last() {
                                let label = self.parse_id(tag)?;
                                let omg_type = self.type_defs.get_mut(id).unwrap();
                                if let OmgTypeDef::Enumeration(labels) = omg_type {
                                    labels.push(label.to_owned());
                                } else {
                                    panic!("unexpected type");
                                }
                            }
                        }
                        TAG_FIELD
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ConvinceTag::Structure(_))) =>
                        {
                            if let Some(ConvinceTag::Structure(id)) = stack.last() {
                                let (field_id, field_type) =
                                    self.parse_struct(tag).with_context(|| {
                                        format!("failed parsing field of struct {id}")
                                    })?;
                                let omg_type = self.type_defs.get_mut(id).unwrap();
                                if let OmgTypeDef::Structure(fields) = omg_type {
                                    fields.insert(field_id, field_type);
                                } else {
                                    panic!("unexpected type");
                                }
                            }
                        }
                        // Unknown tag: skip till matching end tag
                        _ => {
                            warn!("unknown or unexpected tag {tag_name:?}, skipping");
                            continue;
                        }
                    }
                }
                Event::Text(text) => {
                    let text = text.bytes().collect::<Result<Vec<u8>, std::io::Error>>()?;
                    let text = String::from_utf8(text)?;
                    if !text.trim().is_empty() {
                        error!(target: "parser", "text elements not allowed, ignoring");
                    }
                    continue;
                }
                Event::Comment(_comment) => continue,
                Event::CData(_) => {
                    bail!("CData not supported");
                }
                Event::Decl(_) => continue,
                Event::PI(_) => {
                    bail!("Processing Instructions not supported");
                }
                Event::DocType(_) => {
                    bail!("DocType not supported");
                }
                Event::Eof => {
                    info!("parsing completed");
                    if !stack.is_empty() {
                        bail!(ParserError::UnclosedTags);
                    }
                    break;
                }
                Event::GeneralRef(_bytes_ref) => {
                    bail!("General References not supported");
                }
            }
            // if we don't keep a borrow elsewhere, we can clear the buffer to keep memory usage low
            buf.clear();
        }

        Ok(())
    }

    fn parse_id(&mut self, tag: events::BytesStart<'_>) -> anyhow::Result<String> {
        let mut id: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_ID => {
                    id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    bail!(ParserError::UnknownAttrKey(key.to_owned()));
                }
            }
        }
        id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_ID.to_string())))
    }

    fn parse_struct(&mut self, tag: events::BytesStart<'_>) -> anyhow::Result<(String, OmgType)> {
        let mut id: Option<String> = None;
        let mut field_type: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_ID => {
                    id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_TYPE => {
                    field_type = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    bail!(ParserError::UnknownAttrKey(key.to_owned()));
                }
            }
        }
        let id = id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_ID.to_string())))?;
        let field_type =
            field_type.ok_or(anyhow!(ParserError::MissingAttr(ATTR_TYPE.to_string())))?;
        let field_type = self.find_type(field_type.as_str()).with_context(|| {
            format!("field {id} of type {field_type} not a supported base type")
        })?;
        Ok((id, field_type))
    }
}
