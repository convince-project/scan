use std::{collections::HashMap, io::BufRead};

use anyhow::anyhow;
use log::{error, info, trace, warn};
use quick_xml::{
    events::{
        self,
        attributes::{AttrError, Attribute},
        Event,
    },
    Reader,
};
use std::str;

use crate::{
    parser::ConvinceTag, ParserError, ParserErrorType, ATTR_ID, TAG_DATA_TYPE_LIST,
    TAG_ENUMERATION, TAG_LABEL,
};

#[derive(Debug, Clone)]
pub enum OmgType {
    Boolean,
    Int32,
    Structure(),
    Enumeration(Vec<String>),
}

#[derive(Debug, Clone)]
pub struct OmgTypes {
    pub(crate) types: HashMap<String, OmgType>,
}

impl OmgTypes {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

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
                            let id = self.parse_id(tag, reader)?;
                            self.types
                                .insert(id.to_owned(), OmgType::Enumeration(Vec::new()));
                            stack.push(ConvinceTag::Enumeration(id));
                        }
                        // Unknown tag: skip till maching end tag
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
                        return Err(anyhow::Error::new(ParserError(
                            reader.buffer_position(),
                            ParserErrorType::UnexpectedEndTag(tag_name.to_string()),
                        )));
                    }
                }
                Event::Empty(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' empty tag");
                    // let tag_name = ConvinceTag::from(tag_name.as_str());
                    match tag_name {
                        TAG_LABEL
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ConvinceTag::Enumeration(_))) =>
                        {
                            if let Some(ConvinceTag::Enumeration(id)) = stack.last() {
                                let label = self.parse_id(tag, reader)?;
                                self.types.entry(id.to_owned()).and_modify(|t| {
                                    if let OmgType::Enumeration(labels) = t {
                                        labels.push(label);
                                    }
                                });
                            }
                        }
                        // Unknown tag: skip till maching end tag
                        _ => {
                            warn!("unknown or unexpected tag {tag_name:?}, skipping");
                            continue;
                        }
                    }
                }
                Event::Text(_) => continue,
                Event::Comment(_) => continue,
                Event::CData(_) => todo!(),
                Event::Decl(_) => todo!(), // parser.parse_xml_declaration(tag)?,
                Event::PI(_) => todo!(),
                Event::DocType(_) => todo!(),
                // exits the loop when reaching end of file
                Event::Eof => {
                    info!("parsing completed");
                    if !stack.is_empty() {
                        return Err(anyhow!(ParserError(
                            reader.buffer_position(),
                            ParserErrorType::UnclosedTags,
                        )));
                    }
                    break;
                }
            }
            // if we don't keep a borrow elsewhere, we can clear the buffer to keep memory usage low
            buf.clear();
        }

        Ok(())
    }

    fn parse_id<R: BufRead>(
        &mut self,
        tag: events::BytesStart<'_>,
        reader: &mut Reader<R>,
    ) -> anyhow::Result<String> {
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
                    return Err(anyhow::Error::new(ParserError(
                        reader.buffer_position(),
                        ParserErrorType::UnknownKey(key.to_owned()),
                    )));
                }
            }
        }
        id.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_ID.to_string())
        )))
    }
}
