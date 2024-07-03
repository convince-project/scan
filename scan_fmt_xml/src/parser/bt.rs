use super::vocabulary::*;
use crate::parser::ParserError;
use anyhow::anyhow;
use log::{error, info, trace, warn};
use quick_xml::events::attributes::{AttrError, Attribute};
use quick_xml::{events, events::Event, Reader};
use std::io::BufRead;
use std::str;

#[derive(Debug, Clone)]
pub enum BtNode {
    RSeq(Vec<BtNode>),
    RFbk(Vec<BtNode>),
    MSeq(Vec<BtNode>),
    MFbk(Vec<BtNode>),
    Invr(Box<BtNode>),
    LAct(String),
    LCnd(String),
}

#[derive(Debug, Clone)]
pub struct Bt {
    pub(crate) id: String,
    pub(crate) root: BtNode,
}

impl Bt {
    pub(super) fn parse_skill<R: BufRead>(reader: &mut Reader<R>) -> anyhow::Result<Vec<Self>> {
        let mut bts = Vec::new();
        let mut buf = Vec::new();
        info!("parsing bt");
        loop {
            match reader.read_event_into(&mut buf)? {
                Event::Start(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' open tag");
                    match tag_name {
                        TAG_ROOT => {
                            // TODO: parse `main_tree_to_execute`
                            // TODO: fix horrible recursion.
                            return Self::parse_skill(reader);
                        }
                        TAG_BEHAVIOR_TREE => {
                            // return Self::parse_skill(reader);
                            let id = Self::parse_bt(tag)
                                .map_err(|err| err.context(reader.error_position()))?;
                            let root = BtNode::parse_skill(reader)?
                                .pop()
                                .ok_or(ParserError::MissingBtRootNode)?;
                            let bt = Bt { id, root };
                            bts.push(bt);
                        }
                        // Unknown tag: skip till maching end tag
                        _ => {
                            warn!("unknown or unexpected tag {tag_name}, skipping");
                            reader.read_to_end_into(tag.to_end().into_owned().name(), &mut buf)?;
                        }
                    }
                }
                Event::End(_tag) => {
                    return Ok(bts);
                }
                Event::Empty(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    warn!("unknown or unexpected tag {tag_name:?}, skipping");
                }
                Event::Text(_) => continue,
                Event::Comment(_) => continue,
                Event::CData(_) => continue,
                Event::Decl(_) => continue, // parser.parse_xml_declaration(tag)?,
                Event::PI(_) => continue,
                Event::DocType(_) => continue,
                // exits the loop when reaching end of file
                Event::Eof => {
                    return Err(anyhow!(ParserError::UnclosedTags));
                }
            }
            // if we don't keep a borrow elsewhere, we can clear the buffer to keep memory usage low
            buf.clear();
        }
    }

    fn parse_bt(tag: events::BytesStart<'_>) -> anyhow::Result<String> {
        let mut id: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_BT_ID => {
                    id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_STATE}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }
        id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_ID.to_string())))
    }
}

impl BtNode {
    pub(super) fn parse_skill<R: BufRead>(reader: &mut Reader<R>) -> anyhow::Result<Vec<Self>> {
        let mut bts = Vec::new();
        let mut buf = Vec::new();
        info!("parsing bt");
        loop {
            match reader.read_event_into(&mut buf)? {
                Event::Start(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' open tag");
                    match tag_name {
                        // TAG_ROOT | TAG_BEHAVIOR_TREE => {
                        //     return Self::parse_skill(reader);
                        // }
                        TAG_REACTIVE_SEQUENCE => {
                            bts.push(BtNode::RSeq(
                                Self::parse_skill(reader)?.into_iter().collect(),
                            ));
                        }
                        TAG_REACTIVE_FALLBACK => {
                            bts.push(BtNode::RFbk(
                                Self::parse_skill(reader)?.into_iter().collect(),
                            ));
                        }
                        // Unknown tag: skip till maching end tag
                        _ => {
                            warn!("unknown or unexpected tag {tag_name}, skipping");
                            reader.read_to_end_into(tag.to_end().into_owned().name(), &mut buf)?;
                        }
                    }
                }
                Event::End(_tag) => {
                    return Ok(bts);
                }
                Event::Empty(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' empty tag");
                    // let tag_name = ConvinceTag::from(tag_name.as_str());
                    match tag_name {
                        TAG_ACTION => {
                            bts.push(
                                Self::parse_action(tag)
                                    .map_err(|err| err.context(reader.error_position()))?,
                            );
                        }
                        TAG_CONDITION => {
                            bts.push(
                                Self::parse_condition(tag)
                                    .map_err(|err| err.context(reader.error_position()))?,
                            );
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
                Event::CData(_) => continue,
                Event::Decl(_) => continue, // parser.parse_xml_declaration(tag)?,
                Event::PI(_) => continue,
                Event::DocType(_) => continue,
                // exits the loop when reaching end of file
                Event::Eof => {
                    return Err(anyhow!(ParserError::UnclosedTags,));
                }
            }
            // if we don't keep a borrow elsewhere, we can clear the buffer to keep memory usage low
            buf.clear();
        }
    }

    fn parse_action(tag: events::BytesStart<'_>) -> anyhow::Result<BtNode> {
        let mut id: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_BT_ID => {
                    id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_STATE}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }
        let id = id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_ID.to_string())))?;
        let action = BtNode::LAct(id);
        Ok(action)
    }

    fn parse_condition(tag: events::BytesStart<'_>) -> anyhow::Result<BtNode> {
        let mut id: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_BT_ID => {
                    id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_STATE}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }
        let id = id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_ID.to_string())))?;
        let condition = BtNode::LCnd(id);
        Ok(condition)
    }
}
