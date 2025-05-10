/*This file is not part of our library, we added it to integrate our library with scan. */
use super::{ecmascript, ATTR_EVENT, ATTR_EXPR, ATTR_PARAM};
use crate::parser::{attrs, ParserError, ATTR_ID, ATTR_TARGET, ATTR_TYPE};
use anyhow::{anyhow, bail, Context};
use boa_ast::scope::Scope;
use boa_interner::Interner;
use log::{error, info, trace};
use quick_xml::{events::Event, Reader};
use scan_core::Pmtl;
use std::{collections::HashMap, io::BufRead};

const TAG_PORTS: &str = "ports";
const TAG_PORT: &str = "scxml_event_send";
const TAG_PROPERTIES: &str = "properties";
const TAG_PROPERTY: &str = "property";
const TAG_GUARANTEES: &str = "guarantees";
const TAG_ASSUMES: &str = "assumes";
const TAG_STATE_VAR: &str = "state_var";
const TAG_EVENT_VAR: &str = "event_var";
const ATTR_ORIGIN: &str = "origin";
const ATTR_LOGIC: &str = "logic";

#[derive(Debug, Clone)]
enum PropertyTag {
    Ports,
    Port(String, String, String),
    Properties,
    Guarantees,
    Assumes,
}

#[derive(Debug, Clone)]
pub(crate) struct ParserPort {
    pub(crate) r#type: String,
    pub(crate) origin: String,
    pub(crate) target: String,
    pub(crate) event: String,
    pub(crate) param: Option<(String, boa_ast::Expression)>,
}

impl From<&PropertyTag> for &'static str {
    fn from(value: &PropertyTag) -> Self {
        match value {
            PropertyTag::Properties => TAG_PROPERTIES,
            PropertyTag::Guarantees => TAG_GUARANTEES,
            PropertyTag::Assumes => TAG_ASSUMES,
            PropertyTag::Ports => TAG_PORTS,
            PropertyTag::Port(_, _, _) => TAG_PORT,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Properties {
    pub(crate) ports: HashMap<String, ParserPort>,
    pub(crate) predicates: Vec<boa_ast::Expression>,
    pub(crate) guarantees: Vec<(String, Pmtl<usize>)>,
    pub(crate) assumes: Vec<(String, Pmtl<usize>)>,
}

impl Properties {
    pub fn new() -> Self {
        Properties {
            ports: HashMap::new(),
            predicates: Vec::new(),
            guarantees: Vec::new(),
            assumes: Vec::new(),
        }
    }

    pub fn parse<R: BufRead>(
        &mut self,
        reader: &mut Reader<R>,
        interner: &mut Interner,
    ) -> anyhow::Result<()> {
        let mut buf = Vec::new();
        let mut stack: Vec<PropertyTag> = Vec::new();
        info!("parsing properties");
        loop {
            match reader
                .read_event_into(&mut buf)
                .context("failed reading event")?
            {
                Event::Start(tag) => {
                    let tag_name = tag.name();
                    let tag_name = std::str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' open tag");
                    match tag_name {
                        TAG_PROPERTIES if stack.is_empty() => {
                            stack.push(PropertyTag::Properties);
                        }
                        TAG_PORTS
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Properties)) =>
                        {
                            stack.push(PropertyTag::Ports);
                        }
                        TAG_PORT
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Ports)) =>
                        {
                            let attrs = attrs(tag, &[ATTR_EVENT, ATTR_ORIGIN, ATTR_TARGET], &[])
                                .with_context(|| {
                                    format!("failed to parse '{}' tag attributes", TAG_PORT)
                                })?;
                            stack.push(PropertyTag::Port(
                                attrs[ATTR_EVENT].clone(),
                                attrs[ATTR_ORIGIN].clone(),
                                attrs[ATTR_TARGET].clone(),
                            ));
                        }
                        TAG_GUARANTEES
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Properties)) =>
                        {
                            stack.push(PropertyTag::Guarantees);
                        }
                        TAG_ASSUMES
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Properties)) =>
                        {
                            stack.push(PropertyTag::Assumes);
                        }
                        _ => {
                            error!(target: "parser", "unknown or unexpected start tag '{tag_name}'");
                            bail!(ParserError::UnexpectedStartTag(tag_name.to_string()));
                        }
                    }
                }
                Event::End(tag) => {
                    let tag_name = &*reader.decoder().decode(tag.name().into_inner())?;
                    if stack.pop().is_some_and(|state| Into::<&str>::into(&state) == tag_name) {
                        trace!(target: "parser", "end tag '{}'", tag_name);
                    } else {
                        error!(target: "parser", "unknown or unexpected end tag '{tag_name}'");
                        bail!(ParserError::UnexpectedEndTag(tag_name.to_string()));
                    }
                }
                Event::Empty(tag) => {
                    let tag_name = tag.name();
                    let tag_name = std::str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' empty tag");
                    match tag_name {
                        TAG_EVENT_VAR
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Port(_, _, _))) =>
                        {
                            if let Some(PropertyTag::Port(event, origin, target)) = stack.last() {
                                let attrs = attrs(tag, &[ATTR_ID], &[]).with_context(|| {
                                    format!("failed to parse '{}' tag attributes", TAG_EVENT_VAR)
                                })?;
                                let id = attrs[ATTR_ID].clone();
                                self.ports.insert(
                                    id,
                                    ParserPort {
                                        origin: origin.clone(),
                                        target: target.clone(),
                                        event: event.clone(),
                                        param: None,
                                        r#type: "bool".to_string(),
                                    },
                                );
                            } else {
                                unreachable!("A port must be on top of stack");
                            }
                        }
                        TAG_STATE_VAR
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Port(_, _, _))) =>
                        {
                            if let Some(PropertyTag::Port(event, origin, target)) = stack.last_mut()
                            {
                                let attrs =
                                    attrs(tag, &[ATTR_ID, ATTR_PARAM, ATTR_EXPR, ATTR_TYPE], &[])
                                        .with_context(|| {
                                        format!(
                                            "failed to parse '{}' tag attributes",
                                            TAG_STATE_VAR
                                        )
                                    })?;
                                let expression = ecmascript(
                                    attrs[ATTR_EXPR].as_str(),
                                    &Scope::new_global(),
                                    interner,
                                )
                                .with_context(|| {
                                    format!(
                                        "failed parsing expression in '{}' attribute",
                                        ATTR_EXPR
                                    )
                                })?;
                                let id = attrs[ATTR_ID].clone();
                                self.ports.insert(
                                    id,
                                    ParserPort {
                                        origin: origin.clone(),
                                        target: target.clone(),
                                        event: event.clone(),
                                        param: Some((attrs[ATTR_PARAM].clone(), expression)),
                                        r#type: attrs[ATTR_TYPE].clone(),
                                    },
                                );
                            } else {
                                unreachable!("A port must be on top of stack");
                            }
                        }
                        TAG_PROPERTY => {
                            let attrs = attrs(tag, &[ATTR_ID, ATTR_EXPR], &[ATTR_LOGIC])
                                .with_context(|| {
                                    format!("failed to parse '{}' tag attributes", TAG_PROPERTY)
                                })?;
                            let id = attrs[ATTR_ID].to_owned();
                            let expr = attrs[ATTR_EXPR].as_str();
                            let formula = super::rye::parse(expr)
                                .map_err(|err| anyhow!(err))
                                .with_context(|| {
                                    format!("failed to parse '{}' Rye expression", expr)
                                })?;
                            let property =
                                parse_predicates(formula, &mut self.predicates, interner)
                                    .context("failed to parse predicates in Rye expression")?;
                            if self.assumes.iter().any(|(i, _)| i == &id) || self.guarantees.iter().any(|(i, _)| i == &id) {
                                bail!("property defined multiple times");
                            }
                            match stack.last() {
                                Some(PropertyTag::Guarantees) => self.guarantees.push((id, property)),
                                Some(PropertyTag::Assumes) => self.assumes.push((id, property)),
                                _ => bail!("'{TAG_PROPERTY}' tag found outside '{TAG_GUARANTEES}' or '{TAG_ASSUMES}'"),
                            }
                        }
                        _ => {
                            error!(target: "parser", "unknown or unexpected empty tag '{tag_name}'");
                            bail!(ParserError::UnexpectedTag(tag_name.to_string()));
                        }
                    }
                }
                // Ignore comments
                Event::Comment(_)
                // Ignore XML declaration
                | Event::Decl(_) => continue,
                Event::Text(t) => {
                    let text = &*reader.decoder().decode(t.as_ref())?;
                    if !text.trim().is_empty() {
                        error!(target: "parser", "text content not supported");
                        bail!("text content not supported");
                    }
                }
                Event::CData(_) => {
                    error!(target: "parser", "CData not supported");
                    bail!("CData not supported");
                }
                Event::PI(_) => {
                    error!(target: "parser", "Processing Instructions not supported");
                    bail!("Processing Instructions not supported");
                }
                Event::DocType(_) => {
                    error!(target: "parser", "DocType not supported");
                    bail!("DocType not supported");
                }
                // exits the loop when reaching end of file
                Event::Eof => {
                    info!(target: "parser", "parsing completed");
                    break;
                }
            }
            // if we don't keep a borrow elsewhere, we can clear the buffer to keep memory usage low
            buf.clear();
        }
        if let Some(tag) = stack.pop() {
            Err(anyhow!("unclosed tag {}", Into::<&str>::into(&tag)))
        } else {
            Ok(())
        }
    }
}

fn parse_predicates(
    formula: Pmtl<String>,
    predicates: &mut Vec<boa_ast::Expression>,
    interner: &mut Interner,
) -> anyhow::Result<Pmtl<usize>> {
    match formula {
        Pmtl::True => Ok(Pmtl::True),
        Pmtl::False => Ok(Pmtl::False),
        Pmtl::Atom(expr) => {
            let pred = ecmascript(&expr, &Scope::new_global(), interner)?;
            let idx = predicates.len();
            predicates.push(pred);
            Ok(Pmtl::Atom(idx))
        }
        Pmtl::And(vec) => vec
            .into_iter()
            .map(|f| parse_predicates(f, predicates, interner))
            .collect::<Result<Vec<_>, _>>()
            .map(Pmtl::And),
        Pmtl::Or(vec) => vec
            .into_iter()
            .map(|f| parse_predicates(f, predicates, interner))
            .collect::<Result<Vec<_>, _>>()
            .map(Pmtl::Or),
        Pmtl::Not(pmtl) => {
            parse_predicates(*pmtl, predicates, interner).map(|f| Pmtl::Not(Box::new(f)))
        }
        Pmtl::Implies(args) => {
            let (lhs, rhs) = *args;
            Ok(Pmtl::Implies(Box::new((
                parse_predicates(lhs, predicates, interner)?,
                parse_predicates(rhs, predicates, interner)?,
            ))))
        }
        Pmtl::Historically(pmtl, l, u) => parse_predicates(*pmtl, predicates, interner)
            .map(|f| Pmtl::Historically(Box::new(f), l, u)),
        Pmtl::Once(pmtl, l, u) => {
            parse_predicates(*pmtl, predicates, interner).map(|f| Pmtl::Once(Box::new(f), l, u))
        }
        Pmtl::Since(args, l, u) => {
            let (lhs, rhs) = *args;
            Ok(Pmtl::Since(
                Box::new((
                    parse_predicates(lhs, predicates, interner)?,
                    parse_predicates(rhs, predicates, interner)?,
                )),
                l,
                u,
            ))
        }
    }
}
