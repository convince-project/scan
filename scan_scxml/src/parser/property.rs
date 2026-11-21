use super::{ATTR_EVENT, ATTR_EXPR, ATTR_PARAM, ecmascript};
use crate::parser::{ATTR_ID, ATTR_TARGET, ATTR_TYPE, ParserError, attrs};
use anyhow::{Context, anyhow, bail};
use boa_ast::scope::Scope;
use boa_interner::Interner;
use log::{error, info, trace};
use quick_xml::{Reader, XmlVersion, events::Event};
use scan_ctl::{Ctlp, PathCtl};
use scan_pmtl::Pmtl;
use std::{collections::BTreeMap, io::BufRead};

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
const PMTL: &str = "pmtl";
const CTLP: &str = "ctlp";

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
    // pub(crate) r#type: OmgType,
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
pub enum Property {
    Pmtl(Pmtl<usize>),
    Ctlp(Ctlp<usize>),
}

#[derive(Debug, Clone)]
pub struct Properties {
    pub(crate) ports: BTreeMap<String, ParserPort>,
    pub(crate) predicates: Vec<boa_ast::Expression>,
    pub(crate) guarantees: Vec<(String, Property)>,
    pub(crate) assumes: Vec<(String, Property)>,
}

impl Properties {
    pub fn new() -> Self {
        Properties {
            ports: BTreeMap::new(),
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
        let mut xml_version = XmlVersion::Implicit1_0;
        info!("parsing properties");
        loop {
            match reader
                .read_event_into(&mut buf)
                .context("failed reading event")?
            {
                Event::Start(tag) => {
                    let tag_name = tag.name().into_inner();
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
                            let attrs = attrs(
                                &tag,
                                &[ATTR_EVENT, ATTR_ORIGIN, ATTR_TARGET],
                                &[],
                                xml_version,
                            )
                            .with_context(|| {
                                format!("failed to parse '{TAG_PORT}' tag attributes")
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
                    let tag_name = tag.name().into_inner();
                    if stack
                        .pop()
                        .is_some_and(|state| Into::<&str>::into(&state) == tag_name)
                    {
                        trace!(target: "parser", "end tag '{tag_name}'");
                    } else {
                        error!(target: "parser", "unknown or unexpected end tag '{tag_name}'");
                        bail!(ParserError::UnexpectedEndTag(tag_name.to_string()));
                    }
                }
                Event::Empty(tag) => {
                    let tag_name = tag.name().into_inner();
                    trace!("'{tag_name}' empty tag");
                    match tag_name {
                        TAG_EVENT_VAR
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Port(_, _, _))) =>
                        {
                            if let Some(PropertyTag::Port(event, origin, target)) = stack.last() {
                                let attrs = attrs(&tag, &[ATTR_ID], &[], xml_version)
                                    .with_context(|| {
                                        format!("failed to parse '{TAG_EVENT_VAR}' tag attributes")
                                    })?;
                                let id = attrs[ATTR_ID].clone();
                                self.ports.insert(
                                    id,
                                    ParserPort {
                                        origin: origin.clone(),
                                        target: target.clone(),
                                        event: event.clone(),
                                        param: None,
                                        // r#type: OmgType::Base(OmgBaseType::Boolean),
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
                                let attrs = attrs(
                                    &tag,
                                    &[ATTR_ID, ATTR_PARAM, ATTR_EXPR, ATTR_TYPE],
                                    &[],
                                    xml_version,
                                )
                                .with_context(|| {
                                    format!("failed to parse '{TAG_STATE_VAR}' tag attributes")
                                })?;
                                let expression = ecmascript(
                                    attrs[ATTR_EXPR].as_str(),
                                    &Scope::new_global(),
                                    interner,
                                )
                                .with_context(|| {
                                    format!("failed parsing expression in '{ATTR_EXPR}' attribute")
                                })?;
                                let id = attrs[ATTR_ID].clone();
                                self.ports.insert(
                                    id,
                                    ParserPort {
                                        origin: origin.clone(),
                                        target: target.clone(),
                                        event: event.clone(),
                                        param: Some((attrs[ATTR_PARAM].clone(), expression)),
                                        // r#type: omg_types.find_type(&attrs[ATTR_TYPE])?,
                                    },
                                );
                            } else {
                                unreachable!("A port must be on top of stack");
                            }
                        }
                        TAG_PROPERTY => {
                            let attrs =
                                attrs(&tag, &[ATTR_ID, ATTR_EXPR], &[ATTR_LOGIC], xml_version)
                                    .with_context(|| {
                                        format!("failed to parse '{TAG_PROPERTY}' tag attributes")
                                    })?;
                            let id = attrs[ATTR_ID].to_owned();
                            if self.assumes.iter().any(|(i, _)| i == &id)
                                || self.guarantees.iter().any(|(i, _)| i == &id)
                            {
                                bail!("property {id} defined multiple times");
                            }
                            let expr = attrs[ATTR_EXPR].as_str();
                            let logic = attrs.get(ATTR_LOGIC).map_or(PMTL, |v| v.as_str());
                            let property = match logic {
                                CTLP => {
                                    let formula = super::rye_ctl::parse(expr)
                                        .with_context(|| {
                                            format!("failed to parse '{expr}' Rye expression in property {id}")
                                        })?;
                                    let ctl = parse_predicates_ctl(
                                        formula,
                                        &mut self.predicates,
                                        interner,
                                    )
                                    .with_context(|| format!("failed to parse predicates in Rye expression in property {id}"))?;
                                    Property::Ctlp(ctl)
                                }
                                PMTL => {
                                    let formula = super::rye::parse(expr)
                                        .with_context(|| {
                                            format!("failed to parse '{expr}' Rye expression in property {id}")
                                        })?;
                                    let pmtl = parse_predicates_pmtl(
                                        formula,
                                        &mut self.predicates,
                                        interner,
                                    )
                                    .with_context(|| format!("failed to parse predicates in Rye expression in property {id}"))?;
                                    Property::Pmtl(pmtl)
                                }
                                _ => bail!("logic {logic} of property {id} unknown or unsupported"),
                            };
                            match stack.last() {
                                Some(PropertyTag::Guarantees) => {
                                    self.guarantees.push((id, property))
                                }
                                Some(PropertyTag::Assumes) => self.assumes.push((id, property)),
                                _ => bail!(
                                    "'{TAG_PROPERTY}' tag found outside '{TAG_GUARANTEES}' or '{TAG_ASSUMES}'"
                                ),
                            }
                        }
                        _ => {
                            error!(target: "parser", "unknown or unexpected empty tag '{tag_name}'");
                            bail!(ParserError::UnexpectedTag(tag_name.to_string()));
                        }
                    }
                }
                Event::Text(text) => {
                    if !text.trim().is_empty() {
                        error!(target: "parser", "text elements not allowed, ignoring");
                    }
                }
                Event::Comment(_comment) => {
                    // nothing to do
                }
                Event::CData(_) => {
                    bail!("CData not supported");
                }
                Event::Decl(decl) => {
                    xml_version = decl.xml_version()?;
                }
                Event::PI(_) => {
                    bail!("Processing Instructions not supported");
                }
                Event::DocType(_) => {
                    bail!("DocType not supported");
                }
                Event::Eof => {
                    info!("parsing completed");
                    break;
                }
                Event::GeneralRef(_bytes_ref) => {
                    bail!("General References not supported");
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

fn parse_predicates_pmtl(
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
            .map(|f| parse_predicates_pmtl(f, predicates, interner))
            .collect::<Result<Box<[_]>, _>>()
            .map(Pmtl::And),
        Pmtl::Or(vec) => vec
            .into_iter()
            .map(|f| parse_predicates_pmtl(f, predicates, interner))
            .collect::<Result<Box<[_]>, _>>()
            .map(Pmtl::Or),
        Pmtl::Not(pmtl) => {
            parse_predicates_pmtl(*pmtl, predicates, interner).map(|f| Pmtl::Not(Box::new(f)))
        }
        Pmtl::Implies(args) => {
            let (lhs, rhs) = *args;
            Ok(Pmtl::Implies(Box::new((
                parse_predicates_pmtl(lhs, predicates, interner)?,
                parse_predicates_pmtl(rhs, predicates, interner)?,
            ))))
        }
        Pmtl::Historically(pmtl, range) => parse_predicates_pmtl(*pmtl, predicates, interner)
            .map(|f| Pmtl::Historically(Box::new(f), range)),
        Pmtl::Once(pmtl, range) => parse_predicates_pmtl(*pmtl, predicates, interner)
            .map(|f| Pmtl::Once(Box::new(f), range)),
        Pmtl::Since(args, range) => {
            let (lhs, rhs) = *args;
            Ok(Pmtl::Since(
                Box::new((
                    parse_predicates_pmtl(lhs, predicates, interner)?,
                    parse_predicates_pmtl(rhs, predicates, interner)?,
                )),
                range,
            ))
        }
    }
}

fn parse_predicates_ctl(
    formula: Ctlp<String>,
    predicates: &mut Vec<boa_ast::Expression>,
    interner: &mut Interner,
) -> anyhow::Result<Ctlp<usize>> {
    match formula {
        Ctlp::True => Ok(Ctlp::True),
        Ctlp::False => Ok(Ctlp::False),
        Ctlp::Atom(expr) => {
            let pred = ecmascript(&expr, &Scope::new_global(), interner)?;
            let idx = predicates.len();
            predicates.push(pred);
            Ok(Ctlp::Atom(idx))
        }
        Ctlp::And(vec) => vec
            .into_iter()
            .map(|f| parse_predicates_ctl(f, predicates, interner))
            .collect::<Result<Box<[_]>, _>>()
            .map(Ctlp::And),
        Ctlp::Or(vec) => vec
            .into_iter()
            .map(|f| parse_predicates_ctl(f, predicates, interner))
            .collect::<Result<Box<[_]>, _>>()
            .map(Ctlp::Or),
        Ctlp::Not(pmtl) => {
            parse_predicates_ctl(*pmtl, predicates, interner).map(|f| Ctlp::Not(Box::new(f)))
        }
        Ctlp::Implies(args) => {
            let (lhs, rhs) = *args;
            Ok(Ctlp::Implies(Box::new((
                parse_predicates_ctl(lhs, predicates, interner)?,
                parse_predicates_ctl(rhs, predicates, interner)?,
            ))))
        }
        Ctlp::Exists(path_ctl) => {
            parse_predicates_path_ctl(path_ctl, predicates, interner).map(Ctlp::Exists)
        }
        Ctlp::All(path_ctl) => {
            parse_predicates_path_ctl(path_ctl, predicates, interner).map(Ctlp::All)
        }
    }
}

fn parse_predicates_path_ctl(
    formula: PathCtl<String>,
    predicates: &mut Vec<boa_ast::Expression>,
    interner: &mut Interner,
) -> anyhow::Result<PathCtl<usize>> {
    match formula {
        PathCtl::Historically(pmtl, range) => parse_predicates_ctl(*pmtl, predicates, interner)
            .map(|f| PathCtl::Historically(Box::new(f), range)),
        PathCtl::Once(pmtl, range) => parse_predicates_ctl(*pmtl, predicates, interner)
            .map(|f| PathCtl::Once(Box::new(f), range)),
        PathCtl::Since(args, range) => {
            let (lhs, rhs) = *args;
            Ok(PathCtl::Since(
                Box::new((
                    parse_predicates_ctl(lhs, predicates, interner)?,
                    parse_predicates_ctl(rhs, predicates, interner)?,
                )),
                range,
            ))
        }
    }
}
