use std::{collections::HashMap, io::BufRead, str};

use anyhow::{anyhow, Context};
use log::{error, info, trace, warn};
use quick_xml::{
    events::{
        attributes::{AttrError, Attribute},
        Event,
    },
    Reader,
};
use scan_core::{Expression, Mtl, Val};

use crate::parser::{ParserError, ATTR_ID, ATTR_REFID, ATTR_TYPE, TAG_CONST, TAG_VAR};

use super::{ATTR_EVENT, ATTR_EXPR, ATTR_PARAM};

const TAG_PORTS: &'static str = "ports";
const TAG_PORT: &'static str = "port";
const TAG_PROPERTIES: &'static str = "properties";
const TAG_PREDICATES: &'static str = "predicates";
const TAG_PREDICATE: &'static str = "predicate";
const TAG_GUARANTEES: &'static str = "guarantees";
const TAG_GUARANTEE: &'static str = "guarantee";
const TAG_ORIGIN: &'static str = "origin";
const TAG_TARGET: &'static str = "target";
const TAG_MESSAGE: &'static str = "message";
const TAG_EQUAL: &'static str = "equal";
const TAG_LESS: &'static str = "less";
const TAG_LEQ: &'static str = "leq";
const TAG_GREATER: &'static str = "greater";
const TAG_GEQ: &'static str = "geq";
const TAG_AND: &'static str = "and";
const TAG_OR: &'static str = "or";
const TAG_NOT: &'static str = "not";
const TAG_IMPLIES: &'static str = "implies";
const TAG_SUM: &'static str = "sum";
const TAG_MULT: &'static str = "mult";
const TAG_OPPOSITE: &'static str = "opposite";

#[derive(Debug, Clone)]
enum PropertyTag {
    Ports,
    Port(String, Port),
    Properties,
    Predicates,
    Predicate(String, Option<Expression<String>>),
    Guarantees,
    Guarantee,
    // === Expression Tags ===
    Not(Option<Expression<String>>),
    Implies(Option<Expression<String>>, Option<Expression<String>>),
    And(Vec<Expression<String>>),
    Or(Vec<Expression<String>>),
    Sum(Vec<Expression<String>>),
    Mult(Vec<Expression<String>>),
    Opposite(Option<Expression<String>>),
    Equal(Option<Expression<String>>, Option<Expression<String>>),
    Less(Option<Expression<String>>, Option<Expression<String>>),
    LessEq(Option<Expression<String>>, Option<Expression<String>>),
    Greater(Option<Expression<String>>, Option<Expression<String>>),
    GreaterEq(Option<Expression<String>>, Option<Expression<String>>),
}

impl PropertyTag {
    fn is_formula(&self) -> bool {
        matches!(
            self,
            PropertyTag::Predicate(_, _)
                | PropertyTag::Equal(_, _)
                | PropertyTag::Less(_, _)
                | PropertyTag::LessEq(_, _)
                | PropertyTag::Greater(_, _)
                | PropertyTag::GreaterEq(_, _)
                | PropertyTag::And(_)
                | PropertyTag::Or(_)
                | PropertyTag::Implies(_, _)
                | PropertyTag::Not(_)
                | PropertyTag::Sum(_)
                | PropertyTag::Mult(_)
                | PropertyTag::Opposite(_)
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Port {
    pub(crate) r#type: String,
    pub(crate) origin: String,
    pub(crate) target: String,
    pub(crate) event: String,
    pub(crate) param: Option<String>,
}

impl Port {
    fn parse(tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<(String, Self)> {
        let mut port_id: Option<String> = None;
        let mut r#type: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_ID => {
                    port_id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_TYPE => {
                    r#type = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        let port_id = port_id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_ID.to_string())))?;
        let r#type = r#type.ok_or(anyhow!(ParserError::MissingAttr(ATTR_TYPE.to_string())))?;

        Ok((
            port_id,
            Port {
                r#type,
                origin: String::new(),
                target: String::new(),
                event: String::new(),
                param: None,
            },
        ))
    }

    fn parse_event(&mut self, tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<()> {
        let mut event: Option<String> = None;
        let mut param: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_EVENT => {
                    event = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_PARAM => {
                    param = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        let event = event.ok_or(anyhow!(ParserError::MissingAttr(ATTR_EVENT.to_string())))?;

        self.event = event;
        self.param = param;
        Ok(())
    }

    fn parse_origin(&mut self, tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<()> {
        let mut origin: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_REFID => {
                    origin = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        let origin = origin.ok_or(anyhow!(ParserError::MissingAttr(ATTR_REFID.to_string())))?;

        self.origin = origin;
        Ok(())
    }

    fn parse_target(&mut self, tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<()> {
        let mut target: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_REFID => {
                    target = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        let target = target.ok_or(anyhow!(ParserError::MissingAttr(ATTR_REFID.to_string())))?;

        self.target = target;
        Ok(())
    }
}

impl From<&PropertyTag> for &'static str {
    fn from(value: &PropertyTag) -> Self {
        match value {
            PropertyTag::Properties => TAG_PROPERTIES,
            PropertyTag::Predicates => TAG_PREDICATES,
            PropertyTag::Predicate(_, _) => TAG_PREDICATE,
            PropertyTag::Guarantees => TAG_GUARANTEES,
            PropertyTag::Guarantee => TAG_GUARANTEE,
            PropertyTag::Ports => TAG_PORTS,
            PropertyTag::Port(_, _) => TAG_PORT,
            PropertyTag::Equal(_, _) => TAG_EQUAL,
            PropertyTag::Less(_, _) => TAG_LESS,
            PropertyTag::LessEq(_, _) => TAG_LEQ,
            PropertyTag::Greater(_, _) => TAG_GREATER,
            PropertyTag::GreaterEq(_, _) => TAG_GEQ,
            PropertyTag::And(_) => TAG_AND,
            PropertyTag::Or(_) => TAG_OR,
            PropertyTag::Not(_) => TAG_NOT,
            PropertyTag::Implies(_, _) => TAG_IMPLIES,
            PropertyTag::Sum(_) => TAG_SUM,
            PropertyTag::Mult(_) => TAG_MULT,
            PropertyTag::Opposite(_) => TAG_OPPOSITE,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Properties {
    pub(crate) ports: HashMap<String, Port>,
    pub(crate) predicates: HashMap<String, Expression<String>>,
    pub(crate) guarantees: Vec<Mtl<String>>,
}

impl Properties {
    pub fn new() -> Self {
        Properties {
            ports: HashMap::new(),
            predicates: HashMap::new(),
            guarantees: Vec::new(),
        }
    }

    pub fn parse<R: BufRead>(reader: &mut Reader<R>) -> anyhow::Result<Self> {
        let mut buf = Vec::new();
        let mut stack: Vec<PropertyTag> = Vec::new();
        let mut ports = HashMap::new();
        let mut predicates = HashMap::new();
        let mut guarantees = Vec::new();
        // let mut assumes = Vec::new();
        info!("parsing properties");
        loop {
            let event = reader.read_event_into(&mut buf)?;
            match event {
                Event::Start(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
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
                            let (id, port) = Port::parse(tag)?;
                            stack.push(PropertyTag::Port(id, port));
                        }
                        TAG_PREDICATES
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Properties)) =>
                        {
                            stack.push(PropertyTag::Predicates);
                        }
                        TAG_PREDICATE
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Predicates)) =>
                        {
                            let id = Self::parse_id(tag)?;
                            stack.push(PropertyTag::Predicate(id, None));
                        }
                        TAG_GUARANTEES
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Properties)) =>
                        {
                            stack.push(PropertyTag::Guarantees);
                        }
                        TAG_EQUAL if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::Equal(None, None))
                        }
                        TAG_LESS if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::Less(None, None))
                        }
                        TAG_LEQ if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::LessEq(None, None))
                        }
                        TAG_GREATER if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::Greater(None, None))
                        }
                        TAG_GEQ if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::GreaterEq(None, None))
                        }
                        TAG_AND if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::And(Vec::new()))
                        }
                        TAG_OR if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::Or(Vec::new()))
                        }
                        TAG_NOT if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::Not(None))
                        }
                        TAG_IMPLIES if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::Implies(None, None))
                        }
                        TAG_SUM if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::Sum(Vec::new()))
                        }
                        TAG_MULT if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::Mult(Vec::new()))
                        }
                        TAG_OPPOSITE if stack.last().is_some_and(|tag| tag.is_formula()) => {
                            stack.push(PropertyTag::Opposite(None))
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
                    if let Some(tag) = stack.pop() {
                        if <&str>::from(&tag) != tag_name {
                            error!("unexpected end tag {tag_name}");
                            return Err(anyhow!(ParserError::UnexpectedEndTag(
                                tag_name.to_string()
                            )))
                            .context(reader.error_position());
                        } else {
                            trace!("'{tag_name}' end tag");
                            match tag {
                                PropertyTag::Properties if stack.is_empty() => {
                                    return Ok(Properties {
                                        ports,
                                        predicates,
                                        guarantees,
                                    });
                                }
                                PropertyTag::Port(id, port)
                                    if stack
                                        .last()
                                        .is_some_and(|tag| matches!(*tag, PropertyTag::Ports)) =>
                                {
                                    ports.insert(id, port);
                                }
                                PropertyTag::Predicate(id, expr)
                                    if stack.last().is_some_and(|tag| {
                                        matches!(*tag, PropertyTag::Predicates)
                                    }) =>
                                {
                                    predicates
                                        .insert(id, expr.ok_or(anyhow!("predicate missing"))?);
                                }
                                PropertyTag::LessEq(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in leq"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in leq"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::LessEq(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::Equal(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in equal"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in equal"))?;
                                    push_expr(&mut stack, Expression::Equal(Box::new((lhs, rhs))))?;
                                }
                                PropertyTag::Less(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in less"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in less"))?;
                                    push_expr(&mut stack, Expression::Less(Box::new((lhs, rhs))))?;
                                }
                                PropertyTag::LessEq(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in leq"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in leq"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::LessEq(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::Greater(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in greater"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in greater"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::Greater(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::GreaterEq(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in geq"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in geq"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::GreaterEq(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::And(exprs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    push_expr(&mut stack, Expression::And(exprs))?;
                                }
                                PropertyTag::Or(exprs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    push_expr(&mut stack, Expression::Or(exprs))?;
                                }
                                PropertyTag::Not(expr)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    let expr = expr.ok_or(anyhow!("missing expr in not"))?;
                                    push_expr(&mut stack, Expression::Not(Box::new(expr)))?;
                                }
                                PropertyTag::Implies(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in implies"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in implies"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::Implies(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::Sum(exprs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    push_expr(&mut stack, Expression::Sum(exprs))?;
                                }
                                PropertyTag::Mult(exprs)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    push_expr(&mut stack, Expression::Mult(exprs))?;
                                }
                                PropertyTag::Opposite(expr)
                                    if stack.last().is_some_and(|tag| tag.is_formula()) =>
                                {
                                    let expr = expr.ok_or(anyhow!("missing expr in not"))?;
                                    push_expr(&mut stack, Expression::Opposite(Box::new(expr)))?;
                                }
                                PropertyTag::Ports
                                | PropertyTag::Predicates
                                | PropertyTag::Guarantees => {}
                                _ => {
                                    // Closed tag matching open tag but not one of the above?
                                    unreachable!("All tags should be considered");
                                }
                            }
                        }
                    } else {
                        // WARN TODO FIXME: actually tag missing from stack?
                        error!("unexpected end tag {tag_name}");
                        return Err(anyhow!(ParserError::UnexpectedEndTag(tag_name.to_string()),));
                    }
                }
                Event::Empty(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' empty tag");
                    match tag_name {
                        TAG_ORIGIN
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Port(_, _))) =>
                        {
                            if let Some(PropertyTag::Port(_, port)) = stack.last_mut() {
                                port.parse_origin(tag)?;
                            } else {
                                unreachable!("A port must be on top of stack");
                            }
                        }
                        TAG_TARGET
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Port(_, _))) =>
                        {
                            if let Some(PropertyTag::Port(_, port)) = stack.last_mut() {
                                port.parse_target(tag)?;
                            } else {
                                unreachable!("A port must be on top of stack");
                            }
                        }
                        TAG_MESSAGE
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Port(_, _))) =>
                        {
                            if let Some(PropertyTag::Port(_, port)) = stack.last_mut() {
                                port.parse_event(tag)?;
                            } else {
                                unreachable!("A port must be on top of stack");
                            }
                        }
                        TAG_VAR => {
                            let id = Self::parse_refid(tag)?;
                            let expr = Expression::Var(id);
                            push_expr(&mut stack, expr)?;
                        }
                        TAG_CONST => {
                            let val = Self::parse_const(tag)?;
                            let expr = Expression::Const(val);
                            push_expr(&mut stack, expr)?;
                        }
                        // Unknown tag: skip till maching end tag
                        _ => {
                            warn!("unknown or unexpected tag {tag_name:?}, skipping");
                            continue;
                        }
                    }
                }
                Event::Text(_) => continue,
                Event::Comment(_) => {}
                Event::CData(_) => todo!(),
                Event::Decl(_) => todo!(), // parser.parse_xml_declaration(tag)?,
                Event::PI(_) => todo!(),
                Event::DocType(_) => todo!(),
                // exits the loop when reaching end of file
                Event::Eof => {
                    // info!("parsing completed");
                    return Err(anyhow!(ParserError::UnclosedTags));
                }
            }
            // if we don't keep a borrow elsewhere, we can clear the buffer to keep memory usage low
            buf.clear();
        }
    }

    fn parse_id(tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<String> {
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
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_REFID.to_string())))
    }

    fn parse_refid(tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<String> {
        let mut id: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_REFID => {
                    id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_REFID.to_string())))
    }

    fn parse_const(tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<Val> {
        let mut r#type: Option<String> = None;
        let mut val: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_TYPE => {
                    r#type = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_EXPR => {
                    val = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        let val = val.ok_or(anyhow!("missing expression"))?;

        match r#type.ok_or(anyhow!("missing type"))?.as_str() {
            "int32" => Ok(val.parse::<i32>().map(|c| Val::Integer(c))?),
            "boolean" => Ok(val.parse::<bool>().map(|b| Val::Boolean(b))?),
            unknown => Err(anyhow!("unwnown type {unknown}")),
        }
    }
}

fn push_expr(stack: &mut [PropertyTag], expr: Expression<String>) -> anyhow::Result<()> {
    match stack
        .last_mut()
        .ok_or(anyhow!("expression not contained inside proper tag"))?
    {
        PropertyTag::Ports => todo!(),
        PropertyTag::Port(_, _) => todo!(),
        PropertyTag::Properties => todo!(),
        PropertyTag::Predicates => todo!(),
        PropertyTag::Predicate(_, pred) => {
            if pred.is_none() {
                *pred = Some(expr);
            } else {
                return Err(anyhow!("multiple expressions in predicate"));
            }
        }
        PropertyTag::Guarantees => todo!(),
        PropertyTag::Guarantee => todo!(),
        PropertyTag::Equal(lhs, rhs)
        | PropertyTag::Less(lhs, rhs)
        | PropertyTag::LessEq(lhs, rhs)
        | PropertyTag::Greater(lhs, rhs)
        | PropertyTag::GreaterEq(lhs, rhs)
        | PropertyTag::Implies(lhs, rhs) => {
            if lhs.is_none() {
                *lhs = Some(expr);
            } else if rhs.is_none() {
                *rhs = Some(expr);
            } else {
                return Err(anyhow!("too many arguments in binary operator"));
            }
        }
        PropertyTag::And(exprs)
        | PropertyTag::Or(exprs)
        | PropertyTag::Sum(exprs)
        | PropertyTag::Mult(exprs) => {
            exprs.push(expr);
        }
        PropertyTag::Opposite(arg) | PropertyTag::Not(arg) => {
            if arg.is_none() {
                *arg = Some(expr);
            } else {
                return Err(anyhow!("multiple expressions in opposite"));
            }
        }
    }
    Ok(())
}
