use super::{ATTR_EVENT, ATTR_EXPR, ATTR_PARAM};
use crate::parser::{ParserError, ATTR_ID, ATTR_REFID, ATTR_TYPE, TAG_CONST, TAG_VAR};
use anyhow::{anyhow, Context};
use boa_ast::StatementListItem;
use log::{error, info, trace, warn};
use quick_xml::{
    events::{
        attributes::{AttrError, Attribute},
        Event,
    },
    Reader,
};
use scan_core::{Expression, Float, Mtl, Val};
use std::{collections::HashMap, io::BufRead, str};

const TAG_PORTS: &str = "ports";
const TAG_PORT: &str = "port";
const TAG_PROPERTIES: &str = "properties";
const TAG_PREDICATES: &str = "predicates";
const TAG_PREDICATE: &str = "predicate";
const TAG_GUARANTEES: &str = "guarantees";
const TAG_GUARANTEE: &str = "guarantee";
const TAG_ASSUMES: &str = "assumes";
const TAG_ASSUME: &str = "assume";
const TAG_ORIGIN: &str = "origin";
const TAG_TARGET: &str = "target";
const TAG_MESSAGE: &str = "message";
const TAG_EQUAL: &str = "equal";
const TAG_LESS: &str = "less";
const TAG_LEQ: &str = "leq";
const TAG_GREATER: &str = "greater";
const TAG_GEQ: &str = "geq";
const TAG_AND: &str = "and";
const TAG_OR: &str = "or";
const TAG_NOT: &str = "not";
const TAG_IMPLIES: &str = "implies";
const TAG_SUM: &str = "sum";
const TAG_MULT: &str = "mult";
const TAG_OPPOSITE: &str = "opposite";
const TAG_UNTIL: &str = "until";
const TAG_ALWAYS: &str = "always";
const TAG_EVENTUALLY: &str = "eventually";
const TAG_TRUE: &str = "true";

#[derive(Debug, Clone)]
enum PropertyTag {
    Ports,
    Port(String, ParserPort),
    Properties,
    Predicates,
    Predicate(String, Option<Expression<String>>),
    Guarantees,
    Guarantee(String, Option<Mtl<String>>),
    Assumes,
    Assume(String, Option<Mtl<String>>),
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
    // === MTL Tags ===
    MtlNot(Option<Mtl<String>>),
    MtlImplies(Option<Mtl<String>>, Option<Mtl<String>>),
    MtlAnd(Vec<Mtl<String>>),
    MtlOr(Vec<Mtl<String>>),
    MtlUntil(Option<Mtl<String>>, Option<Mtl<String>>),
    MtlAlways(Option<Mtl<String>>),
    MtlEventually(Option<Mtl<String>>),
}

impl PropertyTag {
    fn is_expression(&self) -> bool {
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

    fn is_mtl(&self) -> bool {
        matches!(
            self,
            PropertyTag::Guarantee(_, _)
                | PropertyTag::Assume(_, _)
                | PropertyTag::MtlAnd(_)
                | PropertyTag::MtlOr(_)
                | PropertyTag::MtlImplies(_, _)
                | PropertyTag::MtlUntil(_, _)
                | PropertyTag::MtlNot(_)
                | PropertyTag::MtlAlways(_)
                | PropertyTag::MtlEventually(_)
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ParserPort {
    // pub(crate) r#type: String,
    pub(crate) origin: String,
    pub(crate) target: String,
    pub(crate) event: String,
    pub(crate) param: Option<(String, boa_ast::Expression)>,
}

impl ParserPort {
    fn parse(tag: quick_xml::events::BytesStart<'_>) -> anyhow::Result<(String, Self)> {
        let mut port_id: Option<String> = None;
        // let mut r#type: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_ID => {
                    port_id = Some(attr.unescape_value()?.into_owned());
                }
                ATTR_TYPE => {
                    // r#type = Some(attr.unescape_value()?.into_owned());
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        let port_id = port_id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_ID.to_string())))?;
        // let r#type = r#type.ok_or(anyhow!(ParserError::MissingAttr(ATTR_TYPE.to_string())))?;

        Ok((
            port_id,
            ParserPort {
                // r#type,
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
        let mut expr: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_EVENT => {
                    event = Some(attr.unescape_value()?.into_owned());
                }
                ATTR_PARAM => {
                    param = Some(attr.unescape_value()?.into_owned());
                }
                ATTR_EXPR => {
                    expr = Some(attr.unescape_value()?.into_owned());
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        let event = event.ok_or(anyhow!(ParserError::MissingAttr(ATTR_EVENT.to_string())))?;

        self.event = event;

        if let Some(expression) = expr {
            if let StatementListItem::Statement(boa_ast::Statement::Expression(expression)) =
                boa_parser::Parser::new(boa_parser::Source::from_bytes(&expression))
                    .parse_script(&mut boa_interner::Interner::new())
                    .expect("hope this works")
                    .statements()
                    .first()
                    .expect("hopefully there is a statement")
                    .to_owned()
            {
                self.param = Some((param.unwrap(), expression));
            } else {
                todo!()
            }
        }

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
                    origin = Some(attr.unescape_value()?.into_owned());
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
                    target = Some(attr.unescape_value()?.into_owned());
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
            PropertyTag::Guarantee(_, _) => TAG_GUARANTEE,
            PropertyTag::Assumes => TAG_ASSUMES,
            PropertyTag::Assume(_, _) => TAG_ASSUME,
            PropertyTag::Ports => TAG_PORTS,
            PropertyTag::Port(_, _) => TAG_PORT,
            PropertyTag::Equal(_, _) => TAG_EQUAL,
            PropertyTag::Less(_, _) => TAG_LESS,
            PropertyTag::LessEq(_, _) => TAG_LEQ,
            PropertyTag::Greater(_, _) => TAG_GREATER,
            PropertyTag::GreaterEq(_, _) => TAG_GEQ,
            PropertyTag::And(_) | PropertyTag::MtlAnd(_) => TAG_AND,
            PropertyTag::Or(_) | PropertyTag::MtlOr(_) => TAG_OR,
            PropertyTag::Not(_) | PropertyTag::MtlNot(_) => TAG_NOT,
            PropertyTag::Implies(_, _) | PropertyTag::MtlImplies(_, _) => TAG_IMPLIES,
            PropertyTag::MtlUntil(_, _) => TAG_UNTIL,
            PropertyTag::MtlAlways(_) => TAG_ALWAYS,
            PropertyTag::MtlEventually(_) => TAG_EVENTUALLY,
            PropertyTag::Sum(_) => TAG_SUM,
            PropertyTag::Mult(_) => TAG_MULT,
            PropertyTag::Opposite(_) => TAG_OPPOSITE,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Properties {
    pub(crate) ports: HashMap<String, ParserPort>,
    pub(crate) predicates: HashMap<String, Expression<String>>,
    pub(crate) guarantees: HashMap<String, Mtl<String>>,
    pub(crate) assumes: HashMap<String, Mtl<String>>,
}

impl Properties {
    pub fn new() -> Self {
        Properties {
            ports: HashMap::new(),
            predicates: HashMap::new(),
            guarantees: HashMap::new(),
            assumes: HashMap::new(),
        }
    }

    pub fn parse<R: BufRead>(reader: &mut Reader<R>) -> anyhow::Result<Self> {
        let mut buf = Vec::new();
        let mut stack: Vec<PropertyTag> = Vec::new();
        let mut ports = HashMap::new();
        let mut predicates = HashMap::new();
        let mut guarantees = HashMap::new();
        let mut assumes = HashMap::new();
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
                            let (id, port) = ParserPort::parse(tag)?;
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
                        TAG_GUARANTEE
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Guarantees)) =>
                        {
                            let id = Self::parse_id(tag)?;
                            stack.push(PropertyTag::Guarantee(id, None));
                        }
                        TAG_ASSUMES
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Properties)) =>
                        {
                            stack.push(PropertyTag::Assumes);
                        }
                        TAG_ASSUME
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, PropertyTag::Assumes)) =>
                        {
                            let id = Self::parse_id(tag)?;
                            stack.push(PropertyTag::Assume(id, None));
                        }
                        TAG_EQUAL if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::Equal(None, None))
                        }
                        TAG_LESS if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::Less(None, None))
                        }
                        TAG_LEQ if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::LessEq(None, None))
                        }
                        TAG_GREATER if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::Greater(None, None))
                        }
                        TAG_GEQ if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::GreaterEq(None, None))
                        }
                        TAG_AND if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::And(Vec::new()))
                        }
                        TAG_OR if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::Or(Vec::new()))
                        }
                        TAG_NOT if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::Not(None))
                        }
                        TAG_IMPLIES if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::Implies(None, None))
                        }
                        TAG_SUM if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::Sum(Vec::new()))
                        }
                        TAG_MULT if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::Mult(Vec::new()))
                        }
                        TAG_OPPOSITE if stack.last().is_some_and(PropertyTag::is_expression) => {
                            stack.push(PropertyTag::Opposite(None))
                        }
                        TAG_AND if stack.last().is_some_and(PropertyTag::is_mtl) => {
                            stack.push(PropertyTag::MtlAnd(Vec::new()))
                        }
                        TAG_OR if stack.last().is_some_and(PropertyTag::is_mtl) => {
                            stack.push(PropertyTag::MtlOr(Vec::new()))
                        }
                        TAG_NOT if stack.last().is_some_and(PropertyTag::is_mtl) => {
                            stack.push(PropertyTag::MtlNot(None))
                        }
                        TAG_IMPLIES if stack.last().is_some_and(PropertyTag::is_mtl) => {
                            stack.push(PropertyTag::MtlImplies(None, None))
                        }
                        TAG_UNTIL if stack.last().is_some_and(PropertyTag::is_mtl) => {
                            stack.push(PropertyTag::MtlUntil(None, None))
                        }
                        TAG_ALWAYS if stack.last().is_some_and(PropertyTag::is_mtl) => {
                            stack.push(PropertyTag::MtlAlways(None))
                        }
                        TAG_EVENTUALLY if stack.last().is_some_and(PropertyTag::is_mtl) => {
                            stack.push(PropertyTag::MtlEventually(None))
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
                                        assumes,
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
                                PropertyTag::Guarantee(id, expr)
                                    if stack.last().is_some_and(|tag| {
                                        matches!(*tag, PropertyTag::Guarantees)
                                    }) =>
                                {
                                    guarantees
                                        .insert(id, expr.ok_or(anyhow!("guarantee missing"))?);
                                }
                                PropertyTag::Assume(id, expr)
                                    if stack.last().is_some_and(|tag| {
                                        matches!(*tag, PropertyTag::Assumes)
                                    }) =>
                                {
                                    assumes.insert(id, expr.ok_or(anyhow!("assumes missing"))?);
                                }
                                PropertyTag::LessEq(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in leq"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in leq"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::LessEq(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::Equal(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in equal"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in equal"))?;
                                    push_expr(&mut stack, Expression::Equal(Box::new((lhs, rhs))))?;
                                }
                                PropertyTag::Less(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in less"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in less"))?;
                                    push_expr(&mut stack, Expression::Less(Box::new((lhs, rhs))))?;
                                }
                                PropertyTag::LessEq(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in leq"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in leq"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::LessEq(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::Greater(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in greater"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in greater"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::Greater(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::GreaterEq(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in geq"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in geq"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::GreaterEq(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::And(exprs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    push_expr(&mut stack, Expression::And(exprs))?;
                                }
                                PropertyTag::Or(exprs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    push_expr(&mut stack, Expression::Or(exprs))?;
                                }
                                PropertyTag::Not(expr)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    let expr = expr.ok_or(anyhow!("missing expr in not"))?;
                                    push_expr(&mut stack, Expression::Not(Box::new(expr)))?;
                                }
                                PropertyTag::Implies(lhs, rhs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in implies"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in implies"))?;
                                    push_expr(
                                        &mut stack,
                                        Expression::Implies(Box::new((lhs, rhs))),
                                    )?;
                                }
                                PropertyTag::Sum(exprs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    push_expr(&mut stack, Expression::Sum(exprs))?;
                                }
                                PropertyTag::Mult(exprs)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    push_expr(&mut stack, Expression::Mult(exprs))?;
                                }
                                PropertyTag::Opposite(expr)
                                    if stack.last().is_some_and(|tag| tag.is_expression()) =>
                                {
                                    let expr = expr.ok_or(anyhow!("missing expr in not"))?;
                                    push_expr(&mut stack, Expression::Opposite(Box::new(expr)))?;
                                }
                                PropertyTag::MtlAnd(exprs)
                                    if stack.last().is_some_and(PropertyTag::is_mtl) =>
                                {
                                    push_mtl(&mut stack, Mtl::And(exprs))?;
                                }
                                PropertyTag::MtlOr(exprs)
                                    if stack.last().is_some_and(PropertyTag::is_mtl) =>
                                {
                                    push_mtl(&mut stack, Mtl::Or(exprs))?;
                                }
                                PropertyTag::MtlNot(expr)
                                    if stack.last().is_some_and(PropertyTag::is_mtl) =>
                                {
                                    let expr = expr.ok_or(anyhow!("missing expr in not"))?;
                                    push_mtl(&mut stack, Mtl::Not(Box::new(expr)))?;
                                }
                                PropertyTag::MtlImplies(lhs, rhs)
                                    if stack.last().is_some_and(PropertyTag::is_mtl) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in implies"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in implies"))?;
                                    push_mtl(&mut stack, Mtl::Implies(Box::new((lhs, rhs))))?;
                                }
                                PropertyTag::MtlUntil(lhs, rhs)
                                    if stack.last().is_some_and(PropertyTag::is_mtl) =>
                                {
                                    let lhs = lhs.ok_or(anyhow!("missing lhs in implies"))?;
                                    let rhs = rhs.ok_or(anyhow!("missing rhs in implies"))?;
                                    push_mtl(&mut stack, Mtl::Until(Box::new((lhs, rhs)), None))?;
                                }
                                PropertyTag::MtlAlways(formula)
                                    if stack.last().is_some_and(|tag| tag.is_mtl()) =>
                                {
                                    let formula = formula.ok_or(anyhow!("missing expr in not"))?;
                                    push_mtl(&mut stack, Mtl::Always(Box::new(formula), None))?;
                                }
                                PropertyTag::MtlEventually(formula)
                                    if stack.last().is_some_and(|tag| tag.is_mtl()) =>
                                {
                                    let formula = formula.ok_or(anyhow!("missing expr in not"))?;
                                    push_mtl(&mut stack, Mtl::Eventually(Box::new(formula), None))?;
                                }
                                PropertyTag::Ports
                                | PropertyTag::Predicates
                                | PropertyTag::Assumes
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
                        TAG_VAR if stack.last().is_some_and(PropertyTag::is_expression) => {
                            let id = Self::parse_refid(tag)?;
                            // NOTE: Use fake type because we don't know it.
                            // WARN: Do not use the fake type when building the expression
                            // FIXIT: Replace workaround with proper solution
                            let expr = Expression::Var(id, scan_core::Type::Product(Vec::new()));
                            push_expr(&mut stack, expr)?;
                        }
                        TAG_VAR if stack.last().is_some_and(PropertyTag::is_mtl) => {
                            let id = Self::parse_refid(tag)?;
                            let expr = Mtl::Atom(id);
                            push_mtl(&mut stack, expr)?;
                        }
                        TAG_CONST if stack.last().is_some_and(PropertyTag::is_expression) => {
                            let val = Self::parse_const(tag)?;
                            let expr = Expression::Const(val);
                            push_expr(&mut stack, expr)?;
                        }
                        TAG_TRUE => {
                            let expr = Mtl::True;
                            push_mtl(&mut stack, expr)?;
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
                Event::CData(_) => return Err(anyhow!("CData not supported")),
                // Ignore XML declaration
                Event::Decl(_) => continue,
                Event::PI(_) => return Err(anyhow!("Processing Instructions not supported")),
                Event::DocType(_) => return Err(anyhow!("DocType not supported")),
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
                    id = Some(attr.unescape_value()?.into_owned());
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
                    id = Some(attr.unescape_value()?.into_owned());
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
                    r#type = Some(attr.unescape_value()?.into_owned());
                }
                ATTR_EXPR => {
                    val = Some(attr.unescape_value()?.into_owned());
                }
                key => {
                    error!("found unknown attribute {key}");
                    return Err(anyhow::Error::new(ParserError::UnknownKey(key.to_owned())));
                }
            }
        }

        let val = val.ok_or(anyhow!("missing expression"))?;

        match r#type.ok_or(anyhow!("missing type"))?.as_str() {
            "int32" => Ok(val.parse::<i32>().map(Val::Integer)?),
            "float64" => Ok(val.parse::<Float>().map(Val::from)?),
            "boolean" => Ok(val.parse::<bool>().map(Val::Boolean)?),
            unknown => Err(anyhow!("unwnown type {unknown}")),
        }
    }
}

fn push_expr(stack: &mut [PropertyTag], expr: Expression<String>) -> anyhow::Result<()> {
    match stack
        .last_mut()
        .ok_or(anyhow!("expression not contained inside proper tag"))?
    {
        PropertyTag::Predicate(_, pred) => {
            if pred.is_none() {
                *pred = Some(expr);
            } else {
                return Err(anyhow!("multiple expressions in predicate"));
            }
        }
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
        other_tag => return Err(anyhow!("{other_tag:?} is not an expression tag")),
    }
    Ok(())
}

fn push_mtl(stack: &mut [PropertyTag], expr: Mtl<String>) -> anyhow::Result<()> {
    match stack
        .last_mut()
        .ok_or(anyhow!("MTL formula not contained inside proper tag"))?
    {
        PropertyTag::Guarantee(_, formula)
        | PropertyTag::Assume(_, formula)
        | PropertyTag::MtlAlways(formula)
        | PropertyTag::MtlEventually(formula)
        | PropertyTag::MtlNot(formula) => {
            if formula.is_none() {
                *formula = Some(expr);
            } else {
                return Err(anyhow!("multiple expressions in guarantee"));
            }
        }
        PropertyTag::MtlUntil(lhs, rhs) | PropertyTag::MtlImplies(lhs, rhs) => {
            if lhs.is_none() {
                *lhs = Some(expr);
            } else if rhs.is_none() {
                *rhs = Some(expr);
            } else {
                return Err(anyhow!("too many arguments in binary operator"));
            }
        }
        PropertyTag::MtlAnd(exprs) | PropertyTag::MtlOr(exprs) => {
            exprs.push(expr);
        }
        other_tag => return Err(anyhow!("{other_tag:?} is not an MTL tag")),
    }
    Ok(())
}
