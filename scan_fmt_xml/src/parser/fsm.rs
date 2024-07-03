use super::vocabulary::*;
use crate::parser::ParserError;
use anyhow::{anyhow, Context};
use boa_ast::{Expression as BoaExpression, StatementListItem};
use log::{error, info, trace, warn};
use quick_xml::events::attributes::{AttrError, Attribute};
use quick_xml::{events, events::Event, Reader};
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::{BufRead, Read};
use std::str;

#[derive(Debug)]
enum ScxmlTag {
    State(State),
    Transition(Transition),
    Scxml(Scxml),
    Datamodel(Vec<Data>),
    If(If),
    OnEntry(Vec<Executable>),
    OnExit(Vec<Executable>),
    Send(Send),
}

impl From<&ScxmlTag> for &'static str {
    fn from(value: &ScxmlTag) -> Self {
        match value {
            ScxmlTag::State(_) => TAG_STATE,
            ScxmlTag::Transition(_) => TAG_TRANSITION,
            ScxmlTag::Scxml(_) => TAG_SCXML,
            ScxmlTag::Datamodel(_) => TAG_DATAMODEL,
            ScxmlTag::If(_) => TAG_IF,
            ScxmlTag::OnEntry(_) => TAG_ONENTRY,
            ScxmlTag::OnExit(_) => TAG_ONEXIT,
            ScxmlTag::Send(_) => TAG_SEND,
        }
    }
}

impl ScxmlTag {
    pub fn is_executable(&self) -> bool {
        matches!(
            self,
            ScxmlTag::OnEntry(_) | ScxmlTag::OnExit(_) | ScxmlTag::Transition(_) | ScxmlTag::If(_)
        )
    }
}

#[derive(Debug, Clone)]
pub struct Data {
    pub(crate) id: String,
    pub(crate) expression: Option<boa_ast::Expression>,
    pub(crate) omg_type: String,
}

impl Data {
    fn parse(
        tag: events::BytesStart<'_>,
        ident: String,
        omg_type: String,
        interner: &mut boa_interner::Interner,
    ) -> anyhow::Result<Data> {
        let mut id: Option<String> = None;
        let mut expr: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_ID => {
                    id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_EXPR => {
                    expr = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_DATA}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned(),)));
                }
            }
        }
        let id = id.ok_or(anyhow!(ParserError::MissingAttr(ATTR_ID.to_string())))?;
        // Check id is matching
        if id != ident {
            return Err(anyhow!(ParserError::NoTypeAnnotation,));
        }
        if let Some(expression) = expr {
            if let StatementListItem::Statement(boa_ast::Statement::Expression(expression)) =
                boa_parser::Parser::new(boa_parser::Source::from_bytes(&expression))
                    .parse_script(interner)
                    .expect("hope this works")
                    .statements()
                    .first()
                    .expect("hopefully there is a statement")
                    .to_owned()
            {
                Ok(Data {
                    id,
                    expression: Some(expression),
                    omg_type,
                })
            } else {
                todo!()
            }
        } else {
            Ok(Data {
                id,
                expression: None,
                omg_type,
            })
        }
    }

    fn push(self, stack: &mut [ScxmlTag]) -> anyhow::Result<()> {
        if let Some(ScxmlTag::Datamodel(datamodel)) = stack.last_mut() {
            datamodel.push(self);
            Ok(())
        } else {
            Err(anyhow!("data must be inside datamodel"))
        }
    }
}

#[derive(Debug, Clone)]
pub struct State {
    pub(crate) id: String,
    pub(crate) transitions: Vec<Transition>,
    pub(crate) on_entry: Vec<Executable>,
    pub(crate) on_exit: Vec<Executable>,
}

impl State {
    fn parse(tag: events::BytesStart<'_>) -> anyhow::Result<State> {
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
                    error!("found unknown attribute {key} in {TAG_STATE}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned()),));
                }
            }
        }
        let id = id.ok_or(ParserError::MissingAttr(ATTR_ID.to_string()))?;
        // Check if it is the initial state
        // if self.initial.is_empty() {
        //     id.clone_into(&mut self.initial);
        // }
        // Here it should be checked that no component was already in the list under the same name
        let state = State {
            id,
            transitions: Vec::new(),
            on_entry: Vec::new(),
            on_exit: Vec::new(),
        };
        Ok(state)
    }

    fn push(self, stack: &mut [ScxmlTag]) -> anyhow::Result<()> {
        if let Some(ScxmlTag::Scxml(fsm)) = stack.last_mut() {
            fsm.states.insert(self.id.to_owned(), self);
            Ok(())
        } else {
            Err(anyhow!("states must be inside a scxml tag"))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Transition {
    pub(crate) event: Option<String>,
    pub(crate) target: String,
    pub(crate) cond: Option<boa_ast::Expression>,
    pub(crate) effects: Vec<Executable>,
}

impl Transition {
    fn parse(
        tag: events::BytesStart<'_>,
        interner: &mut boa_interner::Interner,
    ) -> anyhow::Result<Transition> {
        let mut event: Option<String> = None;
        let mut target: Option<String> = None;
        let mut cond: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_EVENT => {
                    event = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_TARGET => {
                    target = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_COND => {
                    cond = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_TRANSITION}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned(),)));
                }
            }
        }
        let target = target.ok_or(anyhow!(ParserError::MissingAttr(ATTR_TARGET.to_string())))?;
        let cond = if let Some(cond) = cond {
            if let StatementListItem::Statement(boa_ast::Statement::Expression(cond)) =
                boa_parser::Parser::new(boa_parser::Source::from_bytes(&cond))
                    .parse_script(interner)
                    .expect("hope this works")
                    .statements()
                    .first()
                    .expect("hopefully there is a statement")
                    .to_owned()
            {
                Some(cond)
            } else {
                return Err(anyhow!(ParserError::EcmaScriptParsing,));
            }
        } else {
            None
        };
        Ok(Transition {
            event,
            target,
            cond,
            effects: Vec::new(),
        })
    }

    fn push(self, stack: &mut [ScxmlTag]) -> anyhow::Result<()> {
        if let Some(ScxmlTag::State(state)) = stack.last_mut() {
            state.transitions.push(self);
            Ok(())
        } else {
            Err(anyhow!("transitions must be inside a state"))
        }
    }
}

#[derive(Debug, Clone)]
pub enum Target {
    Id(String),
    Expr(boa_ast::Expression),
}

#[derive(Debug, Clone)]
pub enum Executable {
    Assign {
        location: String,
        expr: boa_ast::Expression,
    },
    Raise {
        event: String,
    },
    Send(Send),
    If(If),
}

impl Executable {
    fn parse_raise(tag: events::BytesStart<'_>) -> anyhow::Result<Executable> {
        let mut event: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_EVENT => {
                    event = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_TRANSITION}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned()),));
                }
            }
        }
        let event = event.ok_or(anyhow!(ParserError::MissingAttr(ATTR_EVENT.to_string())))?;
        Ok(Executable::Raise { event })
    }

    fn parse_assign(
        tag: events::BytesStart<'_>,
        interner: &mut boa_interner::Interner,
    ) -> anyhow::Result<Executable> {
        let mut location: Option<String> = None;
        let mut expr: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_LOCATION => {
                    location = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_EXPR => {
                    expr = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_TRANSITION}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned()),));
                }
            }
        }
        let location =
            location.ok_or(anyhow!(ParserError::MissingAttr(ATTR_LOCATION.to_string())))?;
        let expr = expr.ok_or(anyhow!(ParserError::MissingAttr(ATTR_EXPR.to_string())))?;
        // FIXME: This is really bad code!
        let statement = boa_parser::Parser::new(boa_parser::Source::from_bytes(&expr))
            .parse_script(interner)
            .expect("hope this works")
            .statements()
            .first()
            .expect("hopefully there is a statement")
            .to_owned();
        match statement {
            StatementListItem::Statement(boa_ast::Statement::Expression(expr)) => {
                Ok(Executable::Assign { location, expr })
            }
            _ => Err(anyhow!("{statement:?} assignment is not an expression")),
        }
    }

    fn push(self, stack: &mut [ScxmlTag]) -> anyhow::Result<()> {
        match stack.last_mut().expect("send must be inside other tag") {
            ScxmlTag::Transition(transition) => {
                transition.effects.push(self);
            }
            ScxmlTag::OnEntry(execs) | ScxmlTag::OnExit(execs) => {
                execs.push(self);
            }
            ScxmlTag::If(r#if) => {
                r#if.execs.push(self);
            }
            _ => return Err(anyhow!("send must be inside an executable tag")),
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Send {
    pub(crate) event: String,
    pub(crate) target: Target,
    pub(crate) params: Vec<Param>,
}

impl Send {
    fn parse(
        tag: events::BytesStart<'_>,
        interner: &mut boa_interner::Interner,
    ) -> anyhow::Result<Send> {
        let mut event: Option<String> = None;
        let mut target: Option<String> = None;
        let mut targetexpr: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_EVENT => {
                    event = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_TARGET => {
                    target = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_TARGETEXPR => {
                    targetexpr = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_TRANSITION}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned(),)));
                }
            }
        }
        let event = event.ok_or(ParserError::MissingAttr(ATTR_EVENT.to_string()))?;
        let target = if let Some(target) = target {
            Target::Id(target)
        } else if let Some(targetexpr) = targetexpr {
            if let StatementListItem::Statement(boa_ast::Statement::Expression(targetexpr)) =
                boa_parser::Parser::new(boa_parser::Source::from_bytes(&targetexpr))
                    .parse_script(interner)
                    .expect("hope this works")
                    .statements()
                    .first()
                    .expect("hopefully there is a statement")
                    .to_owned()
            {
                Target::Expr(targetexpr)
            } else {
                return Err(anyhow!(ParserError::EcmaScriptParsing,));
            }
        } else {
            return Err(anyhow!(ParserError::MissingAttr(
                ATTR_TARGETEXPR.to_string()
            )));
        };
        Ok(Send {
            event,
            target,
            params: Vec::new(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub(crate) cond: boa_ast::Expression,
    pub(crate) execs: Vec<Executable>,
}

impl If {
    fn parse(
        tag: events::BytesStart<'_>,
        interner: &mut boa_interner::Interner,
    ) -> anyhow::Result<If> {
        let mut cond: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_COND => {
                    cond = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_TRANSITION}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned(),)));
                }
            }
        }
        let cond = cond.ok_or(anyhow!(ParserError::MissingAttr(ATTR_COND.to_string())))?;
        if let StatementListItem::Statement(boa_ast::Statement::Expression(cond)) =
            boa_parser::Parser::new(boa_parser::Source::from_bytes(&cond))
                .parse_script(interner)
                .expect("hope this works")
                .statements()
                .first()
                .expect("hopefully there is a statement")
                .to_owned()
        {
            Ok(If {
                cond,
                execs: Vec::new(),
            })
        } else {
            Err(anyhow!(ParserError::EcmaScriptParsing))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub(crate) name: String,
    pub(crate) omg_type: String,
    pub(crate) expr: BoaExpression,
}

impl Param {
    fn parse(
        tag: events::BytesStart<'_>,
        ident: String,
        omg_type: String,
        interner: &mut boa_interner::Interner,
    ) -> anyhow::Result<Param> {
        let mut name: Option<String> = None;
        let mut location: Option<String> = None;
        let mut expr: Option<String> = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_NAME => {
                    name = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_LOCATION => {
                    location = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_EXPR => {
                    expr = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    error!("found unknown attribute {key} in {TAG_TRANSITION}");
                    return Err(anyhow!(ParserError::UnknownKey(key.to_owned()),));
                }
            }
        }
        let name = name.ok_or(ParserError::MissingAttr(ATTR_NAME.to_string()))?;
        if name != ident {
            return Err(anyhow!(ParserError::NoTypeAnnotation,));
        }
        let expr = expr.or(location).ok_or(ParserError::MissingExpr)?;
        if let StatementListItem::Statement(boa_ast::Statement::Expression(expr)) =
            boa_parser::Parser::new(boa_parser::Source::from_bytes(&expr))
                .parse_script(interner)
                .expect("hope this works")
                .statements()
                .first()
                .expect("hopefully there is a statement")
                .to_owned()
        {
            // Full parameter with either location or expression as argument
            Ok(Param {
                name,
                omg_type,
                expr,
            })
        } else {
            Err(anyhow!(ParserError::EcmaScriptParsing,))
        }
    }
}

#[derive(Debug)]
pub struct Scxml {
    pub(crate) id: String,
    pub(crate) initial: String,
    pub(crate) datamodel: Vec<Data>,
    pub(crate) states: HashMap<String, State>,
}

impl Scxml {
    fn parse(tag: events::BytesStart<'_>) -> anyhow::Result<Scxml> {
        let mut id = None;
        let mut initial = None;
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_NAME => {
                    id = Some(String::from_utf8(attr.value.into_owned())?);
                }
                ATTR_INITIAL => {
                    initial = Some(String::from_utf8(attr.value.into_owned())?);
                }
                key => {
                    warn!("found unknown attribute {key} in {TAG_STATE}, ignoring");
                    continue;
                }
            }
        }
        let id = id.ok_or(ParserError::MissingAttr(ATTR_ID.to_owned()))?;
        let initial = initial.ok_or(ParserError::MissingAttr(ATTR_INITIAL.to_owned()))?;
        Ok(Scxml {
            id,
            initial,
            datamodel: Vec::new(),
            states: HashMap::new(),
        })
    }
}

#[derive(Debug)]
pub struct Fsm {
    pub(crate) interner: boa_interner::Interner,
    pub(crate) scxml: Scxml,
}

impl Fsm {
    pub(super) fn parse<R: BufRead>(reader: &mut Reader<R>) -> anyhow::Result<Self> {
        let mut buf = Vec::new();
        let mut stack: Vec<ScxmlTag> = Vec::new();
        let mut type_annotation: Option<(String, String)> = None;
        let mut interner = boa_interner::Interner::new();
        info!("parsing fsm");
        loop {
            let event = reader.read_event_into(&mut buf)?;
            match event {
                Event::Start(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' open tag");
                    match tag_name {
                        TAG_SCXML if stack.is_empty() => {
                            let fsm = Scxml::parse(tag)
                                .map_err(|err| err.context(reader.error_position()))?;
                            stack.push(ScxmlTag::Scxml(fsm));
                        }
                        TAG_DATAMODEL
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml(_))) =>
                        {
                            stack.push(ScxmlTag::Datamodel(Vec::new()));
                        }
                        TAG_STATE
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml(_))) =>
                        {
                            let state = State::parse(tag)
                                .map_err(|err| err.context(reader.error_position()))?;
                            stack.push(ScxmlTag::State(state));
                        }
                        TAG_TRANSITION
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                        {
                            let transition = Transition::parse(tag, &mut interner)
                                .map_err(|err| err.context(reader.error_position()))?;
                            stack.push(ScxmlTag::Transition(transition));
                        }
                        TAG_SEND if stack.iter().rev().any(|tag| tag.is_executable()) => {
                            let send = Send::parse(tag, &mut interner)
                                .map_err(|err| err.context(reader.error_position()))?;
                            stack.push(ScxmlTag::Send(send));
                        }
                        TAG_IF if stack.iter().rev().any(|tag| tag.is_executable()) => {
                            let r#if = If::parse(tag, &mut interner)
                                .map_err(|err| err.context(reader.error_position()))?;
                            stack.push(ScxmlTag::If(r#if));
                        }
                        TAG_ONENTRY
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                        {
                            stack.push(ScxmlTag::OnEntry(Vec::new()));
                        }
                        TAG_ONEXIT
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                        {
                            stack.push(ScxmlTag::OnExit(Vec::new()));
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
                    // if let Some(tag) = stack.pop().is_some_and(|tag| <&str>::from(tag) == tag_name) {
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
                                ScxmlTag::Scxml(fsm) if stack.is_empty() => {
                                    return Ok(Fsm {
                                        interner,
                                        scxml: fsm,
                                    });
                                }
                                ScxmlTag::Datamodel(datamodel)
                                    if stack
                                        .last()
                                        .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml(_))) =>
                                {
                                    if let Some(ScxmlTag::Scxml(fsm)) = stack.last_mut() {
                                        fsm.datamodel = datamodel;
                                    } else {
                                        unreachable!("transitions must be inside a state");
                                    }
                                }
                                ScxmlTag::State(state)
                                    if stack
                                        .last()
                                        .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml(_))) =>
                                {
                                    state.push(&mut stack)?;
                                }
                                ScxmlTag::Transition(transition)
                                    if stack
                                        .last()
                                        .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                                {
                                    transition
                                        .push(&mut stack)
                                        .map_err(|err| err.context(reader.error_position()))?;
                                }
                                ScxmlTag::Send(send)
                                    if stack.iter().rev().any(|tag| tag.is_executable()) =>
                                {
                                    Executable::Send(send)
                                        .push(&mut stack)
                                        .map_err(|err| err.context(reader.error_position()))?;
                                }
                                ScxmlTag::If(r#if)
                                    if stack.iter().rev().any(|tag| tag.is_executable()) =>
                                {
                                    Executable::If(r#if)
                                        .push(&mut stack)
                                        .map_err(|err| err.context(reader.error_position()))?;
                                }
                                ScxmlTag::OnEntry(execs)
                                    if stack
                                        .last()
                                        .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                                {
                                    if let Some(ScxmlTag::State(state)) = stack.last_mut() {
                                        state.on_entry = execs;
                                    }
                                }
                                ScxmlTag::OnExit(execs)
                                    if stack
                                        .last()
                                        .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                                {
                                    if let Some(ScxmlTag::State(state)) = stack.last_mut() {
                                        state.on_exit = execs;
                                    }
                                }
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
                        TAG_DATA
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Datamodel(_))) =>
                        {
                            let (ident, omg_type) = type_annotation
                                .take()
                                .ok_or(anyhow!(ParserError::NoTypeAnnotation))
                                .map_err(|err| err.context(reader.error_position()))?;
                            let data = Data::parse(tag, ident, omg_type, &mut interner)
                                .map_err(|err| err.context(reader.error_position()))?;
                            Data::push(data, &mut stack)
                                .map_err(|err| err.context(reader.error_position()))?;
                        }
                        TAG_STATE
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml(_))) =>
                        {
                            let state = State::parse(tag)
                                .map_err(|err| err.context(reader.error_position()))?;
                            state
                                .push(&mut stack)
                                .map_err(|err| err.context(reader.error_position()))?;
                        }
                        TAG_TRANSITION
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                        {
                            let transition = Transition::parse(tag, &mut interner)
                                .map_err(|err| err.context(reader.error_position()))?;
                            transition
                                .push(&mut stack)
                                .map_err(|err| err.context(reader.error_position()))?;
                        }
                        // we `rev()` the iterator only because we expect the relevant tag to be towards the end of the stack
                        TAG_RAISE if stack.last().is_some_and(|tag| tag.is_executable()) => {
                            let raise = Executable::parse_raise(tag)
                                .map_err(|err| err.context(reader.error_position()))?;
                            raise
                                .push(&mut stack)
                                .map_err(|err| err.context(reader.error_position()))?;
                        }
                        TAG_SEND if stack.last().is_some_and(|tag| tag.is_executable()) => {
                            let send = Send::parse(tag, &mut interner)
                                .map_err(|err| err.context(reader.error_position()))?;
                            Executable::Send(send)
                                .push(&mut stack)
                                .map_err(|err| err.context(reader.error_position()))?;
                        }
                        TAG_ASSIGN if stack.last().is_some_and(|tag| tag.is_executable()) => {
                            let assign = Executable::parse_assign(tag, &mut interner)
                                .map_err(|err| err.context(reader.error_position()))?;
                            assign
                                .push(&mut stack)
                                .map_err(|err| err.context(reader.error_position()))?;
                        }
                        TAG_PARAM
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Send(_))) =>
                        {
                            let (ident, omg_type) = type_annotation
                                .take()
                                .ok_or(ParserError::NoTypeAnnotation)?;
                            let param = Param::parse(tag, ident, omg_type, &mut interner)
                                .map_err(|err| err.context(reader.error_position()))?;
                            if let ScxmlTag::Send(send) =
                                stack.last_mut().expect("param must be inside other tag")
                            {
                                send.params.push(param);
                            } else {
                                unreachable!("param must be inside a send tag");
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
                Event::Comment(comment) => {
                    // Convert comment into string (is there no easier way?)
                    let comment = String::from_utf8(
                        comment
                            .bytes()
                            .collect::<Result<Vec<u8>, std::io::Error>>()?,
                    )?;
                    type_annotation = Self::parse_comment(comment)?;
                }
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

    fn parse_comment(comment: String) -> anyhow::Result<Option<(String, String)>> {
        let mut iter = comment.split_whitespace();
        let keyword = iter.next().ok_or(anyhow!("no keyword"))?;
        if keyword == "TYPE" {
            trace!("parsing TYPE magic comment");
            let body = iter.next().ok_or(anyhow!("no body"))?;
            let (ident, omg_type) = body
                .split_once(':')
                .ok_or(anyhow!("badly formatted type declaration"))?;
            trace!("found ident: {ident}, type: {omg_type}");
            Ok(Some((ident.to_string(), omg_type.to_string())))
        } else {
            Ok(None)
        }
    }
}
