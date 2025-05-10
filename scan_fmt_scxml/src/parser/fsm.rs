use super::{ecmascript, vocabulary::*};
use crate::parser::{attrs, ParserError};
use anyhow::{anyhow, bail, Context};
use boa_ast::scope::Scope;
use boa_ast::Expression as BoaExpression;
use boa_interner::Interner;
use log::{error, info, trace};
use quick_xml::events::Event;
use quick_xml::{events, Reader};
use scan_core::Time;
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
        // ident: Option<String>,
        omg_type: Option<String>,
        interner: &mut Interner,
    ) -> anyhow::Result<Data> {
        let attrs = attrs(tag, &[ATTR_ID], &[ATTR_EXPR, ATTR_TYPE])?;
        let id = attrs[ATTR_ID].to_string();
        // Check id is matching
        // if id != ident {
        //     return Err(anyhow!(ParserError::NoTypeAnnotation));
        // }
        let omg_type = attrs
            .get(ATTR_TYPE)
            .cloned()
            .or(omg_type)
            .ok_or(anyhow!(ParserError::NoTypeAnnotation))?;
        let expression = attrs
            .get(ATTR_EXPR)
            .map(|expression| ecmascript(expression, &Scope::new_global(), interner))
            .transpose()?;
        Ok(Data {
            id,
            expression,
            omg_type,
        })
    }

    fn push(self, stack: &mut [ScxmlTag]) -> anyhow::Result<()> {
        if let Some(ScxmlTag::Datamodel(datamodel)) = stack.last_mut() {
            datamodel.push(self);
            Ok(())
        } else {
            bail!("data must be inside datamodel")
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
        let attrs = attrs(tag, &[ATTR_ID], &[])?;
        Ok(State {
            id: attrs[ATTR_ID].clone(),
            transitions: Vec::new(),
            on_entry: Vec::new(),
            on_exit: Vec::new(),
        })
    }

    fn push(self, stack: &mut [ScxmlTag]) -> anyhow::Result<()> {
        if let Some(ScxmlTag::Scxml(fsm)) = stack.last_mut() {
            fsm.states.insert(self.id.to_owned(), self);
            Ok(())
        } else {
            bail!("states must be inside a scxml tag")
        }
    }

    pub fn stamp(&self){
        print!("id={}\n",self.id);
        for value in self.transitions.clone(){
            value.stamp();
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
    fn parse(tag: events::BytesStart<'_>, interner: &mut Interner) -> anyhow::Result<Transition> {
        let attrs = attrs(tag, &[ATTR_TARGET], &[ATTR_EVENT, ATTR_COND])?;
        let cond = attrs
            .get(ATTR_COND)
            .map(|expression| ecmascript(expression, &Scope::new_global(), interner))
            .transpose()?;
        Ok(Transition {
            event: attrs.get(ATTR_EVENT).cloned(),
            target: attrs[ATTR_TARGET].clone(),
            cond,
            effects: Vec::new(),
        })
    }

    fn push(self, stack: &mut [ScxmlTag]) -> anyhow::Result<()> {
        if let Some(ScxmlTag::State(state)) = stack.last_mut() {
            state.transitions.push(self);
            Ok(())
        } else {
            bail!("transitions must be inside a state")
        }
    }

    pub fn stamp(&self){
        match self.event.clone() {
            Some(value) =>println!("event={}", value),
            None =>println!("The `no event."),
        }
        print!("target={}\n",self.target);
        match self.cond.clone(){
            Some(value) => println!("cond={:?}",value),
            None =>println!("The `no cond."),
        }
        for value in self.effects.clone(){
            match value {
                Executable::Assign { location, expr }   => {
                    println!("location: {}",location);
                    println!("expr={:?}",expr);
                },
                Executable::Raise { event }=> {
                    println!("event: {}",event);
                },
                Executable::Send(Send)=>{

                }
                Executable::If(If)=>{

                }
            }
            //value.stamp();
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
        let attrs = attrs(tag, &[ATTR_EVENT], &[])?;
        let event = attrs[ATTR_EVENT].clone();
        Ok(Executable::Raise { event })
    }

    fn parse_assign(
        tag: events::BytesStart<'_>,
        interner: &mut Interner,
    ) -> anyhow::Result<Executable> {
        let attrs = attrs(tag, &[ATTR_LOCATION, ATTR_EXPR], &[])?;
        let location = attrs[ATTR_LOCATION].clone();
        let expr = ecmascript(attrs[ATTR_EXPR].as_str(), &Scope::new_global(), interner)?;
        Ok(Executable::Assign { location, expr })
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
                if r#if.else_flag {
                    r#if.r#else.push(self);
                } else {
                    r#if.r#elif
                        .last_mut()
                        .expect("vector cannot be empty")
                        .1
                        .push(self);
                }
            }
            _ => bail!("send must be inside an executable tag"),
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Send {
    pub(crate) event: String,
    pub(crate) target: Option<Target>,
    pub(crate) delay: Option<Time>,
    pub(crate) params: Vec<Param>,
}

impl Send {
    fn parse(tag: events::BytesStart<'_>, interner: &mut Interner) -> anyhow::Result<Send> {
        let attrs = attrs(
            tag,
            &[ATTR_EVENT],
            &[ATTR_TARGET, ATTR_TARGETEXPR, ATTR_DELAY],
        )?;
        let target = if let Some(target) = attrs.get(ATTR_TARGET) {
            Some(Target::Id(target.to_string()))
        } else if let Some(targetexpr) = attrs.get(ATTR_TARGETEXPR) {
            let targetexpr = ecmascript(targetexpr.as_str(), &Scope::new_global(), interner)?;
            Some(Target::Expr(targetexpr))
        } else {
            None
        };
        let delay: Option<u32> = attrs
            .get(ATTR_DELAY)
            .map(|delay| {
                delay.parse::<u32>().with_context(|| {
                    format!("failed to parse 'delay' attribute value '{delay}' as integer")
                })
            })
            .transpose()?;
        Ok(Send {
            event: attrs[ATTR_EVENT].to_owned(),
            target,
            delay,
            params: Vec::new(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub(crate) r#elif: Vec<(boa_ast::Expression, Vec<Executable>)>,
    pub(crate) r#else: Vec<Executable>,
    else_flag: bool,
}

impl If {
    fn parse(
        tag: events::BytesStart<'_>,
        interner: &mut Interner,
    ) -> anyhow::Result<boa_ast::Expression> {
        let attrs = attrs(tag, &[ATTR_COND], &[])?;
        ecmascript(attrs[ATTR_COND].as_str(), &Scope::new_global(), interner)
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub(crate) name: String,
    pub(crate) omg_type: Option<String>,
    pub(crate) expr: BoaExpression,
}

impl Param {
    fn parse(
        tag: events::BytesStart<'_>,
        omg_type: Option<String>,
        interner: &mut Interner,
    ) -> anyhow::Result<Param> {
        let attrs = attrs(tag, &[ATTR_NAME], &[ATTR_TYPE, ATTR_LOCATION, ATTR_EXPR])?;
        let name = attrs[ATTR_NAME].clone();
        let omg_type = omg_type.or(attrs.get(ATTR_TYPE).cloned());
        // .ok_or(anyhow!(ParserError::NoTypeAnnotation))?;
        // if name != ident {
        //     return Err(anyhow!(ParserError::NoTypeAnnotation));
        // }
        let expr = attrs
            .get(ATTR_LOCATION)
            .or_else(|| attrs.get(ATTR_EXPR))
            .ok_or_else(|| anyhow!("missing expression or location attribute"))?
            .as_str();
        let expr = ecmascript(expr, &Scope::new_global(), interner)?;
        // Full parameter with either location or expression as argument
        Ok(Param {
            name,
            omg_type,
            expr,
        })
    }
}

#[derive(Debug)]
pub struct Scxml {
    pub(crate) name: String,
    pub(crate) initial: String,
    pub(crate) datamodel: Vec<Data>,
    pub(crate) states: HashMap<String, State>,
}

impl Scxml {
    fn parse(tag: events::BytesStart<'_>) -> anyhow::Result<Scxml> {
        let attrs = attrs(
            tag,
            &[ATTR_NAME, ATTR_INITIAL],
            &[ATTR_VERSION, ATTR_DATAMODEL, ATTR_XMLNS, ATTR_MODEL_SRC],
        )
        .with_context(|| format!("failed to parse '{TAG_SCXML}' tag attributes"))?;
        Ok(Scxml {
            name: attrs[ATTR_NAME].clone(),
            initial: attrs[ATTR_INITIAL].clone(),
            datamodel: Vec::new(),
            states: HashMap::new(),
        })
    }

    pub fn stamp(&self){
        print!("id={}\n",self.name);
        print!("initial={}\n",self.initial);
        for(key,value) in self.states.clone(){
            value.stamp();
        }
    }
}

pub(super) fn parse<R: BufRead>(
    reader: &mut Reader<R>,
    interner: &mut Interner,
) -> anyhow::Result<Scxml> {
    let mut buf = Vec::new();
    let mut stack: Vec<ScxmlTag> = Vec::new();
    // let mut type_annotation: Option<(String, String)> = None;
    let mut type_annotation: Option<String> = None;
    info!(target: "parser", "parsing fsm");
    loop {
        match reader
            .read_event_into(&mut buf)
            .context("failed reading event")?
        {
            Event::Start(tag) => {
                let tag_name = reader
                    .decoder()
                    .decode(tag.name().into_inner())?
                    .into_owned();
                trace!(target: "parser", "start tag '{tag_name}'");
                let tag_obj = parse_start_tag(tag_name, &stack, tag, interner)?;
                stack.push(tag_obj);
            }
            Event::End(tag) => {
                let tag_name = &*reader.decoder().decode(tag.name().into_inner())?;
                // if let Some(tag) = stack.pop().is_some_and(|tag| <&str>::from(tag) == tag_name) {
                if let Some(tag) = stack.pop() {
                    if <&str>::from(&tag) != tag_name {
                        error!(target: "parser", "unknown or unexpected end tag '{tag_name}'");
                        bail!(ParserError::UnexpectedEndTag(tag_name.to_string()));
                    } else {
                        trace!(target: "parser", "end tag '{}'", tag_name);
                        match tag {
                            ScxmlTag::Scxml(fsm) if stack.is_empty() => {
                                return Ok(fsm);
                            }
                            ScxmlTag::Datamodel(datamodel)
                                if stack
                                    .last()
                                    .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml(_))) =>
                            {
                                if let Some(ScxmlTag::Scxml(fsm)) = stack.last_mut() {
                                    fsm.datamodel.extend_from_slice(&datamodel);
                                } else {
                                    unreachable!("tag '{TAG_DATAMODEL}' must be a child of tag '{TAG_SCXML}'");
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
                                transition.push(&mut stack)?;
                            }
                            ScxmlTag::Send(send)
                                if stack.iter().rev().any(|tag| tag.is_executable()) =>
                            {
                                Executable::Send(send).push(&mut stack)?;
                            }
                            ScxmlTag::If(r#if)
                                if stack.iter().rev().any(|tag| tag.is_executable()) =>
                            {
                                Executable::If(r#if).push(&mut stack)?;
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
                    error!(target: "parser", "unexpected end tag {tag_name}");
                    bail!(ParserError::UnexpectedEndTag(tag_name.to_string()));
                }
            }
            Event::Empty(tag) => {
                let tag_name = reader
                    .decoder()
                    .decode(tag.name().into_inner())?
                    .into_owned();
                parse_empty_tag(tag_name, &mut stack, tag, &mut type_annotation, interner)?;
            }
            // Ignore text between tags
            Event::Text(_) => continue,
            Event::Comment(comment) => {
                // Convert comment into string (is there no easier way?)
                let comment = comment
                    .bytes()
                    .collect::<Result<Vec<u8>, std::io::Error>>()
                    .with_context(|| reader.buffer_position())?;
                let comment =
                    String::from_utf8(comment).with_context(|| reader.buffer_position())?;
                type_annotation =
                    parse_comment(comment).with_context(|| reader.buffer_position())?;
            }
            Event::CData(_) => {
                return Err(anyhow!("CData not supported"))
                    .with_context(|| reader.buffer_position());
            }
            // Ignore XML declaration
            Event::Decl(_) => continue,
            Event::PI(_) => {
                return Err(anyhow!("Processing Instructions not supported"))
                    .with_context(|| reader.buffer_position());
            }
            Event::DocType(_) => {
                return Err(anyhow!("DocType not supported"))
                    .with_context(|| reader.buffer_position());
            }
            // exits the loop when reaching end of file
            Event::Eof => {
                error!(target: "parser", "parsing completed with unclosed tags");
                return Err(anyhow!(ParserError::UnclosedTags))
                    .with_context(|| reader.buffer_position());
            }
        }
        // if we don't keep a borrow elsewhere, we can clear the buffer to keep memory usage low
        buf.clear();
    }
}

fn parse_empty_tag(
    tag_name: String,
    stack: &mut [ScxmlTag],
    tag: events::BytesStart<'_>,
    type_annotation: &mut Option<String>,
    interner: &mut Interner,
) -> Result<(), anyhow::Error> {
    trace!(target: "parser", "'{tag_name}' empty tag");
    match tag_name.as_str() {
        TAG_DATA
            if stack
                .last()
                .is_some_and(|tag| matches!(*tag, ScxmlTag::Datamodel(_))) =>
        {
            let data = Data::parse(tag, type_annotation.take(), interner)
                .with_context(|| ParserError::Tag(tag_name))?;
            Data::push(data, stack)?;
        }
        TAG_STATE
            if stack
                .last()
                .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml(_))) =>
        {
            let state = State::parse(tag).with_context(|| ParserError::Tag(tag_name))?;
            state.push(stack)?;
        }
        TAG_TRANSITION
            if stack
                .last()
                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
        {
            let transition =
                Transition::parse(tag, interner).with_context(|| ParserError::Tag(tag_name))?;
            transition.push(stack)?;
        }
        // we `rev()` the iterator only because we expect the relevant tag to be towards the end of the stack
        TAG_RAISE if stack.last().is_some_and(|tag| tag.is_executable()) => {
            let raise = Executable::parse_raise(tag).with_context(|| ParserError::Tag(tag_name))?;
            raise.push(stack)?;
        }
        TAG_SEND if stack.last().is_some_and(|tag| tag.is_executable()) => {
            let send = Send::parse(tag, interner).with_context(|| ParserError::Tag(tag_name))?;
            Executable::Send(send).push(stack)?;
        }
        TAG_ASSIGN if stack.last().is_some_and(|tag| tag.is_executable()) => {
            let assign = Executable::parse_assign(tag, interner)
                .with_context(|| ParserError::Tag(tag_name))?;
            assign.push(stack)?;
        }
        TAG_PARAM
            if stack
                .last()
                .is_some_and(|tag| matches!(*tag, ScxmlTag::Send(_))) =>
        {
            // let (ident, omg_type) = type_annotation
            //     .take()
            //     .ok_or(anyhow::Error::from(ParserError::NoTypeAnnotation))
            //     .with_context(|| reader.buffer_position())?;
            let param = Param::parse(tag, type_annotation.take(), interner)
                .with_context(|| ParserError::Tag(tag_name))?;
            if let ScxmlTag::Send(send) = stack.last_mut().expect("param must be inside other tag")
            {
                send.params.push(param);
            } else {
                unreachable!("param must be inside a send tag");
            }
        }
        TAG_ELSE
            if stack
                .last()
                .is_some_and(|tag| matches!(tag, ScxmlTag::If(_))) =>
        {
            if let Some(ScxmlTag::If(r#if)) = stack.last_mut() {
                if r#if.else_flag {
                    bail!("multiple `else` inside `if` tag");
                } else {
                    r#if.else_flag = true;
                }
            } else {
                unreachable!()
            }
        }
        TAG_ELIF
            if stack
                .last()
                .is_some_and(|tag| matches!(tag, ScxmlTag::If(_))) =>
        {
            if let Some(ScxmlTag::If(r#if)) = stack.last_mut() {
                let cond = If::parse(tag, interner).with_context(|| ParserError::Tag(tag_name))?;
                r#if.elif.push((cond, Vec::new()));
            } else {
                unreachable!()
            }
        }
        _ => {
            error!(target: "parser", "unknown or unexpected empty tag '{tag_name}'");
            bail!(ParserError::UnexpectedTag(tag_name.to_string()));
        }
    };
    Ok(())
}

fn parse_start_tag(
    tag_name: String,
    stack: &[ScxmlTag],
    tag: events::BytesStart<'_>,
    interner: &mut Interner,
) -> Result<ScxmlTag, anyhow::Error> {
    match tag_name.as_str() {
        TAG_SCXML if stack.is_empty() => Scxml::parse(tag).map(ScxmlTag::Scxml),
        TAG_DATAMODEL
            if stack
                .last()
                .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml(_))) =>
        {
            Ok(ScxmlTag::Datamodel(Vec::new()))
        }
        TAG_STATE
            if stack
                .last()
                .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml(_))) =>
        {
            State::parse(tag).map(ScxmlTag::State)
        }
        TAG_TRANSITION
            if stack
                .last()
                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
        {
            Transition::parse(tag, interner).map(ScxmlTag::Transition)
        }
        TAG_SEND if stack.iter().rev().any(|tag| tag.is_executable()) => {
            Send::parse(tag, interner).map(ScxmlTag::Send)
        }
        TAG_IF if stack.iter().rev().any(|tag| tag.is_executable()) => If::parse(tag, interner)
            .map(|cond| If {
                elif: vec![(cond, Vec::new())],
                r#else: Vec::new(),
                else_flag: false,
            })
            .map(ScxmlTag::If),
        TAG_ONENTRY
            if stack
                .last()
                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
        {
            Ok(ScxmlTag::OnEntry(Vec::new()))
        }
        TAG_ONEXIT
            if stack
                .last()
                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
        {
            Ok(ScxmlTag::OnExit(Vec::new()))
        }
        _ => {
            error!(target: "parser", "unknown or unexpected start tag '{tag_name}'");
            bail!(ParserError::UnexpectedStartTag(tag_name.to_string()));
        }
    }
    .with_context(|| ParserError::Tag(tag_name.to_string()))
}

fn parse_comment(comment: String) -> anyhow::Result<Option<String>> {
    let mut iter = comment.split_whitespace();
    let keyword = iter.next().ok_or(anyhow!("no keyword"))?;
    if keyword == "TYPE" {
        trace!(target: "parser", "parsing TYPE magic comment");
        let body = iter.next().ok_or(anyhow!("no body"))?;
        let (ident, omg_type) = body
            .split_once(':')
            .ok_or(anyhow!("badly formatted type declaration"))?;
        trace!(target: "parser", "found ident: {ident}, type: {omg_type}");
        // Ok(Some((ident.to_string(), omg_type.to_string())))
        Ok(Some(omg_type.to_string()))
    } else {
        Ok(None)
    }
}
