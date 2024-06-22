use super::vocabulary::*;
use crate::parser::{ParserError, ParserErrorType};
use anyhow::anyhow;
use boa_ast::{Expression as BoaExpression, StatementListItem};
use log::{error, info, trace, warn};
use quick_xml::events::attributes::{AttrError, Attribute};
use quick_xml::{events, events::Event, Reader};
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::{BufRead, Read};
use std::str;

#[derive(Debug, Clone, PartialEq, Eq)]
enum ScxmlTag {
    State(String),
    Transition,
    Scxml,
    Datamodel,
    If,
    OnEntry,
    OnExit,
    Send,
}

impl From<ScxmlTag> for &'static str {
    fn from(value: ScxmlTag) -> Self {
        match value {
            ScxmlTag::State(_) => TAG_STATE,
            ScxmlTag::Transition => TAG_TRANSITION,
            ScxmlTag::Scxml => TAG_SCXML,
            ScxmlTag::Datamodel => TAG_DATAMODEL,
            ScxmlTag::If => TAG_IF,
            ScxmlTag::OnEntry => TAG_ONENTRY,
            ScxmlTag::OnExit => TAG_ONEXIT,
            ScxmlTag::Send => TAG_SEND,
        }
    }
}

impl ScxmlTag {
    pub fn is_executable(&self) -> bool {
        matches!(
            self,
            ScxmlTag::OnEntry | ScxmlTag::OnExit | ScxmlTag::Transition | ScxmlTag::If
        )
    }
}

#[derive(Debug, Clone)]
pub struct State {
    pub(crate) transitions: Vec<Transition>,
    pub(crate) on_entry: Vec<Executable>,
    pub(crate) on_exit: Vec<Executable>,
}

#[derive(Debug, Clone)]
pub struct Transition {
    pub(crate) event: Option<String>,
    pub(crate) target: String,
    pub(crate) cond: Option<boa_ast::Expression>,
    pub(crate) effects: Vec<Executable>,
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
    Send {
        event: String,
        target: Target,
        params: Vec<Param>,
    },
    If {
        cond: boa_ast::Expression,
        execs: Vec<Executable>,
    },
}

#[derive(Debug, Clone)]
pub struct Param {
    pub(crate) name: String,
    pub(crate) omg_type: String,
    pub(crate) expr: BoaExpression,
}

#[derive(Debug)]
pub struct Fsm {
    pub(crate) id: String,
    pub(crate) initial: String,
    pub(crate) datamodel: HashMap<String, (String, Option<boa_ast::Expression>)>,
    pub(crate) states: HashMap<String, State>,
    pub(crate) interner: boa_interner::Interner,
}

impl Fsm {
    pub(super) fn parse<R: BufRead>(reader: &mut Reader<R>) -> anyhow::Result<Self> {
        let mut fsm = Fsm {
            id: String::new(),
            initial: String::new(),
            datamodel: HashMap::new(),
            states: HashMap::new(),
            interner: boa_interner::Interner::new(),
        };
        let mut buf = Vec::new();
        let mut stack: Vec<ScxmlTag> = Vec::new();
        let mut type_annotation: Option<(String, String)> = None;
        info!("parsing fsm");
        loop {
            match reader.read_event_into(&mut buf)? {
                Event::Start(tag) => {
                    let tag_name = tag.name();
                    let tag_name = str::from_utf8(tag_name.as_ref())?;
                    trace!("'{tag_name}' open tag");
                    match tag_name {
                        TAG_SCXML if stack.is_empty() => {
                            fsm.parse_scxml(tag, reader)?;
                            stack.push(ScxmlTag::Scxml);
                        }
                        TAG_DATAMODEL
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml)) =>
                        {
                            stack.push(ScxmlTag::Datamodel);
                        }
                        TAG_STATE
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml)) =>
                        {
                            let id = fsm.parse_state(tag, reader)?;
                            stack.push(ScxmlTag::State(id));
                        }
                        TAG_TRANSITION
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                        {
                            fsm.parse_transition(tag, reader, &stack)?;
                            stack.push(ScxmlTag::Transition);
                        }
                        TAG_SEND if stack.iter().rev().any(|tag| tag.is_executable()) => {
                            fsm.parse_send(tag, reader, &stack)?;
                            stack.push(ScxmlTag::Send);
                        }
                        TAG_IF if stack.iter().rev().any(|tag| tag.is_executable()) => {
                            fsm.parse_if(tag, reader, &stack)?;
                            stack.push(ScxmlTag::If);
                        }
                        TAG_ONENTRY
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                        {
                            stack.push(ScxmlTag::OnEntry);
                        }
                        TAG_ONEXIT
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                        {
                            stack.push(ScxmlTag::OnExit);
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
                        TAG_DATA
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Datamodel)) =>
                        {
                            let (ident, omg_type) = type_annotation.take().ok_or(ParserError(
                                reader.buffer_position(),
                                ParserErrorType::NoTypeAnnotation,
                            ))?;
                            fsm.parse_data(tag, ident, omg_type, reader)?;
                        }
                        TAG_STATE
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Scxml)) =>
                        {
                            let _id = fsm.parse_state(tag, reader)?;
                        }
                        TAG_TRANSITION
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::State(_))) =>
                        {
                            fsm.parse_transition(tag, reader, &stack)?;
                        }
                        // we `rev()` the iterator only because we expect the relevant tag to be towards the end of the stack
                        TAG_RAISE if stack.last().is_some_and(|tag| tag.is_executable()) => {
                            fsm.parse_raise(tag, reader, &stack)?;
                        }
                        TAG_SEND if stack.last().is_some_and(|tag| tag.is_executable()) => {
                            fsm.parse_send(tag, reader, &stack)?;
                        }
                        TAG_ASSIGN if stack.last().is_some_and(|tag| tag.is_executable()) => {
                            fsm.parse_assign(tag, reader, &stack)?;
                        }
                        TAG_PARAM
                            if stack
                                .last()
                                .is_some_and(|tag| matches!(*tag, ScxmlTag::Send)) =>
                        {
                            let (ident, omg_type) = type_annotation.take().ok_or(ParserError(
                                reader.buffer_position(),
                                ParserErrorType::NoTypeAnnotation,
                            ))?;
                            fsm.parse_param(tag, ident, omg_type, reader, &stack)?;
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
                    type_annotation = fsm.parse_comment(comment)?;
                }
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
        Ok(fsm)
    }

    fn parse_scxml<R: BufRead>(
        &mut self,
        tag: events::BytesStart<'_>,
        _reader: &mut Reader<R>,
    ) -> anyhow::Result<()> {
        for attr in tag
            .attributes()
            .collect::<Result<Vec<Attribute>, AttrError>>()?
        {
            match str::from_utf8(attr.key.as_ref())? {
                ATTR_NAME => {
                    self.id = String::from_utf8(attr.value.into_owned())?;
                }
                ATTR_INITIAL => {
                    self.initial = String::from_utf8(attr.value.into_owned())?;
                }
                key => {
                    warn!("found unknown attribute {key} in {TAG_STATE}, ignoring");
                    continue;
                }
            }
        }
        Ok(())
    }

    fn parse_state<R: BufRead>(
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
                    error!("found unknown attribute {key} in {TAG_STATE}");
                    return Err(anyhow::Error::new(ParserError(
                        reader.buffer_position(),
                        ParserErrorType::UnknownKey(key.to_owned()),
                    )));
                }
            }
        }
        let id = id.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_ID.to_string())
        )))?;
        // Check if it is the initial state
        if self.initial.is_empty() {
            id.clone_into(&mut self.initial);
        }
        let state = State {
            transitions: Vec::new(),
            on_entry: Vec::new(),
            on_exit: Vec::new(),
        };
        // Here it should be checked that no component was already in the list under the same name
        self.states.insert(id.to_owned(), state);
        Ok(id)
    }

    fn parse_transition<R: BufRead>(
        &mut self,
        tag: events::BytesStart<'_>,
        reader: &mut Reader<R>,
        stack: &[ScxmlTag],
    ) -> anyhow::Result<()> {
        let state: &str = stack
            .iter()
            .rev()
            .find_map(|tag| {
                if let ScxmlTag::State(state) = tag {
                    Some(state)
                } else {
                    None
                }
            })
            .ok_or_else(|| ParserError(reader.buffer_position(), ParserErrorType::NotAState))?;
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
                    return Err(anyhow::Error::new(ParserError(
                        reader.buffer_position(),
                        ParserErrorType::UnknownKey(key.to_owned()),
                    )));
                }
            }
        }
        let target = target.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_TARGET.to_string())
        )))?;
        let cond = if let Some(cond) = cond {
            if let StatementListItem::Statement(boa_ast::Statement::Expression(cond)) =
                boa_parser::Parser::new(boa_parser::Source::from_bytes(cond.as_bytes()))
                    .parse_script(&mut self.interner)
                    .expect("hope this works")
                    .statements()
                    .first()
                    .expect("hopefully there is a statement")
                    .to_owned()
            {
                Some(cond)
            } else {
                return Err(anyhow!(ParserError(
                    reader.buffer_position(),
                    ParserErrorType::EcmaScriptParsing,
                )));
            }
        } else {
            None
        };
        let transition = Transition {
            event,
            target,
            cond,
            effects: Vec::new(),
        };
        // Need to know current state
        self.states
            .get_mut(state)
            .expect("the state tag has already been processed")
            .transitions
            .push(transition);
        Ok(())
    }

    fn parse_raise<R: BufRead>(
        &mut self,
        tag: events::BytesStart<'_>,
        reader: &mut Reader<R>,
        stack: &[ScxmlTag],
    ) -> anyhow::Result<()> {
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
                    return Err(anyhow::Error::new(ParserError(
                        reader.buffer_position(),
                        ParserErrorType::UnknownKey(key.to_owned()),
                    )));
                }
            }
        }
        let event = event.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_EVENT.to_string())
        )))?;
        let executable = Executable::Raise { event };
        self.add_executable(stack, reader, executable)?;
        Ok(())
    }

    fn parse_send<R: BufRead>(
        &mut self,
        tag: events::BytesStart<'_>,
        reader: &mut Reader<R>,
        stack: &[ScxmlTag],
    ) -> anyhow::Result<()> {
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
                    return Err(anyhow::Error::new(ParserError(
                        reader.buffer_position(),
                        ParserErrorType::UnknownKey(key.to_owned()),
                    )));
                }
            }
        }
        let event = event.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_EVENT.to_string())
        )))?;
        let target = if let Some(target) = target {
            Target::Id(target)
        } else if let Some(targetexpr) = targetexpr {
            if let StatementListItem::Statement(boa_ast::Statement::Expression(targetexpr)) =
                boa_parser::Parser::new(boa_parser::Source::from_bytes(targetexpr.as_bytes()))
                    .parse_script(&mut self.interner)
                    .expect("hope this works")
                    .statements()
                    .first()
                    .expect("hopefully there is a statement")
                    .to_owned()
            {
                Target::Expr(targetexpr)
            } else {
                return Err(anyhow!(ParserError(
                    reader.buffer_position(),
                    ParserErrorType::EcmaScriptParsing,
                )));
            }
        } else {
            return Err(anyhow!(ParserError(
                reader.buffer_position(),
                ParserErrorType::MissingAttr(ATTR_TARGETEXPR.to_string())
            )));
        };
        let executable = Executable::Send {
            event,
            target,
            params: Vec::new(),
        };
        self.add_executable(stack, reader, executable)?;
        Ok(())
    }

    fn parse_if<R: BufRead>(
        &mut self,
        tag: events::BytesStart<'_>,
        reader: &mut Reader<R>,
        stack: &[ScxmlTag],
    ) -> anyhow::Result<()> {
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
                    return Err(anyhow::Error::new(ParserError(
                        reader.buffer_position(),
                        ParserErrorType::UnknownKey(key.to_owned()),
                    )));
                }
            }
        }
        let cond = cond.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_COND.to_string())
        )))?;
        if let StatementListItem::Statement(boa_ast::Statement::Expression(cond)) =
            boa_parser::Parser::new(boa_parser::Source::from_bytes(cond.as_bytes()))
                .parse_script(&mut self.interner)
                .expect("hope this works")
                .statements()
                .first()
                .expect("hopefully there is a statement")
                .to_owned()
        {
            self.add_executable(
                stack,
                reader,
                Executable::If {
                    cond,
                    execs: Vec::new(),
                },
            )
        } else {
            Err(anyhow!(ParserError(
                reader.buffer_position(),
                ParserErrorType::EcmaScriptParsing,
            )))
        }
    }

    fn parse_assign<R: BufRead>(
        &mut self,
        tag: events::BytesStart<'_>,
        reader: &mut Reader<R>,
        stack: &[ScxmlTag],
    ) -> anyhow::Result<()> {
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
                    return Err(anyhow::Error::new(ParserError(
                        reader.buffer_position(),
                        ParserErrorType::UnknownKey(key.to_owned()),
                    )));
                }
            }
        }
        let location = location.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_LOCATION.to_string())
        )))?;
        let expr = expr.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_EXPR.to_string())
        )))?;
        // FIXME: This is really bad code!
        if let StatementListItem::Statement(boa_ast::Statement::Expression(expr)) =
            boa_parser::Parser::new(boa_parser::Source::from_bytes(expr.as_bytes()))
                .parse_script(&mut self.interner)
                .expect("hope this works")
                .statements()
                .first()
                .expect("hopefully there is a statement")
                .to_owned()
        {
            let executable = Executable::Assign { location, expr };
            self.add_executable(stack, reader, executable)?;
        }
        Ok(())
    }

    fn add_executable<R: BufRead>(
        &mut self,
        stack: &[ScxmlTag],
        reader: &mut Reader<R>,
        executable: Executable,
    ) -> Result<(), anyhow::Error> {
        let state_id: &str = stack
            .iter()
            .find_map(|tag| {
                if let ScxmlTag::State(state) = tag {
                    Some(state)
                } else {
                    None
                }
            })
            .ok_or_else(|| ParserError(reader.buffer_position(), ParserErrorType::NotAState))?;
        let state = self
            .states
            .get_mut(state_id)
            .expect("State in stack has to exist");
        let (i, tag) = stack
            .iter()
            .enumerate()
            .find(|(_, tag)| tag.is_executable())
            .expect("there must be an executable tag");
        let stack = &stack[i + 1..];
        match tag {
            ScxmlTag::OnEntry => {
                Self::put_executable(stack, executable, &mut state.on_entry, reader)?;
            }
            ScxmlTag::OnExit => {
                Self::put_executable(stack, executable, &mut state.on_exit, reader)?;
            }
            ScxmlTag::Transition => {
                Self::put_executable(
                    stack,
                    executable,
                    &mut state
                        .transitions
                        .last_mut()
                        .expect("inside a `Transition` tag")
                        .effects,
                    reader,
                )?;
            }
            _ => panic!("non executable tag"),
        }
        Ok(())
    }

    fn put_executable<R: BufRead>(
        stack: &[ScxmlTag],
        executable: Executable,
        into: &mut Vec<Executable>,
        reader: &mut Reader<R>,
    ) -> Result<(), anyhow::Error> {
        if stack.is_empty() {
            into.push(executable);
            Ok(())
        } else {
            match stack
                .iter()
                .find(|tag| tag.is_executable())
                .expect("there must be an executable tag")
            {
                ScxmlTag::If => match into
                    .last_mut()
                    .ok_or_else(|| anyhow!("no executable found"))?
                {
                    Executable::If { cond: _, execs } => {
                        Self::put_executable(&stack[1..], executable, execs, reader)
                    }
                    _ => Err(anyhow!("no nested executable found")),
                },
                _ => panic!("non executable tag"),
            }
        }
    }

    fn parse_param<R: BufRead>(
        &mut self,
        tag: events::BytesStart<'_>,
        ident: String,
        omg_type: String,
        reader: &mut Reader<R>,
        stack: &[ScxmlTag],
    ) -> anyhow::Result<()> {
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
                    return Err(anyhow::Error::new(ParserError(
                        reader.buffer_position(),
                        ParserErrorType::UnknownKey(key.to_owned()),
                    )));
                }
            }
        }
        let name = name.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_NAME.to_string())
        )))?;
        if name != ident {
            return Err(anyhow!(ParserError(
                reader.buffer_position(),
                ParserErrorType::NoTypeAnnotation,
            )));
        }
        let param;
        let expr = expr.or(location).ok_or(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingExpr,
        ))?;
        if let StatementListItem::Statement(boa_ast::Statement::Expression(expr)) =
            boa_parser::Parser::new(boa_parser::Source::from_bytes(expr.as_bytes()))
                .parse_script(&mut self.interner)
                .expect("hope this works")
                .statements()
                .first()
                .expect("hopefully there is a statement")
                .to_owned()
        {
            // Full parameter with either location or expression as argument
            param = Param {
                name,
                omg_type,
                expr,
            };
        } else {
            return Err(anyhow!(ParserError(
                reader.buffer_position(),
                ParserErrorType::EcmaScriptParsing,
            )));
        }

        // Find which `State` is being parsed.
        let state_id: &str = stack
            .iter()
            .rev()
            .find_map(|tag| {
                if let ScxmlTag::State(state) = tag {
                    Some(state)
                } else {
                    None
                }
            })
            .ok_or_else(|| ParserError(reader.buffer_position(), ParserErrorType::NotAState))?;
        let state = self
            .states
            .get_mut(state_id)
            .expect("State in stack has to exist");

        // Find in which executable element the `Send` (the `Param` belongs to) is.
        // The `Send` must be the last `Executable` being parsed.
        // Then, push the `Param`.
        // TODO: Handle errors.
        let (i, tag) = stack
            .iter()
            .enumerate()
            .find(|(_, tag)| tag.is_executable())
            .expect("there must be an executable tag");
        let stack = &stack[i + 1..];
        match tag {
            ScxmlTag::OnEntry => {
                if let Some(Executable::Send {
                    event: _,
                    target: _,
                    params,
                }) = state.on_entry.last_mut()
                {
                    params.push(param);
                }
            }
            ScxmlTag::OnExit => {
                if let Some(Executable::Send {
                    event: _,
                    target: _,
                    params,
                }) = state.on_exit.last_mut()
                {
                    params.push(param);
                }
            }
            ScxmlTag::Transition => {
                Self::put_param(
                    stack,
                    param,
                    &mut state
                        .transitions
                        .last_mut()
                        .expect("inside a `Transition` tag")
                        .effects,
                    reader,
                )?;
            }
            _ => panic!("non executable tag"),
        }
        Ok(())
    }

    fn put_param<R: BufRead>(
        stack: &[ScxmlTag],
        param: Param,
        into: &mut [Executable],
        reader: &mut Reader<R>,
    ) -> Result<(), anyhow::Error> {
        match stack.first().expect("there must be an executable tag") {
            ScxmlTag::Send => {
                if let Executable::Send {
                    event: _,
                    target: _,
                    params,
                } = into.last_mut().ok_or_else(|| anyhow!(""))?
                {
                    params.push(param);
                    Ok(())
                } else {
                    Err(anyhow!(""))
                }
            }
            ScxmlTag::If => match into
                .last_mut()
                .ok_or_else(|| anyhow!("no executable found"))?
            {
                Executable::If { cond: _, execs } => {
                    Self::put_param(&stack[1..], param, execs, reader)
                }
                _ => Err(anyhow!("no nested executable found")),
            },
            _ => panic!("non executable tag"),
        }
    }

    fn parse_data<R: BufRead>(
        &mut self,
        tag: events::BytesStart<'_>,
        ident: String,
        omg_type: String,
        reader: &Reader<R>,
    ) -> anyhow::Result<()> {
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
                    return Err(anyhow::Error::new(ParserError(
                        reader.buffer_position(),
                        ParserErrorType::UnknownKey(key.to_owned()),
                    )));
                }
            }
        }
        let id = id.ok_or(anyhow!(ParserError(
            reader.buffer_position(),
            ParserErrorType::MissingAttr(ATTR_ID.to_string())
        )))?;
        // Check id is matching
        if id != ident {
            return Err(anyhow!(ParserError(
                reader.buffer_position(),
                ParserErrorType::NoTypeAnnotation,
            )));
        }
        if let Some(expr) = expr {
            if let StatementListItem::Statement(boa_ast::Statement::Expression(expr)) =
                boa_parser::Parser::new(boa_parser::Source::from_bytes(expr.as_bytes()))
                    .parse_script(&mut self.interner)
                    .expect("hope this works")
                    .statements()
                    .first()
                    .expect("hopefully there is a statement")
                    .to_owned()
            {
                self.datamodel.insert(id, (omg_type, Some(expr)));
            }
        } else {
            self.datamodel.insert(id, (omg_type, None));
        }
        Ok(())
    }

    fn parse_comment(&mut self, comment: String) -> anyhow::Result<Option<(String, String)>> {
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
