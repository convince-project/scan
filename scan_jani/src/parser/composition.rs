use super::Identifier;
use serde::Deserialize;

/// Automata composition
#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Composition {
    pub(crate) elements: Vec<Element>,
    #[serde(default)]
    pub(crate) syncs: Vec<Sync>,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Element {
    /// the name of an automaton
    pub(crate) automaton: Identifier,
    /// a set of action names on which to make the automaton input-enabled;
    /// for CTMC and CTMDP, the new transitions have rate 1
    #[serde(default)]
    pub(crate) input_enable: Vec<Identifier>,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Sync {
    /// a list of action names or null, same length as elements
    pub(crate) synchronise: Vec<Option<Identifier>>,
    /// an action name, the result of the synchronisation; if omitted, it is the silent action
    #[serde(default)]
    pub(crate) result: Option<Identifier>,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}
