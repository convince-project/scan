use super::{Expression, Identifier, LValue, VariableDeclaration};
use serde::Deserialize;

/// all expressions and assignments inside an automaton can only reference its own local
/// variables and the global variables of the enclosing model
#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Automaton {
    /// the name of the automaton, unique among all automata
    pub(crate) name: String,
    /// the local variables of the automaton
    #[serde(default)]
    pub(crate) variables: Vec<VariableDeclaration>,
    /// the locations that make up the automaton; at least one
    pub(crate) locations: Vec<Location>,
    // TODO
    // "?restrict-initial": { // restricts the initial values of the local variables of this automaton (i.e. it has no
    //                        // effect on the initial values of global variables or local variables of other automata)
    //   "exp": Expression, // the initial states expression; type bool, must not reference transient variables
    //   "?comment": String // an optional comment
    // },
    /// the automaton's initial locations
    pub(crate) initial_locations: Vec<Identifier>,
    /// the edges connecting the locations
    pub(crate) edges: Vec<Edge>,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Location {
    /// the name of the location, unique among all locations of this automaton
    pub(crate) name: Identifier,
    /// values for transient variables in this location
    #[serde(default)]
    pub(crate) transient_values: Vec<TransientValue>,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
    // TODO
    // "?time-progress": { // the location's time progress condition, not allowed except TA, PTA, STA, HA, PHA and STA,
    //                     // type bool; if omitted in TA, PTA, STA, HA, PHA or SHA, it is true
    //   "exp": Expression, // the invariant expression, type bool
    //   "?comment": String // an optional comment
    // },
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct TransientValue {
    /// what to set the value for
    pub(crate) r#ref: LValue,
    /// the value, must not contain references to transient variables or variables of type
    /// "clock" or "continuous"
    pub(crate) value: Expression,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}

#[derive(Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Edge {
    /// the edge's source location
    pub(crate) location: Identifier,
    /// the edge's action label; if omitted, the label is the silent action
    #[serde(default)]
    pub(crate) action: Option<Identifier>,
    // "?rate": { // the edge's rate, required for CTMC and CTMDP, optional for MA,
    //            // optional in DTMC where it represents the weight of the edge to be able to resolve nondeterminism,
    //            // not allowed in all other model types; if present in a MA, action must be omitted
    //   "exp": Expression, // the rate expression, type real
    //   "?comment": String // an optional comment
    // },
    /// the edge's guard; if omitted, it is true
    #[serde(default)]
    pub(crate) guard: Option<Guard>,
    /// the destinations of the edge, at least one, at most one for LTS, TA and HA
    pub(crate) destinations: Vec<Destination>,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}

#[derive(Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Guard {
    /// the guard expression, type bool
    pub(crate) exp: Expression,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}

#[derive(Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Destination {
    /// the destination's target location
    pub(crate) location: Identifier,
    /// the destination's probability, not allowed in LTS, TA and HA; if omitted, it is 1
    #[serde(default)]
    pub(crate) probability: Option<Probability>,
    /// the set of assignments to execute atomically
    #[serde(default)]
    pub(crate) assignments: Vec<Assignment>,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}

#[derive(Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Assignment {
    /// what to assign to (can be both transient and non-transient)
    pub(crate) r#ref: LValue,
    /// the new value to assign to the variable; must be of the variable's type;
    /// if the variable's type is clock, must be a clock- and sampling-free expression
    pub(crate) value: Expression,
    // TODO
    // "?index": Number.step(1), // the index, to create sequences of atomic assignment sets, default 0
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}

#[derive(Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Probability {
    /// the probability expression, type real; note that this may evaluate to zero
    pub(crate) exp: Expression,
    /// an optional comment
    #[serde(skip)]
    pub(crate) comment: String,
}
