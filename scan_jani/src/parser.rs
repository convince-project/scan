use serde::Deserialize;

mod automaton;
mod composition;
mod constant_declaration;
mod expression;
mod jani_type;
mod variable_declaration;

pub(crate) use automaton::Automaton;
pub(crate) use composition::Composition;
pub(crate) use constant_declaration::ConstantDeclaration;
pub(crate) use expression::Expression;
pub(crate) use jani_type::Type;
pub(crate) use variable_declaration::VariableDeclaration;

pub(crate) type Identifier = String;
/// L-values (for assignment left-hand sides)
pub(crate) type LValue = Identifier;

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) struct Model {
    /// the jani-model version of this model
    jani_version: u8,
    /// the name of the model (e.g. the name of the underlying model file)
    name: String,
    /// the model's metadata
    #[serde(default)]
    metadata: Option<Metadata>,
    /// the model's type
    #[serde(rename = "type")]
    model_type: ModelType,
    /// extended jani-model features defined elsewhere that are used by this model
    #[serde(default)]
    features: Vec<ModelFeature>,
    /// the model's actions
    #[serde(default)]
    actions: Vec<Action>,
    /// the model's constants
    #[serde(default)]
    constants: Vec<ConstantDeclaration>,
    /// the model's global variables
    #[serde(default)]
    variables: Vec<VariableDeclaration>,
    /// the model's automata; at least one
    automata: Vec<Automaton>,
    /// the model's automata network composition expression, note that one automaton
    /// can appear multiple times (= in multiple instances)
    system: Composition,
    // TODO
    // "?restrict-initial": { // restricts the initial values of the global variables
    //   "exp": Expression, // the initial states expression, type bool, must not reference transient variables
    //   "?comment": String // an optional comment
    // },
    // "?properties": Array.of(Property), // the properties to check
}

#[derive(Deserialize)]
pub(crate) struct Metadata {
    // TODO
}

#[derive(Deserialize)]
pub(crate) struct Action {
    /// the action's name, unique among all actions
    name: Identifier,
    /// an optional comment
    #[serde(skip)]
    comment: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum ModelFeature {
    /// support for array types, defined in the Extensions section
    Arrays,
    /// support for complex datatypes, defined in the Extensions section
    Datatypes,
    /// support for some derived operators in expressions, defined in the Extensions section
    DerivedOperators,
    /// support for priorities on edges, defined in the Extensions section
    EdgePriorities,
    /// support for functions, defined in the Extensions section
    Functions,
    /// support for hyperbolic functions, defined in the Extensions section
    HyperbolicFunctions,
    /// support for named subexpressions, defined in the Extensions section
    NamedExpressions,
    /// support for nondeterministic selection in expressions, defined in the Extensions section
    NondetSelection,
    /// support for accumulating rewards when leaving a state, defined in the Extensions section
    StateExitRewards,
    /// support for multi-objective tradeoff properties, defined in the Extensions section
    TradeoffProperties,
    /// support for trigonometric functions, defined in the Extensions section
    TrigonometricFunctions,
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum ModelType {
    /// LTS: a labelled transition system (or Kripke structure or finite state automaton) (untimed)
    Lts,
    /// DTMC: a discrete-time Markov chain (untimed)
    Dtmc,
    /// CTMC: a continuous-time Markov chain (timed)
    Ctmc,
    /// MDP: a discrete-time Markov decision process (untimed)
    Mdp,
    /// CTMDP: a continuous-time Markov decision process (timed)
    Ctmdp,
    /// MA: a Markov automaton (timed)
    Ma,
    /// TA: a timed automaton (timed)
    Ta,
    /// PTA: a probabilistic timed automaton (timed)
    Pta,
    /// STA: a stochastic timed automaton (timed)
    Sta,
    /// HA: a hybrid automaton (timed)
    Ha,
    /// PHA: a probabilistic hybrid automaton (timed)
    Pha,
    /// SHA: a stochastic hybrid automaton (timed)
    Sha,
}
