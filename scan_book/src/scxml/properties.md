Properties are to be specified in a custom XML format
(as there is no property specification format in SCXML).
The following example shows the format structure and its elements:

```xml
<properties>
  <!-- First, declare the variables that are needed for expressing the properties. -->
  <ports>
    <!-- Every SCXML event can be declared as a port, -->
    <!-- by specifying its name, origin automaton and target automaton. -->
    <scxml_event_send event="event" origin="origin_automaton_name" target="target_automaton_name">
      <!-- Every port can give rise to state variables and event variables. -->

      <!-- State variables are associated to a parameter of the event, -->
      <!-- and their value correspond to that of the parameter when the event was last sent. -->
      <!-- State variables have associated type, param and initialization value -->
      <state_var id="state_variable_id" param="event_parameter" type="variable_type" expr="initial_value"/>

      <!-- Event variables are boolean variables associated only to the event, -->
      <!-- and their value is true when the event is being sent, false otherwise. -->
      <!-- Event variables have no associated type, param or value -->
      <event_var id="event_variable_id"/>
    </scxml_event_send>

    <!-- Other port declarations... -->
  </ports>

  <!-- Properties can either be specified as guarantees to verify, -->
  <!-- or as assumes to restrict the system execution. -->
  <guarantees>
    <!-- Properties are assigned an id. -->
    <!-- Currently, the only supported logic is pMTL; -->
    <!-- thus the only accepted value for the `logic` attribute is "pmtl" -->
    <!-- The property itself is expressed via the `expr` attribute. -->
    <property id="property_id" logic="pmtl" expr="pMTL_formula"/>

    <!-- Other properties... -->
  </guarantees>

  <assumes>
    <!-- Same format as guarantees -->

    <!-- Other properties... -->
  </assumes>
</properties>
```

The properties themselves are expressed in (a subset of) the [Rye](https://doganulus.github.io/reelay/rye/) format,
as already used by the [Reelay](https://doganulus.github.io/reelay/) monitoring library.
In this format, atomic predicates are written in curly braces and can include:

- Previously declared state and event variables;
- Boolean constant values (`true` and `false`);
- Boolean operators (`&&`, `||`, `!`, `->`);
- Arithmetic values (integers and float);
- Arithmetic operators (`+`, `-`, `*`, `/`, `%`).

For example, `{ battery < 30}` and ` { alarm }` (where `battery` is a state variable of numeric type, and `alarm` is an event variable) are atomic predicates.
Formulae are formed by atomic predicates, Boolean constants (`true` and `false`), and by the operators of past Metric Temporal Logic, which are:

- `&&` Boolean disjunction (and);
- `||` Boolean conjunction (or);
- `!` Boolean negation (not);
- `->` Boolean implication (if ... then ...);
- `S[a:b]` temporal Since (with optional time bounds);
- `H[a:b]` temporal Historically (with optional time bounds);
- `O[a:b]` temporal Once (with optional time bounds).

For example, `H ({ battery < 30 } S { alarm })` is a well-formed formula.
