use crate::spinv4_y::*;
use rand::rngs::SmallRng;
use scan_core::channel_system::CsError;
use scan_core::channel_system::{Channel, ChannelSystemBuilder, Location, PgId, Var};
use scan_core::{Expression, Type, Val};
use std::collections::HashMap;
use std::fmt;

/// Enumerates all possible errors that can occur during the translation from Promela AST to a Channel System.
#[derive(Debug)]
pub enum BuilderError {
    UnsupportedModule(String),
    UnsupportedStatement(String),
    UnsupportedExpression(String),
    UnsupportedStep(String),
    UnsupportedDeclaration(String),
    TypeError(String),
    UnsupportedInit(String),
    UnsupportedNever(String),
    UnsupportedTrace(String),
    UnsupportedUtype(String),
    UnsupportedDeclList(String),
    ScanCoreError(CsError),
}

/// Allows automatic conversion from SCAN's internal errors (`CsError`) to our custom `BuilderError`.
/// This is necessary to use the `?` operator on functions that return `Result<_, CsError>`.
impl From<CsError> for BuilderError {
    fn from(err: CsError) -> Self {
        BuilderError::ScanCoreError(err)
    }
}

impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuilderError::UnsupportedModule(s) => write!(f, "Unsupported module: {s}"),
            BuilderError::UnsupportedStatement(s) => write!(f, "Unsupported statement: {s}"),
            BuilderError::UnsupportedExpression(s) => write!(f, "Unsupported expression: {s}"),
            BuilderError::UnsupportedStep(s) => write!(f, "Unsupported step: {s}"),
            BuilderError::UnsupportedDeclaration(s) => write!(f, "Unsupported declaration: {s}"),
            BuilderError::TypeError(s) => write!(f, "Type error: {s}"),
            BuilderError::UnsupportedInit(s) => write!(f, "Unsupported init: {s}"),
            BuilderError::UnsupportedNever(s) => write!(f, "Unsupported never-claim: {s}"),
            BuilderError::UnsupportedTrace(s) => write!(f, "Unsupported trace: {s}"),
            BuilderError::UnsupportedUtype(s) => write!(f, "Unsupported utype: {s}"),
            BuilderError::UnsupportedDeclList(s) => write!(f, "Unsupported declaration list: {s}"),
            BuilderError::ScanCoreError(e) => write!(f, "SCAN core error: {e}"),
        }
    }
}

//------------------------ STRUCTS-------------------------------------

/// The operational context for building a single Program-Graph (for one `proctype`).
/// It holds all local definitions and a mutable reference to the main builder.
struct PgCtx<'a> {
    /// Mutable reference to the global `ChannelSystemBuilder`.
    cs: &'a mut ChannelSystemBuilder<SmallRng>,
    /// Identifier of the program‑graph currently under construction.
    pg: PgId,
    /// Symbol table for local variables: maps a Promela identifier to a SCAN `Var` handle.                              
    vars: HashMap<String, scan_core::channel_system::Var>,
    /// Caches the static `Type` of each SCAN variable handle.
    type_vars: HashMap<scan_core::channel_system::Var, Type>,
    /// Symbol table for local channels: maps a Promela identifier to a SCAN `Channel` handle.
    chans: HashMap<String, Channel>,
}

/// The global context, holding identifiers declared outside any `proctype`.
/// Note: SCAN does not have a concept of shared variables. This context is used to
/// manage global declarations, which are currently instantiated locally in each process.
#[derive(Default)]
struct GlobalCtx {
    /// Symbol table for global variables.
    vars: HashMap<String, Var>,
    /// Type cache for global variables.
    types: HashMap<Var, Type>,
    /// Symbol table for global channels.    
    chans: HashMap<String, Channel>,
}

/// The main builder struct, containing the translation logic.
pub struct Builder;

enum GuardClass<'a> {
    Normal(Expression<Var>, &'a [Step]),
    Else(&'a [Step]),
}

//------------------------ HELPER FUNCTION -------------------------------------

/// Looks up a variable's `Var` handle by its name.
/// The search order is: local context (`PgCtx`) first, then global context (`GlobalCtx`).
///
/// # Limitations
/// * Does not support array indexing or record field access.
/// * Does not handle variable shadowing.
fn lookup_var<'a>(vr: &Varref, ctx: &PgCtx<'a>, gctx: &GlobalCtx) -> Result<Var, BuilderError> {
    if let Some(&v) = ctx.vars.get(&vr.name.to_string()) {
        Ok(v)
    } else if let Some(&v) = gctx.vars.get(&vr.name.to_string()) {
        Ok(v)
    } else {
        Err(BuilderError::UnsupportedExpression(format!(
            "undefined {}",
            vr.name
        )))
    }
}

/// Looks up a channel's `Channel` handle by its name.
/// The search order is: local context (`PgCtx`) first, then global context (`GlobalCtx`).
fn lookup_chan(vr: &Varref, ctx: &PgCtx<'_>, gctx: &GlobalCtx) -> Result<Channel, BuilderError> {
    ctx.chans
        .get(&vr.name.to_string())
        .copied()
        .or_else(|| gctx.chans.get(&vr.name.to_string()).copied())
        .ok_or_else(|| {
            BuilderError::UnsupportedExpression(format!("undefined channel {}", vr.name))
        })
}

/// Converts a Promela `Typename` into a SCAN `Type`.
/// Note: Promela's different integer sizes (byte, short, int) are all mapped to `Type::Integer`.
pub fn typename_to_type(tn: &Typename) -> Result<Type, BuilderError> {
    use Typename::*;
    Ok(match tn {
        Bool | Bit => Type::Boolean,
        Byte | Short | Int => Type::Integer,
        Chan => {
            return Err(BuilderError::TypeError(
                "Typename::Chan cannot be converted to a value type".into(),
            ));
        }
        other => return Err(BuilderError::UnsupportedInit(format!("typename {other:?}"))),
    })
}

/// Allocates a fresh, unnamed `Location` in the current Program-Graph.
fn fresh_loc<'a>(ctx: &mut PgCtx<'a>) -> Result<Location, BuilderError> {
    ctx.cs
        .new_location(ctx.pg)
        .map_err(|_| BuilderError::TypeError("new loc".into()))
}

/// Registers all variable declarations from a `DeclList` as local variables in the current `PgCtx`.
/// This is primarily used to handle formal parameters of a `proctype`.
fn register_decl_list_as_locals(dl: &DeclList, ctx: &mut PgCtx<'_>) -> Result<(), BuilderError> {
    // We pass a temporary, empty `GlobalCtx` to `visit_one_decl` to ensure
    // that these declarations are not added to the actual global scope.
    for one in &dl.decls {
        let _ = Builder::visit_one_decl(one, ctx, &mut GlobalCtx::default())?;
    }
    Ok(())
}

//------------------------ CORE IMPLEMENTATION -------------------------------------

impl Builder {
    /// Main entry point of the builder.
    ///
    /// Consumes the parsed Promela AST and returns a fully built `ChannelSystem`.
    pub fn create_channel_system(
        ast: Vec<Module>,
    ) -> Result<ChannelSystemBuilder<SmallRng>, BuilderError> {
        let mut cs_builder = ChannelSystemBuilder::new();
        let mut gctx = GlobalCtx::default();
        Self::visit_ast(ast, &mut cs_builder, &mut gctx)?;
        Ok(cs_builder)
    }

    /// Top-level visitor that dispatches to the appropriate handler for each `Module` type.
    fn visit_ast(
        ast: Vec<Module>,
        cs_builder: &mut ChannelSystemBuilder<SmallRng>,
        gctx: &mut GlobalCtx,
    ) -> Result<(), BuilderError> {
        for module in ast {
            match module {
                Module::Proctype(p) => Self::visit_proctype(&p, cs_builder, gctx)?,
                Module::Init(init) => Self::visit_init(&init, cs_builder, gctx)?,
                Module::Never(never) => Self::visit_never(&never, cs_builder, gctx)?,
                Module::Trace(trace) => Self::visit_trace(&trace, cs_builder, gctx)?,
                Module::Utype(ut) => Self::visit_utype(&ut, gctx)?,
                Module::DeclList(dl) => Self::visit_decl_list(&dl, cs_builder, gctx)?,

                _ => return Err(BuilderError::UnsupportedModule(format!("{:?}", module))),
            }
        }
        Ok(())
    }

    /// Handles a top-level DeclList.
    /// Policy: only `chan` declarations are allowed at top level; any global var is rejected.
    fn visit_decl_list(
        dl: &DeclList,
        cs: &mut ChannelSystemBuilder<SmallRng>,
        gctx: &mut GlobalCtx,
    ) -> Result<(), BuilderError> {
        // ---- pre-scan: ensure ALL decls are channels ----
        for one in &dl.decls {
            match one {
                OneDecl::Var(vd) => {
                    if !matches!(vd.typename, Typename::Chan) {
                        return Err(BuilderError::UnsupportedDeclList(
                            "Top-level DeclList must contain only `chan` declarations; \
                            global variables are not supported (SCAN does not provide shared variables across Program Graphs)."
                                .into(),
                        ));
                    }
                }
                // Other decl-kinds are not allowed at top level either
                _ => {
                    return Err(BuilderError::UnsupportedDeclList(
                        "Top-level DeclList supports only `chan` declarations.".into(),
                    ));
                }
            }
        }

        // ---- creation ----
        // We still reuse the existing per-PG helper by allocating a dummy PG.
        // Channels in SCAN are global, so this is safe; no Vars are created here.
        let pg = cs.new_program_graph();
        cs.new_initial_location(pg)
            .map_err(|_| BuilderError::UnsupportedDeclList("initial location".into()))?;

        let mut lctx = PgCtx {
            cs,
            pg,
            vars: HashMap::new(),
            type_vars: HashMap::new(),
            chans: HashMap::new(),
        };

        for decl in &dl.decls {
            // visit_one_decl will create channels and register them in gctx.chans
            // (it returns an empty map for channels; no Vars are produced here).
            let _ = Self::visit_one_decl(decl, &mut lctx, gctx)?;
        }

        Ok(())
    }

    /// Translates a Promela `proctype` into a complete Program-Graph.
    /// This function sets up the context and then sequentially builds the graph
    /// by visiting each `Step` in the process body.
    fn visit_proctype(
        proctype: &Proctype,
        cs_builder: &mut ChannelSystemBuilder<SmallRng>,
        gctx: &mut GlobalCtx,
    ) -> Result<(), BuilderError> {
        let pg = cs_builder.new_program_graph();

        let mut ctx = PgCtx {
            cs: cs_builder,
            pg,
            vars: HashMap::new(),
            type_vars: HashMap::new(),
            chans: HashMap::new(),
        };

        // Formal parameters become *local* variables.
        if let Some(params) = &proctype.decl_list {
            register_decl_list_as_locals(params, &mut ctx)?;
        }

        // Initial location.
        let initial_loc = ctx
            .cs
            .new_initial_location(pg)
            .map_err(|_| BuilderError::TypeError("initial loc".into()))?;

        // This variable tracks the current point of control flow during construction.
        let mut current_loc = initial_loc;

        // Sequentially translate every step in the body.
        for step in &proctype.sequence.steps {
            current_loc = Self::visit_step(step, current_loc, &mut ctx, gctx)?;
        }
        Ok(())
    }

    /// Translates a single `Step` from a Promela sequence.
    /// A step can be either a declaration or a statement.
    /// Returns the location *after* the step has been executed.
    fn visit_step(
        step: &Step,
        entry_loc: Location,
        ctx: &mut PgCtx<'_>,
        gctx: &mut GlobalCtx,
    ) -> Result<Location, BuilderError> {
        match step {
            // ---------------------------- Statement ----------------------------
            Step::Statement(stmt_box, _unless) => {
                Self::visit_statement(stmt_box, entry_loc, ctx, gctx)
            }

            // --------------------------- Declaration ---------------------------
            Step::Declaration(decl_box) => {
                let _ = Self::visit_one_decl(decl_box, ctx, gctx)?;
                // Declarations do not create transitions or advance control flow.
                Ok(entry_loc)
            }

            // ------------------------------ XR / XS ----------------------------
            Step::XR(_) | Step::XS(_) => Err(BuilderError::UnsupportedStep(
                "XR/XS receive statements are not supported".into(),
            )),
        }
    }

    /// Handles a single declaration line (`OneDecl`), creating variables or channels.
    fn visit_one_decl(
        decl: &OneDecl,
        ctx: &mut PgCtx<'_>,
        gctx: &mut GlobalCtx,
    ) -> Result<HashMap<String, Var>, BuilderError> {
        let mut created = HashMap::new();

        use OneDecl::*;
        match decl {
            Var(var_decl) if matches!(var_decl.typename, Typename::Chan) => {
                for ivar in &var_decl.ivars {
                    let ch_handle = match &ivar.init {
                        // Simple declaration: `chan q;`.
                        // Promela would default to a synchronous channel (capacity 0).
                        // Since SCAN requires a static type and our context doesn't support
                        // full synchronicity yet, we make a design choice:
                        // Default to an INFINITE capacity channel of INTEGERS.
                        Some(OptInit::ChInit(ch_init)) => {
                            Self::channel_initialization(ch_init, ctx)?
                        }
                        None => ctx.cs.new_channel(Type::Integer, None),
                        _ => {
                            return Err(BuilderError::UnsupportedDeclaration(
                                "illegal init on chan".into(),
                            ));
                        }
                    };

                    ctx.chans.insert(ivar.name.to_string(), ch_handle);
                    gctx.chans.insert(ivar.name.to_string(), ch_handle);
                }
                Ok(HashMap::new())
            }

            Var(var_decl) => {
                if var_decl.visible.is_some() {
                    return Err(BuilderError::UnsupportedDeclaration(
                        "visibility not handled".into(),
                    ));
                }

                for ivar in &var_decl.ivars {
                    let init_expr = match &ivar.init {
                        Some(OptInit::Expr(any_expr)) => Self::translate_expr(any_expr, ctx, gctx)?,
                        Some(OptInit::ChInit(_chi)) => {
                            // TODO: channel initialisation → Expression
                            return Err(BuilderError::UnsupportedDeclaration(
                                "channel init not yet handled".into(),
                            ));
                        }
                        None => Expression::Const(Val::Integer(0)),
                    };

                    if ivar.array_dims.is_some() {
                        return Err(BuilderError::UnsupportedDeclaration(
                            "arrays not yet supported".into(),
                        ));
                    }
                    let ty = init_expr
                        .r#type()
                        .map_err(|_| BuilderError::TypeError("type".into()))?;
                    let v = ctx
                        .cs
                        .new_var(ctx.pg, init_expr)
                        .map_err(|_| BuilderError::TypeError("init".into()))?;
                    ctx.vars.insert(ivar.name.to_string(), v);
                    created.insert(ivar.name.to_string(), v);
                    ctx.type_vars.insert(v, ty);
                }
                Ok(created)
            }
            _ => Err(BuilderError::UnsupportedDeclaration(format!("{:?}", decl))),
        }
    }

    /// Recursively translates a Promela `AnyExpr` into a SCAN `Expression`.
    fn translate_expr(
        e: &AnyExpr,
        ctx: &PgCtx<'_>,
        gctx: &GlobalCtx,
    ) -> Result<Expression<scan_core::channel_system::Var>, BuilderError> {
        use AnyExpr::*;

        Ok(match e {
            Const(inner) => match inner.as_ref() {
                crate::spinv4_y::Const::Number(i) => Expression::Const(Val::Integer(*i)),
                crate::spinv4_y::Const::True => Expression::Const(Val::Boolean(true)),
                crate::spinv4_y::Const::False => Expression::Const(Val::Boolean(false)),
                crate::spinv4_y::Const::Skip => Expression::Const(Val::Boolean(true)),
            },

            // TODO: da gestire
            Timeout => Expression::Const(Val::Boolean(false)), // placeholder
            Np => return Err(BuilderError::UnsupportedExpression("const ::np".into())),

            Varref(vr) => {
                if let Some(var) = ctx.vars.get(&vr.name.to_string()) {
                    let ty = ctx.type_vars.get(var).cloned().unwrap_or(Type::Integer);
                    Expression::Var(*var, ty)
                } else {
                    let var = gctx.vars.get(&vr.name.to_string()).ok_or_else(|| {
                        BuilderError::UnsupportedExpression(format!("undefined {}", vr.name))
                    })?;
                    let ty = gctx.types.get(var).cloned().unwrap_or(Type::Integer);
                    Expression::Var(*var, ty)
                }
            }

            Unary(Unarop::Not, sub) => {
                Expression::Not(Box::new(Self::translate_expr(sub, ctx, gctx)?))
            }
            Negate(sub) => Expression::Opposite(Box::new(Self::translate_expr(sub, ctx, gctx)?)),

            Add(l, r) => Expression::Sum(vec![
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ]),
            Subtract(l, r) => Expression::Sum(vec![
                Self::translate_expr(l, ctx, gctx)?,
                Expression::Opposite(Box::new(Self::translate_expr(r, ctx, gctx)?)),
            ]),
            Multiply(l, r) => Expression::Mult(vec![
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ]),
            Divide(l, r) => Expression::Div(Box::new((
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ))),
            Modulo(l, r) => Expression::Mod(Box::new((
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ))),

            Equal(l, r) => Expression::Equal(Box::new((
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ))),
            NotEqual(l, r) => Expression::Not(Box::new(Expression::Equal(Box::new((
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ))))),
            GreaterThan(l, r) => Expression::Greater(Box::new((
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ))),
            GreaterEqual(l, r) => Expression::GreaterEq(Box::new((
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ))),
            LessThan(l, r) => Expression::Less(Box::new((
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ))),
            LessEqual(l, r) => Expression::LessEq(Box::new((
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ))),

            Logical(l, LogicalOp::And, r) | Andor(l, r) => Expression::And(vec![
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ]),
            Logical(l, LogicalOp::Or, r) => Expression::Or(vec![
                Self::translate_expr(l, ctx, gctx)?,
                Self::translate_expr(r, ctx, gctx)?,
            ]),

            Ternary(c, a, b) => Expression::Ite(Box::new((
                Self::translate_expr(c, ctx, gctx)?,
                Self::translate_expr(a, ctx, gctx)?,
                Self::translate_expr(b, ctx, gctx)?,
            ))),

            Len(_) => {
                return Err(BuilderError::UnsupportedExpression(
                    "len() not yet supported".into(),
                ));
            }

            other => return Err(BuilderError::UnsupportedExpression(format!("{:?}", other))),
        })
    }

    /// Send to the correct handler for each `Stmnt` (statement) type.
    ///
    /// This is the core of the sequential graph construction. Each call takes an
    /// input location (`in_l`) and returns the location the control flow is at
    /// after the statement has been executed.
    fn visit_statement(
        s: &Stmnt,
        in_l: Location,
        ctx: &mut PgCtx<'_>,
        gctx: &mut GlobalCtx,
    ) -> Result<Location, BuilderError> {
        use Stmnt::*;
        match s {
            Atomic(seq) | DStep(seq) => {
                let act = ctx
                    .cs
                    .new_action(ctx.pg)
                    .map_err(|_| BuilderError::TypeError("action".into()))?;
                let mut cur_l = in_l;
                for step in &seq.steps {
                    cur_l = Self::visit_step(step, cur_l, ctx, gctx)?;
                }
                // link with only one action
                ctx.cs
                    .add_transition(ctx.pg, in_l, act, cur_l, None)
                    .map_err(|_| BuilderError::TypeError("transition".into()))?;
                Ok(cur_l)
            }

            // 1. skip / ε-transition ---------------------------------------------q
            Block(seq) if seq.steps.is_empty() => {
                let out = fresh_loc(ctx)?;
                let act = ctx
                    .cs
                    .new_action(ctx.pg)
                    .map_err(|_| BuilderError::TypeError("action".into()))?;
                ctx.cs
                    .add_transition(ctx.pg, in_l, act, out, None)
                    .map_err(|_| BuilderError::TypeError("transition".into()))?;
                Ok(out)
            }

            Expr(expr_box) if matches!(&**expr_box, AnyExpr::Chanpoll(_, _)) => {
                let AnyExpr::Chanpoll(kind_box, chan_expr) = &**expr_box else {
                    unreachable!()
                };
                Self::gen_probe(in_l, kind_box, chan_expr, ctx, gctx)
            }

            // skip pure expressions
            Expr(_) => {
                let out = fresh_loc(ctx)?;
                let act = ctx
                    .cs
                    .new_action(ctx.pg)
                    .map_err(|_| BuilderError::TypeError("action".into()))?;
                ctx.cs
                    .add_transition(ctx.pg, in_l, act, out, None)
                    .map_err(|_| BuilderError::TypeError("transition".into()))?;
                Ok(out)
            }

            // 2. assignment
            Assign(assign_box) => {
                // extract fields from `Assign` struct
                let a = &**assign_box;
                let out = Self::gen_assign(in_l, &a.varref, &a.op, a.expr.as_ref(), ctx, gctx)?;
                Ok(out)
            }

            // 3. assert
            Assert(cond) => Self::gen_assert(in_l, cond, ctx, gctx),

            // 4. block { S1; S2; }
            Block(seq) => {
                let mut cur = in_l;
                for step in &seq.steps {
                    cur = Self::visit_step(step, cur, ctx, gctx)?;
                }
                Ok(cur)
            }

            // 5. send
            SendMsg(msg_box) => Self::gen_send(in_l, msg_box, ctx, gctx),

            // 6. receive
            Receive(recv_box) => Self::gen_recv(in_l, recv_box, ctx, gctx),

            // 7. if
            If(opts) => Self::gen_branching(in_l, opts, false, ctx, gctx),

            // 8. do (looped branching)
            Do(opts) => Self::gen_branching(in_l, opts, true, ctx, gctx),

            other => Err(BuilderError::UnsupportedStatement(format!("{:?}", other))),
        }
    }

    fn classify_guard<'a>(
        seq: &'a Sequence,
        ctx: &PgCtx,
        gctx: &GlobalCtx,
    ) -> Result<GuardClass<'a>, BuilderError> {
        use Stmnt::*;
        let first = seq
            .steps
            .first()
            .ok_or_else(|| BuilderError::UnsupportedStatement("empty option-sequence".into()))?;

        match first {
            Step::Statement(stmt_box, _) => match &**stmt_box {
                Expr(e) => Ok(GuardClass::Normal(
                    Self::translate_expr(e, ctx, gctx)?,
                    &seq.steps[1..],
                )),
                Assert(e) => Ok(GuardClass::Normal(
                    Self::translate_expr(e, ctx, gctx)?,
                    &seq.steps[1..],
                )),
                Else => Ok(GuardClass::Else(&seq.steps[1..])),
                _ => Err(BuilderError::UnsupportedStatement(
                    "guard must be bool expr / assert / else".into(),
                )),
            },
            _ => Err(BuilderError::UnsupportedStatement(
                "first step must be a Statement".into(),
            )),
        }
    }

    fn gen_branching(
        entry: Location,
        opts: &Options,
        looped: bool,
        ctx: &mut PgCtx,
        gctx: &mut GlobalCtx,
    ) -> Result<Location, BuilderError> {
        // 1) separa rami normali da 'else'
        let mut normal: Vec<(Expression<Var>, &[Step])> = Vec::new();
        let mut else_body: Option<&[Step]> = None;

        for seq in &opts.sequences {
            match Self::classify_guard(seq, ctx, gctx)? {
                GuardClass::Normal(g, body) => normal.push((g, body)),
                GuardClass::Else(body) => {
                    if else_body.is_some() {
                        return Err(BuilderError::UnsupportedStatement(
                            "multiple else branches".into(),
                        ));
                    }
                    else_body = Some(body);
                }
            }
        }

        // 2) G_else = ¬(G1 ∨ ... ∨ Gn)   (solo guardie normali!)
        let g_else = if normal.is_empty() {
            Expression::Const(Val::Boolean(true))
        } else {
            let or = Expression::Or(normal.iter().map(|(g, _)| g.clone()).collect());
            Expression::Not(Box::new(or))
        };

        let exit = fresh_loc(ctx)?;

        // 3) rami normali
        for (g, body) in &normal {
            let body_entry = fresh_loc(ctx)?;
            let choose = ctx.cs.new_action(ctx.pg)?;
            ctx.cs
                .add_transition(ctx.pg, entry, choose, body_entry, Some(g.clone()))?;

            let mut cur = body_entry;
            for st in *body {
                cur = Self::visit_step(st, cur, ctx, gctx)?;
            }

            let close = ctx.cs.new_action(ctx.pg)?;
            let target = if looped { entry } else { exit };
            ctx.cs.add_transition(ctx.pg, cur, close, target, None)?;
        }

        // 4) ramo 'else'
        if let Some(body) = else_body {
            // if: entry --(G_else)--> body ; body --> exit
            // do: entry --(G_else)--> body ; body --> exit (termina il ciclo)
            let body_entry = fresh_loc(ctx)?;
            let choose = ctx.cs.new_action(ctx.pg)?;
            ctx.cs
                .add_transition(ctx.pg, entry, choose, body_entry, Some(g_else.clone()))?;

            let mut cur = body_entry;
            for st in body {
                cur = Self::visit_step(st, cur, ctx, gctx)?;
            }

            let close = ctx.cs.new_action(ctx.pg)?;
            ctx.cs.add_transition(ctx.pg, cur, close, exit, None)?;
        } else if looped {
            // 5) do senza else: uscita silente con G_else
            let eps = ctx.cs.new_action(ctx.pg)?;
            ctx.cs
                .add_transition(ctx.pg, entry, eps, exit, Some(g_else))?;
        }

        Ok(exit)
    }

    /// Generates the graph nodes for an assignment statement.
    fn gen_assign(
        in_l: Location,
        lhs: &Varref,
        op: &AssignOp,
        rhs: Option<&AnyExpr>,
        ctx: &mut PgCtx<'_>,
        gctx: &GlobalCtx,
    ) -> Result<Location, BuilderError> {
        let var = lookup_var(lhs, ctx, gctx)?;
        let rhs_expr = match op {
            AssignOp::Eq => Self::translate_expr(rhs.expect("= needs expr"), ctx, gctx)?,
            AssignOp::Inc => Expression::Sum(vec![
                Expression::Var(var, Type::Integer),
                Expression::Const(Val::Integer(1)),
            ]),
            AssignOp::Dec => Expression::Sum(vec![
                Expression::Var(var, Type::Integer),
                Expression::Const(Val::Integer(-1)),
            ]),
        };

        let out = fresh_loc(ctx)?;
        let act = ctx
            .cs
            .new_action(ctx.pg)
            .map_err(|_| BuilderError::TypeError("action".into()))?;

        ctx.cs
            .add_effect(ctx.pg, act, var, rhs_expr)
            .map_err(|_| BuilderError::TypeError("effect".into()))?;

        ctx.cs
            .add_transition(ctx.pg, in_l, act, out, None)
            .map_err(|_| BuilderError::TypeError("transition".into()))?;

        Ok(out)
    }

    /// Generates the graph nodes for an `assert` statement.
    fn gen_assert(
        in_l: Location,
        cond: &AnyExpr,
        ctx: &mut PgCtx<'_>,
        gctx: &GlobalCtx,
    ) -> Result<Location, BuilderError> {
        let guard = Self::translate_expr(cond, ctx, gctx)?;
        let out = fresh_loc(ctx)?;
        let act = ctx
            .cs
            .new_action(ctx.pg)
            .map_err(|_| BuilderError::TypeError("action".into()))?;

        ctx.cs
            .add_transition(ctx.pg, in_l, act, out, Some(guard))
            .map_err(|_| BuilderError::TypeError("transition".into()))?;

        Ok(out)
    }

    /// Handles channel declarations with capacity and type.
    fn channel_initialization(
        ch_init: &ChInit,
        ctx: &mut PgCtx<'_>,
    ) -> Result<Channel, BuilderError> {
        // without = [...] we assume infinite capacity and
        // **still** let the user choose the type:
        //  - if in Promela they write "chan q;"       → Integer (default)
        //  - if they write "chan q = [ ] of { T }"    → T (handled above)

        // 1. capacity (None = infinite)
        let cap = match &ch_init.const_value {
            Const::Number(n) if *n >= 0 => Some(*n as usize),
            Const::Skip => None,
            _ => {
                return Err(BuilderError::UnsupportedInit(
                    "channel capacity must be non-negative integer".into(),
                ));
            }
        };

        // 2. handshake [0] not supported yet
        if cap == Some(0) {
            return Err(BuilderError::UnsupportedInit(
                "handshake channels ([0]) not supported yet".into(),
            ));
        }

        // 3. message type: only one typename allowed
        if ch_init.typename_list.len() != 1 {
            return Err(BuilderError::UnsupportedInit(
                "only single-type channels supported".into(),
            ));
        }
        let elem_type = typename_to_type(&ch_init.typename_list[0])?;

        // 4. create the channel
        Ok(ctx.cs.new_channel(elem_type, cap))
    }

    /// Generates the graph nodes for a channel send operation (`ch ! msg`).
    fn gen_send(
        in_l: Location,
        msg: &SendMsg,
        ctx: &mut PgCtx<'_>,
        gctx: &GlobalCtx,
    ) -> Result<Location, BuilderError> {
        let ch = lookup_chan(&msg.varref, ctx, gctx)?;

        let mut payload: Vec<Expression<_>> = match &msg.args {
            SendArgs::Simple(v) => v,
            SendArgs::WithExpr(_, v) => v,
        }
        .iter()
        .map(|a| Builder::translate_expr(&a.expr, ctx, gctx))
        .collect::<Result<_, _>>()?;

        // (Promela allows sending 0–N fields.
        //  SCAN accepts a single Expression: we pack them into a Tuple,
        //  or leave the single field as it is.)
        // TODO
        let value_expr = if payload.len() == 1 {
            payload.pop().unwrap()
        } else {
            Expression::Tuple(payload)
        };

        let send_act = ctx
            .cs
            .new_send(ctx.pg, ch, value_expr)
            .map_err(|_| BuilderError::TypeError("send".into()))?;

        let out = fresh_loc(ctx)?;
        ctx.cs
            .add_transition(ctx.pg, in_l, send_act, out, None)
            .map_err(|_| BuilderError::TypeError("transition".into()))?;

        Ok(out)
    }

    /// Generates the graph nodes for a channel receive operation (`ch ? var`).
    fn gen_recv(
        in_l: Location,
        recv: &Receive,
        ctx: &mut PgCtx<'_>,
        gctx: &GlobalCtx,
    ) -> Result<Location, BuilderError> {
        let ch = lookup_chan(&recv.varref, ctx, gctx)?;

        let dest_vr = match &recv.recv_args {
            RecvArgs::Simple(v) if v.len() == 1 => match &v[0] {
                RecvArg::Varref(vr) => vr,
                _ => {
                    return Err(BuilderError::UnsupportedStatement(
                        "complex recv pattern".into(),
                    ));
                }
            },
            _ => {
                return Err(BuilderError::UnsupportedStatement(
                    "multi-field receive not yet supported".into(),
                ));
            }
        };

        let dest_var = lookup_var(dest_vr, ctx, gctx)?;

        let recv_act = ctx
            .cs
            .new_receive(ctx.pg, ch, dest_var)
            .map_err(|_| BuilderError::TypeError("receive".into()))?;

        let out = fresh_loc(ctx)?;
        ctx.cs
            .add_transition(ctx.pg, in_l, recv_act, out, None)
            .map_err(|_| BuilderError::TypeError("transition".into()))?;

        Ok(out)
    }

    /// Generates the graph nodes for a channel poll expression (e.g., `empty(ch)`).
    fn gen_probe(
        in_l: Location,
        kind: &crate::spinv4_y::Chanpoll, // Full, Empty, NFull, NEmpty
        chan: &crate::spinv4_y::AnyExpr,
        ctx: &mut PgCtx<'_>,
        gctx: &GlobalCtx,
    ) -> Result<Location, BuilderError> {
        use crate::spinv4_y::{AnyExpr, Chanpoll::*};

        let vr = match chan {
            AnyExpr::Varref(v) if v.details.is_none() => v,
            _ => {
                return Err(BuilderError::UnsupportedStatement(
                    "argument of FULL/EMPTY must be simple varref".into(),
                ));
            }
        };

        let ch = lookup_chan(vr, ctx, gctx)?;

        // create the appropriate probe action
        let act = match kind {
            Full => ctx
                .cs
                .new_probe_full_queue(ctx.pg, ch)
                .map_err(|_| BuilderError::TypeError("probe full".into()))?,
            Empty => ctx
                .cs
                .new_probe_empty_queue(ctx.pg, ch)
                .map_err(|_| BuilderError::TypeError("probe empty".into()))?,
            NFull | NEmpty => {
                return Err(BuilderError::UnsupportedStatement(
                    "NFULL / NEMPTY are not supported yet".into(),
                ));
            }
        };

        let out = fresh_loc(ctx)?;
        ctx.cs
            .add_transition(ctx.pg, in_l, act, out, None)
            .map_err(|_| BuilderError::TypeError("transition".into()))?;
        Ok(out)
    }

    //=============== STUBBED FUNCTIONS ========================
    // These modules are recognized but not fully translated yet.

    fn visit_init(
        init: &Init,
        cs_builder: &mut ChannelSystemBuilder<SmallRng>,
        _gctx: &mut GlobalCtx,
    ) -> Result<(), BuilderError> {
        let pg = cs_builder.new_program_graph();

        let mut ctx = PgCtx {
            cs: cs_builder,
            pg,
            vars: HashMap::new(),
            type_vars: HashMap::new(),
            chans: HashMap::new(),
        };

        let entry = ctx
            .cs
            .new_initial_location(pg)
            .map_err(|_| BuilderError::UnsupportedInit("loc".into()))?;
        let mut cur = entry;
        for step in &init.sequence.steps {
            cur = Self::visit_step(step, cur, &mut ctx, &mut GlobalCtx::default())?;
        }
        Ok(())
    }

    fn visit_never(
        _never: &Never,
        _cs: &mut ChannelSystemBuilder<SmallRng>,
        _gctx: &mut GlobalCtx,
    ) -> Result<(), BuilderError> {
        Ok(())
    }

    fn visit_trace(
        _trace: &Trace,
        _cs: &mut ChannelSystemBuilder<SmallRng>,
        _gctx: &mut GlobalCtx,
    ) -> Result<(), BuilderError> {
        Ok(())
    }

    fn visit_utype(_ut: &Utype, _gctx: &mut GlobalCtx) -> Result<(), BuilderError> {
        Ok(())
    }
}
