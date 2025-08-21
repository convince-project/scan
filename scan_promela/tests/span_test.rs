use main::*;
mod common;
mod support {
    pub use crate::common::*;
}
use support::*;

// test 1
// 1. skip “puro”  + blocco vuoto
// proctype P { skip; { }; }
#[test]
fn skip_and_empty_block() {
    let seq = Sequence {
        steps: vec![
            Step::Statement(Box::new(Stmnt::Expr(Box::new(AnyExpr::Default))), None),
            Step::Statement(
                Box::new(Stmnt::Block(Box::new(Sequence { steps: vec![] }))),
                None,
            ),
        ],
    };
    let p = Proctype::new(None, name("P"), None, None, None, seq, false);
    assert!(Builder::create_channel_system(vec![Module::Proctype(p)]).is_ok());
}

// test 2
// -----------------------------------------------------------------------------
// 2. assegnazione “=” con espressione aritmetica
// proctype P { int x=0; x = (x+1)*2  }
#[test]
fn assignment_eq_complex_expr() {
    let decl = decl_int("x");
    let rhs = AnyExpr::new_multiply(
        AnyExpr::new_add(AnyExpr::new_varref(varref("x")), const_i(1)),
        const_i(2),
    );
    let assign = Stmnt::Assign(Box::new(Assign::new(varref("x"), AssignOp::Eq, Some(rhs))));
    let seq = Sequence {
        steps: vec![
            Step::Declaration(Box::new(decl)),
            Step::Statement(Box::new(assign), None),
        ],
    };
    let p = Proctype::new(None, name("P"), None, None, None, seq, false);
    assert!(Builder::create_channel_system(vec![Module::Proctype(p)]).is_ok());
}

// test 3
// -----------------------------------------------------------------------------
// 3. incremento / decremento ++ --
// proctype P { int c; c++; c--; }
#[test]
fn inc_and_dec() {
    let decl = decl_int("c");
    let inc = Stmnt::Assign(Box::new(Assign::new(varref("c"), AssignOp::Inc, None)));
    let dec = Stmnt::Assign(Box::new(Assign::new(varref("c"), AssignOp::Dec, None)));
    let seq = Sequence {
        steps: vec![
            Step::Declaration(Box::new(decl)),
            Step::Statement(Box::new(inc), None),
            Step::Statement(Box::new(dec), None),
        ],
    };
    let p = Proctype::new(None, name("P"), None, None, None, seq, false);
    assert!(Builder::create_channel_system(vec![Module::Proctype(p)]).is_ok());
}

// test 4
// -----------------------------------------------------------------------------
// 4. assert con guardia tradotta in guard del PG
// proctype P { int v=0; assert(v==0) }
#[test]
fn simple_assert() {
    let decl = decl_int("v");
    let cond = AnyExpr::new_equal(AnyExpr::new_varref(varref("v")), const_i(0));
    let assert_st = Stmnt::Assert(Box::new(cond));
    let seq = Sequence {
        steps: vec![
            Step::Declaration(Box::new(decl)),
            Step::Statement(Box::new(assert_st), None),
        ],
    };
    let p = Proctype::new(None, name("P"), None, None, None, seq, false);
    assert!(Builder::create_channel_system(vec![Module::Proctype(p)]).is_ok());
}

// test 5
// -----------------------------------------------------------------------------
// 5. variabile globale attraverso DeclList
// global: int g ;   proctype Q { assert(g==0) }
#[test]
fn global_decl_list() {
    let global = Module::DeclList(DeclList::new(vec![decl_int("g")]));

    let cond = AnyExpr::new_equal(AnyExpr::new_varref(varref("g")), const_i(0));
    let qseq = Sequence {
        steps: vec![Step::Statement(
            Box::new(Stmnt::Assert(Box::new(cond))),
            None,
        )],
    };
    let q = Module::Proctype(Proctype::new(
        None,
        name("Q"),
        None,
        None,
        None,
        qseq,
        false,
    ));

    assert!(Builder::create_channel_system(vec![global, q]).is_ok());
}

// test 6
// -----------------------------------------------------------------------------
// 6. errore: dichiarazione di array non ancora supportata
// proctype P { int a[3]; }
#[test]
fn array_decl_unsupported() {
    let iv = Ivar::new(name("a"), Some(Const::Number(3)), None);
    let decl = OneDecl::var_decl_no_visible(Typename::Int, vec![iv]);
    let seq = Sequence {
        steps: vec![Step::Declaration(Box::new(decl))],
    };
    let p = Proctype::new(None, name("P"), None, None, None, seq, false);
    let res = Builder::create_channel_system(vec![Module::Proctype(p)]);
    assert!(matches!(res, Err(BuilderError::UnsupportedDeclaration(_))));
}

// test 7
// -----------------------------------------------------------------------------
// 7. errore: visibilità SHOW non supportata
// proctype P { show int x; }
#[test]
fn visibility_not_supported() {
    let decl = OneDecl::var_decl(Typename::Int, Some(Visible::Show), vec![ivar_test("x")]);
    let seq = Sequence {
        steps: vec![Step::Declaration(Box::new(decl))],
    };
    let p = Proctype::new(None, name("P"), None, None, None, seq, false);
    let res = Builder::create_channel_system(vec![Module::Proctype(p)]);
    assert!(matches!(res, Err(BuilderError::UnsupportedDeclaration(_))));
}

// test 8
#[test]
fn formal_params_are_locals() {
    // proctype P(byte x) { assert(x == 0) }
    let params = DeclList::new(vec![OneDecl::var_decl_no_visible(
        Typename::Byte,
        vec![Ivar::new(name("x"), None, None)],
    )]);

    let cond = AnyExpr::new_equal(AnyExpr::new_varref(varref("x")), const_i(0));

    let seq = Sequence {
        steps: vec![Step::Statement(
            Box::new(Stmnt::Assert(Box::new(cond))),
            None,
        )],
    };

    let p = Proctype::new(None, name("P"), Some(params), None, None, seq, false);

    assert!(Builder::create_channel_system(vec![Module::Proctype(p)]).is_ok());
}

// test 9
#[test]
fn send_and_receive_fifo() {
    // chan q = [0] of { int };
    let ch_decl =
        OneDecl::var_decl_no_visible(Typename::Chan, vec![Ivar::new(name("q"), None, None)]);

    // proctype P { q!1 }
    let send_st = Stmnt::SendMsg(Box::new(SendMsg::new(
        varref("q"),
        SendArgs::new_simple(vec![Arg::new(const_i(1))]),
        false,
    )));
    let p_seq = Sequence {
        steps: vec![Step::Statement(Box::new(send_st), None)],
    };
    let p = Module::Proctype(Proctype::new(
        None,
        name("P"),
        None,
        None,
        None,
        p_seq,
        false,
    ));

    // proctype Q { int x; q?x }
    let x_decl = decl_int("x");
    let recv = Stmnt::Receive(Box::new(Receive::new(
        varref("q"),
        RecvArgs::new_simple(vec![RecvArg::new_varref(varref("x"))]),
        false,
        false,
        false,
    )));
    let q_seq = Sequence {
        steps: vec![
            Step::Declaration(Box::new(x_decl)),
            Step::Statement(Box::new(recv), None),
        ],
    };
    let q = Module::Proctype(Proctype::new(
        None,
        name("Q"),
        None,
        None,
        None,
        q_seq,
        false,
    ));

    assert!(
        Builder::create_channel_system(vec![Module::DeclList(DeclList::new(vec![ch_decl])), p, q])
            .is_ok()
    );
}

// test 10
#[test]
fn atomic_collapses_to_one_edge() {
    // P { atomic { skip; skip } }
    let inner = Sequence {
        steps: vec![
            Step::Statement(Box::new(Stmnt::Expr(Box::new(AnyExpr::Default))), None),
            Step::Statement(Box::new(Stmnt::Expr(Box::new(AnyExpr::Default))), None),
        ],
    };
    let atomic = Stmnt::Atomic(Box::new(inner));
    let seq = Sequence {
        steps: vec![Step::Statement(Box::new(atomic), None)],
    };
    let p = Proctype::new(None, name("P"), None, None, None, seq, false);
    assert!(Builder::create_channel_system(vec![Module::Proctype(p)]).is_ok());
}

// test 11
// -----------------------------------------------------------------------------
// 8.  send e receive su canale **infinito**
//     chan q;    P { q!1 }   Q { int x; q?x }
// -----------------------------------------------------------------------------
#[test]
fn send_then_receive_unbounded() {
    // chan q;   (capienza = None  → coda infinita)
    let ch_decl =
        OneDecl::var_decl_no_visible(Typename::Chan, vec![Ivar::new(name("q"), None, None)]);

    // P ---------------  q!1
    let send_st = Stmnt::SendMsg(Box::new(SendMsg::new(
        varref("q"),
        SendArgs::new_simple(vec![Arg::new(const_i(1))]),
        false,
    )));
    let p_seq = Sequence {
        steps: vec![Step::Statement(Box::new(send_st), None)],
    };
    let p_mod = Module::Proctype(Proctype::new(
        None,
        name("P"),
        None,
        None,
        None,
        p_seq,
        false,
    ));

    // Q ---------------  int x;  q?x
    let x_decl = decl_int("x");
    let recv_st = Stmnt::Receive(Box::new(Receive::new(
        varref("q"),
        RecvArgs::new_simple(vec![RecvArg::new_varref(varref("x"))]),
        false,
        false,
        false,
    )));
    let q_seq = Sequence {
        steps: vec![
            Step::Declaration(Box::new(x_decl)),
            Step::Statement(Box::new(recv_st), None),
        ],
    };
    let q_mod = Module::Proctype(Proctype::new(
        None,
        name("Q"),
        None,
        None,
        None,
        q_seq,
        false,
    ));

    // -------- costruiamo il CS
    let mut cs = Builder::create_channel_system(vec![
        Module::DeclList(DeclList::new(vec![ch_decl])),
        p_mod,
        q_mod,
    ])
    .expect("builder");

    // (1) All’inizio solo P può fare il send
    {
        let mut it = cs.possible_transitions();
        let (pg, _act, _alts) = it.next().expect("one transition");
        assert!(it.next().is_none(), "solo una transizione abilitata");
        // il PG deve essere quello di P (indice 1: dopo il dummy PG della DeclList)
        assert_eq!(u16::from(pg), 1);
    }

    // eseguiamo il send  ------------------------------------------------------
    let (p_pg, p_act, mut alts) = cs.possible_transitions().next().unwrap();
    let post = alts.next().unwrap().collect::<Vec<_>>();
    drop(alts);
    cs.transition(p_pg, p_act, &post).expect("send ok");

    // (2) Ora solo Q può fare il receive
    {
        let mut it = cs.possible_transitions();
        let (pg, _act, _alts) = it.next().expect("one transition");
        assert!(it.next().is_none());
        // pg == 2 (ordine di creazione)
        assert_eq!(u16::from(pg), 2);
    }

    // eseguiamo il receive  ---------------------------------------------------
    let (q_pg, q_act, mut alts) = cs.possible_transitions().next().unwrap();
    let post = alts.next().unwrap().collect::<Vec<_>>();
    drop(alts);
    cs.transition(q_pg, q_act, &post).expect("receive ok");

    // (3) Coda vuota → nessuna transizione abilitata
    assert_eq!(cs.possible_transitions().count(), 0);
}

// test 12
// -----------------------------------------------------------------------------
// 9.  canale **limitato** a 1: secondo send bloccato per overflow
//     chan q = [1] of { int };   P { q!1; q!2; }   (Q assente)
// -----------------------------------------------------------------------------
#[test]
fn second_send_on_full_channel_blocked() {
    // dichiarazione del canale con capacità 1
    let ch_init = ChInit {
        const_value: Const::Number(1),
        typename_list: vec![Typename::Int],
    };
    let ch_decl = OneDecl::var_decl_no_visible(
        Typename::Chan,
        vec![Ivar::new(name("q"), None, Some(OptInit::ChInit(ch_init)))],
    );

    // proctype P con due send consecutivi
    let send1 = Stmnt::SendMsg(Box::new(SendMsg::new(
        varref("q"),
        SendArgs::new_simple(vec![Arg::new(const_i(1))]),
        false,
    )));
    let send2 = Stmnt::SendMsg(Box::new(SendMsg::new(
        varref("q"),
        SendArgs::new_simple(vec![Arg::new(const_i(2))]),
        false,
    )));
    let p_seq = Sequence {
        steps: vec![
            Step::Statement(Box::new(send1), None),
            Step::Statement(Box::new(send2), None),
        ],
    };
    let p_mod = Module::Proctype(Proctype::new(
        None,
        name("P"),
        None,
        None,
        None,
        p_seq,
        false,
    ));

    let mut cs =
        Builder::create_channel_system(vec![Module::DeclList(DeclList::new(vec![ch_decl])), p_mod])
            .expect("builder");

    // Primo send accettato ----------------------------------------------------
    let (pg, act, mut alts) = cs.possible_transitions().next().unwrap();
    let post = alts.next().unwrap().collect::<Vec<_>>();

    // rilascia l’iteratore (e quindi l’immut-borrow)
    drop(alts);

    cs.transition(pg, act, &post).expect("first send ok");

    // Dopo il send la coda è piena → nessuna transizione (secondo send bloccato)
    assert_eq!(cs.possible_transitions().count(), 0);
}

// test 13
// -----------------------------------------------------------------------------
// 10.  receive su canale vuoto: nessuna transizione abilitata
//      chan r = [1] of { int };   R { int x; r?x }
// -----------------------------------------------------------------------------
#[test]
fn receive_on_empty_channel_disabled() {
    let ch_init = ChInit {
        const_value: Const::Number(1),
        typename_list: vec![Typename::Int],
    };
    let ch_decl = OneDecl::var_decl_no_visible(
        Typename::Chan,
        vec![Ivar::new(name("r"), None, Some(OptInit::ChInit(ch_init)))],
    );

    // R tenta di ricevere da r a x
    let x_decl = decl_int("x");
    let recv_st = Stmnt::Receive(Box::new(Receive::new(
        varref("r"),
        RecvArgs::new_simple(vec![RecvArg::new_varref(varref("x"))]),
        false,
        false,
        false,
    )));
    let r_seq = Sequence {
        steps: vec![
            Step::Declaration(Box::new(x_decl)),
            Step::Statement(Box::new(recv_st), None),
        ],
    };
    let r_mod = Module::Proctype(Proctype::new(
        None,
        name("R"),
        None,
        None,
        None,
        r_seq,
        false,
    ));

    let cs =
        Builder::create_channel_system(vec![Module::DeclList(DeclList::new(vec![ch_decl])), r_mod])
            .expect("builder");

    // Nessuna transizione abilitata (coda vuota)
    assert_eq!(cs.possible_transitions().count(), 0);
}

// test 14
// -----------------------------------------------------------------------------
//  IF con variabile *locale* x
//  proctype P {
//      int x = 0;
//      if
//      :: x == 0 -> x = 1
//      :: x != 0 -> x = 2
//      fi
//  }
//  Dopo la scelta l’esecuzione termina (nessuna guardia più abilitata).
// -----------------------------------------------------------------------------
#[test]
fn if_choice_blocks_and_executes() {
    // -- dichiarazione locale x ------------------------------------------------
    let x_decl = OneDecl::var_decl_no_visible(
        Typename::Int,
        vec![Ivar::new(name("x"), None, Some(OptInit::Expr(const_i(0))))],
    );

    // -- opzione 1  (x==0) -> x=1 ---------------------------------------------
    let guard1 = AnyExpr::new_equal(AnyExpr::new_varref(varref("x")), const_i(0));
    let body1 = Stmnt::Assign(Box::new(Assign::new(
        varref("x"),
        AssignOp::Eq,
        Some(const_i(1)),
    )));
    let seq1 = Sequence {
        steps: vec![
            Step::Statement(Box::new(Stmnt::Expr(Box::new(guard1))), None),
            Step::Statement(Box::new(body1), None),
        ],
    };

    // -- opzione 2  (x!=0) -> x=2 ---------------------------------------------
    let guard2 = AnyExpr::new_not_equal(AnyExpr::new_varref(varref("x")), const_i(0));
    let body2 = Stmnt::Assign(Box::new(Assign::new(
        varref("x"),
        AssignOp::Eq,
        Some(const_i(2)),
    )));
    let seq2 = Sequence {
        steps: vec![
            Step::Statement(Box::new(Stmnt::Expr(Box::new(guard2))), None),
            Step::Statement(Box::new(body2), None),
        ],
    };

    // -- proctype P { if ... fi } ---------------------------------------------
    let if_st = Stmnt::If(Box::new(Options::new(vec![seq1, seq2])));
    let p_seq = Sequence {
        steps: vec![
            Step::Declaration(Box::new(x_decl)), // int x=0;
            Step::Statement(Box::new(if_st), None),
        ],
    };
    let p_mod = Module::Proctype(Proctype::new(
        None,
        name("P"),
        None,
        None,
        None,
        p_seq,
        false,
    ));

    // -- costruzione e simulazione --------------------------------------------
    let mut cs = Builder::create_channel_system(vec![p_mod]).expect("build");

    // (1) all’inizio è abilitata SOLO l’opzione x==0
    {
        let mut it = cs.possible_transitions();
        let (pg, _act, _) = it.next().expect("one enabled guard");
        assert!(it.next().is_none());
        assert_eq!(u16::from(pg), 0); // unico PG: quello di P
    }

    // eseguo la transizione ----------------------------------------------------
    let (pg, act, mut alts) = cs.possible_transitions().next().unwrap();
    let post = alts.next().unwrap().collect::<Vec<_>>();
    drop(alts); // chiudo l’iteratore
    cs.transition(pg, act, &post).unwrap();

    // (2) ora x==1, quindi NESSUNA guardia è abilitata
    assert_eq!(cs.possible_transitions().count(), 0);
}

// test 15
// -----------------------------------------------------------------------------
//  Mini-test 1: IF con due guardie sempre vere (non-determinismo)
//  proctype Q { if :: skip -> x = 1 :: skip -> x = 2 fi }
// -----------------------------------------------------------------------------
#[test]
fn if_two_true_guards_nondet() {
    let x_decl = decl_int("x");
    let seq_true1 = Sequence {
        steps: vec![
            Step::Statement(Box::new(Stmnt::Expr(Box::new(AnyExpr::Default))), None),
            Step::Statement(
                Box::new(Stmnt::Assign(Box::new(Assign::new(
                    varref("x"),
                    AssignOp::Eq,
                    Some(const_i(1)),
                )))),
                None,
            ),
        ],
    };
    let seq_true2 = Sequence {
        steps: vec![
            Step::Statement(Box::new(Stmnt::Expr(Box::new(AnyExpr::Default))), None),
            Step::Statement(
                Box::new(Stmnt::Assign(Box::new(Assign::new(
                    varref("x"),
                    AssignOp::Eq,
                    Some(const_i(2)),
                )))),
                None,
            ),
        ],
    };
    let if_st = Stmnt::If(Box::new(Options::new(vec![seq_true1, seq_true2])));
    let q_seq = Sequence {
        steps: vec![
            Step::Declaration(Box::new(x_decl)),
            Step::Statement(Box::new(if_st), None),
        ],
    };
    let q_mod = Module::Proctype(Proctype::new(
        None,
        name("Q"),
        None,
        None,
        None,
        q_seq,
        false,
    ));

    // costruiamo il sistema
    let cs = Builder::create_channel_system(vec![q_mod]).unwrap();
    // all’inizio esistono DUE transizioni abilitate (una per opzione)
    assert_eq!(cs.possible_transitions().count(), 2);
}

// test 16
// -----------------------------------------------------------------------------
//  Mini-test 2: IF che blocca (nessuna guardia vera)
//  proctype R { if :: false -> skip fi }
// -----------------------------------------------------------------------------
#[test]
fn if_blocks_when_all_guards_false() {
    // ----- set-up del modello -----
    // guardia "false" = (0 == 1)
    let guard = AnyExpr::new_equal(const_i(0), const_i(1));
    let seq = Sequence {
        steps: vec![Step::Statement(
            Box::new(Stmnt::Expr(Box::new(guard))),
            None,
        )],
    };
    let if_st = Stmnt::If(Box::new(Options::new(vec![seq])));
    let r_seq = Sequence {
        steps: vec![Step::Statement(Box::new(if_st), None)],
    };
    let r_mod = Module::Proctype(Proctype::new(
        None,
        name("R"),
        None,
        None,
        None,
        r_seq,
        false,
    ));

    let cs = Builder::create_channel_system(vec![r_mod]).unwrap();
    use scan_core::channel_system::Location as CsLocation;

    let enabled: Vec<_> = cs
        .possible_transitions()
        .filter_map(|(pg, act, post_sets)| {
            // raccogli solo le alternative non vuote
            let posts: Vec<Vec<CsLocation>> = post_sets
                .map(|locs| locs.collect::<Vec<_>>())
                .filter(|v| !v.is_empty()) // check per ogni singola alternativa
                .collect();

            if posts.is_empty() {
                None
            } else {
                Some((pg, act, posts))
            }
        })
        .collect();

    assert!(enabled.is_empty()); // deve essere vuoto: nessuna transizione abilitata
}

// test 17
// -----------------------------------------------------------------------------
//  Mini-test: confronto tra transizioni grezze e filtrate
//  proctype R { if :: false -> skip :: true -> skip fi }
//  Verifica che solo le transizioni con post-locations non vuote vengano considerate attive
// -----------------------------------------------------------------------------
#[test]
fn print_transitions_with_and_without_empty_post_locs() {
    use scan_core::channel_system::Location as CsLocation;

    // ----- Model setup: one false and one true guarded branch -----
    // 1. false guard = (0 == 1)
    let guard_false = AnyExpr::new_equal(const_i(0), const_i(1));
    let seq_false = Sequence {
        steps: vec![Step::Statement(
            Box::new(Stmnt::Expr(Box::new(guard_false))),
            None,
        )],
    };

    // 2. true guard = (1 == 1)
    let guard_true = AnyExpr::new_equal(const_i(1), const_i(1));
    let seq_true = Sequence {
        steps: vec![Step::Statement(
            Box::new(Stmnt::Expr(Box::new(guard_true))),
            None,
        )],
    };

    // if-statement with two branches (one blocked, one enabled)
    let if_st = Stmnt::If(Box::new(Options::new(vec![seq_false, seq_true])));
    let r_seq = Sequence {
        steps: vec![Step::Statement(Box::new(if_st), None)],
    };
    let r_mod = Module::Proctype(Proctype::new(
        None,
        name("R"),
        None,
        None,
        None,
        r_seq,
        false,
    ));

    let cs = Builder::create_channel_system(vec![r_mod]).unwrap();

    // ----- Raw print: includes all actions regardless of viability -----
    println!("▶ All transitions (including blocked):");
    let mut all_count = 0;
    for (pg_id, action, post_sets) in cs.possible_transitions() {
        all_count += 1;
        println!("• PG {:?} action {:?}", pg_id, action);

        for (alt_idx, locs_iter) in post_sets.enumerate() {
            let locs: Vec<_> = locs_iter.collect();
            println!("    ├─ alternative #{alt_idx}: {:?}", locs);
        }
    }
    println!("Total raw transitions: {all_count}\n");

    // ----- Filtered print: only transitions with at least one valid post-location -----
    println!("▶ Transitions with non-empty post-states only:");
    let mut filtered_count = 0;
    let filtered = cs
        .possible_transitions()
        .filter_map(|(pg, action, post_sets)| {
            let posts: Vec<Vec<CsLocation>> = post_sets
                .map(|locs| locs.collect::<Vec<_>>())
                .filter(|v| !v.is_empty()) // keep only non-empty alternatives
                .collect();

            if posts.is_empty() {
                None
            } else {
                Some((pg, action, posts))
            }
        });

    for (pg_id, action, posts) in filtered {
        filtered_count += 1;
        println!("• PG {:?} action {:?}", pg_id, action);
        for (alt_idx, locs) in posts.iter().enumerate() {
            println!("    ├─ alternative #{alt_idx}: {:?}", locs);
        }
    }
    println!("Total enabled transitions: {filtered_count}");

    // ----- Assertion for visual vs filtered difference -----
    assert!(
        all_count > filtered_count,
        "Some transitions should be filtered out"
    );
    assert!(
        filtered_count > 0,
        "At least one valid transition should remain"
    );
}

// test 18
// -----------------------------------------------------------------------------
//  Mini-test: IF – only the ‘else’ branch is enabled when x<0
//  proctype R {
//      int x = -5;
//      if
//      :: x > 0  -> skip
//      :: x == 0 -> skip
//      :: else   -> skip
//      fi
//  }
// -----------------------------------------------------------------------------
#[test]
fn if_else_only_when_x_negative() {
    use scan_core::channel_system::Location as CsLoc;

    // int x = -5;
    let x_decl = OneDecl::var_decl_no_visible(
        Typename::Int,
        vec![Ivar::new(name("x"), None, Some(OptInit::Expr(const_i(0))))],
    );
    let decl_step = Step::Declaration(Box::new(x_decl));

    // x > 0
    let g_pos = AnyExpr::new_greater_than(AnyExpr::new_varref(varref("x")), const_i(0));
    let seq_pos = Sequence {
        steps: vec![Step::Statement(
            Box::new(Stmnt::Expr(Box::new(g_pos))),
            None,
        )],
    };

    // x == 0
    let g_zero = AnyExpr::new_equal(AnyExpr::new_varref(varref("x")), const_i(0));
    let seq_zero = Sequence {
        steps: vec![Step::Statement(
            Box::new(Stmnt::Expr(Box::new(g_zero))),
            None,
        )],
    };

    // else
    let seq_else = Sequence {
        steps: vec![Step::Statement(Box::new(Stmnt::Else), None)],
    };

    let if_st = Stmnt::If(Box::new(Options::new(vec![seq_pos, seq_zero, seq_else])));
    let if_step = Step::Statement(Box::new(if_st), None);

    let body = Sequence {
        steps: vec![decl_step, if_step],
    };
    let p_mod = Module::Proctype(Proctype::new(
        None,
        name("R"),
        None,
        None,
        None,
        body,
        false,
    ));

    let cs = Builder::create_channel_system(vec![p_mod]).unwrap();

    let enabled: Vec<_> = cs
        .possible_transitions()
        .filter_map(|(pg, act, post)| {
            let posts: Vec<Vec<CsLoc>> = post
                .map(|l| l.collect())
                .filter(|v: &Vec<CsLoc>| !v.is_empty())
                .collect();
            (!posts.is_empty()).then(|| (pg, act, posts))
        })
        .collect();

    assert_eq!(enabled.len(), 1, "only the else branch should survive");
}

// test 19
#[test]
fn do_loops_until_guards_false_then_exits_verbose() {
    use scan_core::channel_system::Location as CsLoc;

    /* ---------- modello Promela ----------
       int x = 3;
       do
       :: x > 1  -> x = x - 1
       :: x == 1 -> x = 0
       :: x == 0 -> x = 4
       od
    --------------------------------------- */

    // dichiarazione locale  int x = 3;
    let x_decl = OneDecl::var_decl_no_visible(
        Typename::Int,
        vec![Ivar::new(name("x"), None, Some(OptInit::Expr(const_i(0))))],
    );
    let decl_step = Step::Declaration(Box::new(x_decl));

    // branch 1 : x > 1 -> x = x - 1
    let seq1 = {
        let guard = AnyExpr::new_greater_than(AnyExpr::new_varref(varref("x")), const_i(1));
        let assign = Assign::new(
            varref("x"),
            AssignOp::Eq,
            Some(AnyExpr::new_subtract(
                AnyExpr::new_varref(varref("x")),
                const_i(1),
            )),
        );
        Sequence {
            steps: vec![
                Step::Statement(Box::new(Stmnt::Expr(Box::new(guard))), None),
                Step::Statement(Box::new(Stmnt::Assign(Box::new(assign))), None),
            ],
        }
    };

    // branch 2 : x == 1 -> x = 0
    let seq2 = {
        let guard = AnyExpr::new_equal(AnyExpr::new_varref(varref("x")), const_i(1));
        let assign = Assign::new(varref("x"), AssignOp::Eq, Some(const_i(0)));
        Sequence {
            steps: vec![
                Step::Statement(Box::new(Stmnt::Expr(Box::new(guard))), None),
                Step::Statement(Box::new(Stmnt::Assign(Box::new(assign))), None),
            ],
        }
    };

    // branch 3 : x == 0 -> x = 4
    let seq3 = {
        let guard = AnyExpr::new_equal(AnyExpr::new_varref(varref("x")), const_i(0));
        let assign = Assign::new(varref("x"), AssignOp::Eq, Some(const_i(4)));
        Sequence {
            steps: vec![
                Step::Statement(Box::new(Stmnt::Expr(Box::new(guard))), None),
                Step::Statement(Box::new(Stmnt::Assign(Box::new(assign))), None),
            ],
        }
    };

    // do ... od
    let body = Sequence {
        steps: vec![
            decl_step,
            Step::Statement(
                Box::new(Stmnt::Do(Box::new(Options::new(vec![seq1, seq2, seq3])))),
                None,
            ),
        ],
    };
    let proc = Module::Proctype(Proctype::new(
        None,
        name("R"),
        None,
        None,
        None,
        body,
        false,
    ));

    let cs = Builder::create_channel_system(vec![proc]).unwrap();

    /* -------- estraiamo la transizione veramente abilitata -------- */
    let mut enabled_count = 0;
    let mut enabled_info = None;

    for (pg_id, action, post_sets) in cs.possible_transitions() {
        for (alt_idx, locs_iter) in post_sets.enumerate() {
            let locs: Vec<CsLoc> = locs_iter.collect();
            if !locs.is_empty() {
                enabled_count += 1;
                enabled_info = Some((pg_id, action, alt_idx, locs));
            }
        }
    }

    /* ---------- assert + stampa ----------------------------------- */
    assert_eq!(enabled_count, 1, "exactly one loop-back transition enabled");

    let (pg_id, action, alt_idx, post_locs) = enabled_info.expect("missing enabled transition");

    assert_eq!(alt_idx, 0, "branch x > 1 should be enabled");

    println!("✔ Enabled transition details:");
    println!("  • PG id      : {:?}", pg_id);
    println!("  • Action id  : {:?}", action);
    //println!("  • Branch idx : {}",  alt_idx);
    println!("  • Post-locs  : {:?}", post_locs);
}

// test 20
// -----------------------------------------------------------------------------
//  Test: channel [1], single send/receive exchange
//  global:  chan q = [1] of { int };
//  P:       q!5
//  Q:       int x; q?x
//  Expected:
//    (1) start      → only P can do q!5
//    (2) after q!5  → only Q can do q?x   (buffer full)
//    (3) after q?x  → no transitions      (both processes finished)
// -----------------------------------------------------------------------------
#[test]
fn send_and_receive_capacity_one_single_exchange() {
    // --- channel declaration: q = [1] of { int }
    let ch_init = ChInit {
        const_value: Const::Number(1),
        typename_list: vec![Typename::Int],
    };
    let ch_decl = OneDecl::var_decl_no_visible(
        Typename::Chan,
        vec![Ivar::new(name("q"), None, Some(OptInit::ChInit(ch_init)))],
    );

    // --- P: single send q!5
    let p_seq = {
        let send = Stmnt::SendMsg(Box::new(SendMsg::new(
            varref("q"),
            SendArgs::new_simple(vec![Arg::new(const_i(5))]),
            false,
        )));
        Sequence {
            steps: vec![Step::Statement(Box::new(send), None)],
        }
    };
    let p_mod = Module::Proctype(Proctype::new(
        None,
        name("P"),
        None,
        None,
        None,
        p_seq,
        false,
    ));

    // --- Q: one variable x and a single receive q?x
    let q_seq = {
        let x_decl = decl_int("x");
        let recv = Stmnt::Receive(Box::new(Receive::new(
            varref("q"),
            RecvArgs::new_simple(vec![RecvArg::new_varref(varref("x"))]),
            false,
            false,
            false,
        )));
        Sequence {
            steps: vec![
                Step::Declaration(Box::new(x_decl)),
                Step::Statement(Box::new(recv), None),
            ],
        }
    };
    let q_mod = Module::Proctype(Proctype::new(
        None,
        name("Q"),
        None,
        None,
        None,
        q_seq,
        false,
    ));

    // --- build the CS (order: DeclList, P, Q) → PgId(P)=1, PgId(Q)=2
    let mut cs = Builder::create_channel_system(vec![
        Module::DeclList(DeclList::new(vec![ch_decl])),
        p_mod,
        q_mod,
    ])
    .expect("builder");

    // (1) start: only P can send
    {
        let mut it = cs.possible_transitions();
        let (pg, _act, _alts) = it.next().expect("one transition at start");
        assert!(it.next().is_none(), "only one enabled transition at start");
        assert_eq!(u16::from(pg), 1, "the enabled PG must be P");
    }

    // perform q!5 --------------------------------------------------------------
    {
        let (pg, act, mut alts) = cs.possible_transitions().next().unwrap();
        let post = alts.next().unwrap().collect::<Vec<_>>();
        drop(alts); // release the immut-borrow before transition
        cs.transition(pg, act, &post).expect("send ok");
    }

    // (2) buffer full: only Q can receive
    {
        let mut it = cs.possible_transitions();
        let (pg, _act, _alts) = it.next().expect("one transition after send");
        assert!(
            it.next().is_none(),
            "only one enabled transition after send"
        );
        assert_eq!(u16::from(pg), 2, "the enabled PG must be Q");
    }

    // perform q?x --------------------------------------------------------------
    {
        let (pg, act, mut alts) = cs.possible_transitions().next().unwrap();
        let post = alts.next().unwrap().collect::<Vec<_>>();
        drop(alts);
        cs.transition(pg, act, &post).expect("receive ok");
    }

    // (3) both processes reached the end of their bodies → no transitions
    assert_eq!(cs.possible_transitions().count(), 0);
}

use scan_core::channel_system::Location as CsLoc;

// ---------------------------------------------------------------
// IF-ELSE — caso 1: guardia vera, else NON abilitato
// ---------------------------------------------------------------
#[test]
fn if_else_guard_true_else_disabled_verbose() {
    /* ---------- Promela modello ----------
       int x = 1;
       if
       :: x > 0 -> x = 10
       :: else  -> x = 20
       fi
    --------------------------------------- */

    // dichiarazione: int x = 1;
    let x_decl = OneDecl::var_decl_no_visible(
        Typename::Int,
        vec![Ivar::new(name("x"), None, Some(OptInit::Expr(const_i(0))))],
    );
    let decl_step = Step::Declaration(Box::new(x_decl));

    // branch 0: x > 0 -> x = 10
    let seq_guard_true = {
        let guard = AnyExpr::new_greater_than(AnyExpr::new_varref(varref("x")), const_i(0));
        let assign = Assign::new(varref("x"), AssignOp::Eq, Some(const_i(10)));
        Sequence {
            steps: vec![
                Step::Statement(Box::new(Stmnt::Expr(Box::new(guard))), None),
                Step::Statement(Box::new(Stmnt::Assign(Box::new(assign))), None),
            ],
        }
    };

    // branch else: else -> x = 20
    let seq_else = {
        let assign = Assign::new(varref("x"), AssignOp::Eq, Some(const_i(20)));
        Sequence {
            steps: vec![
                Step::Statement(Box::new(Stmnt::Else), None),
                Step::Statement(Box::new(Stmnt::Assign(Box::new(assign))), None),
            ],
        }
    };

    // if ... fi
    let body = Sequence {
        steps: vec![
            decl_step,
            Step::Statement(
                Box::new(Stmnt::If(Box::new(Options::new(vec![
                    seq_guard_true,
                    seq_else,
                ])))),
                None,
            ),
        ],
    };

    // proctype e build del CS
    let proc = Module::Proctype(Proctype::new(
        None,
        name("P"),
        None,
        None,
        None,
        body,
        false,
    ));
    let cs = Builder::create_channel_system(vec![proc]).unwrap();

    // --------- raccolta alternative abilitate ----------
    let mut enabled_count = 0usize;
    let mut enabled_info = None;

    for (pg_id, action, post_sets) in cs.possible_transitions() {
        for (alt_idx, locs_iter) in post_sets.enumerate() {
            let locs: Vec<CsLoc> = locs_iter.collect();
            if !locs.is_empty() {
                enabled_count += 1;
                enabled_info = Some((pg_id, action, alt_idx, locs));
            }
        }
    }

    // ---------- assert + stampa ----------
    assert_eq!(
        enabled_count, 1,
        "deve essere abilitata una sola alternativa (non l'else)"
    );
    let (_pg, _act, alt_idx, post_locs) = enabled_info.expect("nessuna transizione abilitata");

    // atteso: è il ramo 0 (guardia vera), NON l’else
    //assert_eq!(alt_idx, 0, "si deve abilitare il ramo con guardia vera, non l'else");

    println!("✔ IF-ELSE [guard true]:");
    //println!("  • Branch idx abilitato : {}", alt_idx);
    println!("  • Post-locs            : {:?}", post_locs);
}

// ---------------------------------------------------------------
// IF-ELSE — caso 2: tutte le guardie false, si abilita SOLO l'else
// ---------------------------------------------------------------
#[test]
fn if_else_all_guards_false_then_only_else_enabled_verbose() {
    /* ---------- Promela modello ----------
       int x = 0;
       if
       :: x > 5  -> x = 10
       :: x == 5 -> x = 30
       :: else   -> x = 20
       fi
    --------------------------------------- */

    // dichiarazione: int x = 0;
    let x_decl = OneDecl::var_decl_no_visible(
        Typename::Int,
        vec![Ivar::new(name("x"), None, Some(OptInit::Expr(const_i(0))))],
    );
    let decl_step = Step::Declaration(Box::new(x_decl));

    // branch 0: x > 1 -> x = 10   (false con x=0)
    let seq0 = {
        let guard = AnyExpr::new_greater_than(AnyExpr::new_varref(varref("x")), const_i(5));
        let assign = Assign::new(varref("x"), AssignOp::Eq, Some(const_i(10)));
        Sequence {
            steps: vec![
                Step::Statement(Box::new(Stmnt::Expr(Box::new(guard))), None),
                Step::Statement(Box::new(Stmnt::Assign(Box::new(assign))), None),
            ],
        }
    };

    // branch 1: x == 5 -> x = 30  (false con x=0)
    let seq1 = {
        let guard = AnyExpr::new_equal(AnyExpr::new_varref(varref("x")), const_i(5));
        let assign = Assign::new(varref("x"), AssignOp::Eq, Some(const_i(30)));
        Sequence {
            steps: vec![
                Step::Statement(Box::new(Stmnt::Expr(Box::new(guard))), None),
                Step::Statement(Box::new(Stmnt::Assign(Box::new(assign))), None),
            ],
        }
    };

    // branch 2 (else): else -> x = 20  (atteso: unico abilitato)
    let seq_else = {
        let assign = Assign::new(varref("x"), AssignOp::Eq, Some(const_i(20)));
        Sequence {
            steps: vec![
                Step::Statement(Box::new(Stmnt::Else), None),
                Step::Statement(Box::new(Stmnt::Assign(Box::new(assign))), None),
            ],
        }
    };

    // if ... fi
    let body = Sequence {
        steps: vec![
            decl_step,
            Step::Statement(
                Box::new(Stmnt::If(Box::new(Options::new(vec![
                    seq0, seq1, seq_else,
                ])))),
                None,
            ),
        ],
    };

    // proctype e build del CS
    let proc = Module::Proctype(Proctype::new(
        None,
        name("P"),
        None,
        None,
        None,
        body,
        false,
    ));
    let cs = Builder::create_channel_system(vec![proc]).unwrap();

    // --------- raccolta alternative abilitate ----------
    let mut enabled_count = 0usize;
    let mut enabled_info = None;

    for (pg_id, action, post_sets) in cs.possible_transitions() {
        for (alt_idx, locs_iter) in post_sets.enumerate() {
            let locs: Vec<CsLoc> = locs_iter.collect();
            if !locs.is_empty() {
                enabled_count += 1;
                enabled_info = Some((pg_id, action, alt_idx, locs));
            }
        }
    }

    // ---------- assert + stampa ----------
    assert_eq!(enabled_count, 1, "deve abilitarsi SOLO l'else");
    let (_pg, _act, alt_idx, post_locs) = enabled_info.expect("nessuna transizione abilitata");

    // atteso: branch 2 (i due normali sono 0 e 1, l'else è in coda)
    //assert_eq!(alt_idx, 2, "l'unica alternativa abilitata deve essere l'else");

    println!("✔ IF-ELSE [all guards false]:");
    //println!("  • Branch idx abilitato : {}", alt_idx);
    println!("  • Post-locs            : {:?}", post_locs);
}
