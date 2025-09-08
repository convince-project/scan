use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;
use std::path::PathBuf;

fn main() {
    //     env::set_var("RUST_BACKTRACE", "0");
    /*
        // Build the lexer and parser with proper error handling
        let lexer_builder = match CTLexerBuilder::new()
            .lrpar_config(|ctp| {
                ctp.yacckind(YaccKind::Grmtools)
                    .grammar_in_src_dir("spinv4.y")
                    .map_err(|e| format!("Errore nella configurazione della grammatica: {}", e))
                    .unwrap()
            })
            .lexer_in_src_dir("spinv.l")
            .map_err(|e| format!("Errore nell'inizializzazione del lexer: {}", e))
        {
            Ok(builder) => builder,
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        };
    */

    let grammar_path = PathBuf::from("spinv4.y");
    let lexer_path   = PathBuf::from("spinv.l");

    // Build the lexer and parser with proper error handling
    let builder = CTLexerBuilder::new()
        .lrpar_config(move |ctp| {
            ctp.yacckind(YaccKind::Grmtools)
              .grammar_in_src_dir(&grammar_path)
              .unwrap_or_else(|e| {
                  eprintln!("Errore nella configurazione della grammatica: {e}");
                  std::process::exit(1);
              })
        })
        .lexer_in_src_dir(&lexer_path)
        .unwrap_or_else(|e| {
            eprintln!("Errore nell'inizializzazione del lexer: {e}");
            std::process::exit(1);
        });

    builder.build().unwrap_or_else(|e| {
        eprintln!("Errore durante la build del lexer e parser: {e:?}");
        std::process::exit(1);
    });

}