use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;
use std::env;

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
    // Build the lexer and parser with proper error handling
    let lexer_builder = match CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            println!("Inizializzazione di lrpar_config...");
            let result = ctp
                .yacckind(YaccKind::Grmtools)
                .grammar_in_src_dir(r".\main\spinv4.y")
                .map_err(|e| format!("Errore nella configurazione della grammatica: {}", e));

            match result {
                Ok(_) => println!("Configurazione della grammatica riuscita"),
                Err(ref e) => println!("Errore nella configurazione della grammatica: {}", e),
            }

            result.unwrap_or_else(|_| {
                eprintln!("Errore nel unwrap della grammatica");
                std::process::exit(1);
            })
        })
        .lexer_in_src_dir(r".\main\spinv.l")
        .map_err(|e| {
            println!("Errore durante l'inizializzazione del lexer: {}", e);
            format!("Errore nell'inizializzazione del lexer: {}", e)
        }) {
        Ok(builder) => builder,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };

    match lexer_builder.build() {
        Ok(_) => println!("Build completata con successo"),
        Err(e) => {
            eprintln!("Errore durante la build del lexer e parser: {:?}", e);
            std::process::exit(1);
        }
    }
}
