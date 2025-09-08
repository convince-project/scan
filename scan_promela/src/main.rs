use crate::spinv4_y::Module;
#[warn(unused_imports)]
use lrlex::lrlex_mod;
use lrpar::Lexeme;
use lrpar::Lexer;
use lrpar::NonStreamingLexer;
use lrpar::lrpar_mod;
use regex::Regex;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::Path;

lrlex_mod!("spinv.l");
lrpar_mod!("spinv4.y");

pub mod builder;
use crate::builder::Builder;

fn main() {
    // Retrive the path from the command-line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file_or_directory_path>", args[0]);
        std::process::exit(1);
    }

    // Enable this when you need extra diagnostics
    let debug_mode = false;

    // First positional argument is the target path
    let path_arg = &args[1];
    let path = Path::new(path_arg);

    // Dispatch on whether it’s a file or a directory
    if path.is_file() {
        process_file(path, debug_mode);
    } else if path.is_dir() {
        process_directory(path, debug_mode);
    } else {
        eprintln!("Error: {} is not a valid file or directory", path.display());
        std::process::exit(1);
    }
}

fn process_file(path: &Path, debug_mode: bool) {
    /* ---------- 1. extension check ----------------------------------------- */
    match path.extension().and_then(|e| e.to_str()) {
        Some("pml" | "prm") => {} // ok
        Some(ext) => {
            eprintln!("Error: “.{ext}” is not a valid Promela extension.");
            std::process::exit(1);
        }
        None => {
            eprintln!("Error: file has no extension (expected .pml / .prm).");
            std::process::exit(1);
        }
    }

    /* ---------- 2. read file ------------------------------------------------ */
    let raw = match read_file(path) {
        Some(txt) => txt,
        None => return, // read_file already emitted an error message
    };

    /* ---------- 3. preprocessing ------------------------------------------- */
    let no_comments = remove_comments(&raw); // strip /* … */ and //
    let expanded = replace_macros_in_content(&no_comments); // expand #define …

    // TODO add delete printf in the files during the preprocessing step

    /* ---------- 4. parsing -------------------------------------------------- */
    let ast = if debug_mode {
        println!("Debug mode enabled – running verbose parser …");
        // Puoi decidere se far ritornare l’AST anche in debug_parsing; per ora lo usiamo solo per diagnosi.
        match parse_content(&expanded) {
            Ok(ast) => {
                println!("Parsing finished successfully ({} modules).", ast.len());
                ast
            }
            Err(e) => {
                eprintln!("Parsing failed: {e}");
                std::process::exit(1);
            }
        }
    } else {
        match parse_content(&expanded) {
            Ok(ast) => ast,
            Err(e) => {
                eprintln!("Parsing failed: {e}");
                std::process::exit(1);
            }
        }
    };

    /* ---------- 5. build: AST → Channel System ------------------------------ */
    match Builder::create_channel_system(ast) {
        Ok(_cs) => {
            println!("Channel system built successfully.");
        }
        Err(e) => {
            eprintln!("Builder error: {e}");
            if debug_mode {
                // stampa anche il dettaglio Debug quando --debug è attivo
                eprintln!("Debug (builder): {e:#?}");
            }
            std::process::exit(1);
        }
    }
}

fn process_directory(dir_path: &Path, debug_mode: bool) {
    // Reading all file inside te directory
    match fs::read_dir(dir_path) {
        Ok(entries) => {
            for entry in entries {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if path.is_file()
                        && path
                            .extension()
                            .map_or(false, |ext| ext == "prm" || ext == "pml")
                    {
                        process_file(&path, debug_mode);
                    } else if path.is_dir() {
                        process_directory(&path, debug_mode);
                    }
                }
            }
        }
        Err(err) => {
            eprintln!("Error reading directory {}: {}", dir_path.display(), err);
            std::process::exit(1);
        }
    }
}

fn read_file(file_path: &Path) -> Option<String> {
    let mut file = match File::open(file_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", file_path.display(), err);
            return None;
        }
    };

    let mut content = String::new();
    if let Err(err) = file.read_to_string(&mut content) {
        eprintln!("Error reading file {}: {}", file_path.display(), err);
        return None;
    }

    Some(content)
}

fn debug_parsing(content: &str) {
    // ---------------------------------------------------------------- Lexer
    let lexerdef = spinv_l::lexerdef();
    let lexer = lexerdef.lexer(content);

    println!("--- Lexer trace ---");
    for token in lexer.iter() {
        match token {
            Ok(lexeme) => {
                let span = lexeme.span();
                println!(
                    "Token: {:?}, text: '{}'",
                    lexeme.tok_id(),
                    lexer.span_str(span)
                );
            }
            Err(e) => eprintln!("Lexer error: {e:?}"),
        }
    }

    // ---------------------------------------------------------------- Parser
    println!("--- Parser trace ---");
    let (res, errs) = spinv4_y::parse(&lexer);

    // Print all error‑repair proposals reported by the GLR/LALR parser.
    for e in errs {
        println!("Parse error: {}", e.pp(&lexer, &spinv4_y::token_epp));
    }

    // Final outcome
    match res {
        Some(Ok(ast)) => {
            println!("Parsing finished successfully.");
            for module in ast {
                // Each `module` implements `Display`, so we show it nicely.
                println!("{module}");
            }
        }
        Some(Err(e)) => eprintln!("Parser returned an error: {e:?}"),
        None => eprintln!("Parsing failed: no result produced."),
    }
}

/// Tokenise `content`, parse it and return the AST (Vec<Module>).
///
/// - On success: Ok(Vec<Module>)
/// - On failure: Err(String) with diagnostics
fn parse_content(content: &str) -> Result<Vec<Module>, String> {
    // ------------------------------------------------------------- Lexing
    let lexer_def = spinv_l::lexerdef();
    let lexer = lexer_def.lexer(content);

    // ------------------------------------------------------------- Parsing
    let (result, errors) = spinv4_y::parse(&lexer);

    // Emit parser error messages (if any) to stderr, but don't abort yet:
    for err in errors {
        eprintln!("{}", err.pp(&lexer, &spinv4_y::token_epp));
    }

    // Stampa in formato debug
    // Some(Ok(r)) => println!("{:#?}", r),

    // Successful parse: print each top‑level module
    /*
    Some(Ok(ast)) => {
        println!("Parsing completed successfully:");
        for module in ast.into_iter() {
            println!("{module}");
        }
    }
    */

    // -------------------------------------------------------- Return AST or error
    match result {
        Some(Ok(ast)) => Ok(ast),
        Some(Err(e)) => Err(format!("Parser returned an error: {e:?}")),
        None => Err("Parsing failed: no result produced.".into()),
    }
}

/// Expand `#define` directives (simple constants and
/// “function‑like” macros) inside the Promela source.
///
/// * Replaces every constant/macros occurrence in‐line.
/// * Returns the updated source code.
///
/// Notes  
/// * Only handles one‑line `#define` statements.
/// * Function‑like macros are assumed to have the form  
///   `#define NAME(arg1, arg2) ( …body… )`.
fn replace_macros_in_content(src: &str) -> String {
    use regex::Regex;
    use std::collections::HashMap;

    // Maps for      #define  NAME   token
    let mut constants: HashMap<String, String> = HashMap::new();
    // Maps for   #define  NAME(args)  body
    let mut fun_macros: HashMap<String, (String, Vec<String>)> = HashMap::new();

    // ---------- 1. capture simple #define <name> <value>
    let re_const = Regex::new(r"(?m)^#define\s+(\S+)\s+(\S+)\s*$").unwrap();
    // ---------- 2. capture #define <name>(param, …) (body)
    let re_func = Regex::new(r"(?m)^#define\s+(\S+)\(([^)]*)\)\s*(\(.*\))").unwrap();

    // Strip constant defines and store them
    let mut code = re_const
        .replace_all(src, |caps: &regex::Captures| {
            constants.insert(caps[1].to_string(), caps[2].to_string());
            ""
        })
        .to_string();

    // Strip function‑like defines and store them
    code = re_func
        .replace_all(&code, |caps: &regex::Captures| {
            let name = caps[1].to_string();
            let params = caps[2]
                .split(',')
                .map(|s| s.trim().to_string())
                .collect::<Vec<_>>();
            let body = caps[3].to_string();
            fun_macros.insert(name, (body, params));
            ""
        })
        .to_string();

    // ---------- 3. replace constant occurrences
    for (name, value) in &constants {
        let re_name = Regex::new(&format!(r"\b{}\b", regex::escape(name))).unwrap();
        code = re_name.replace_all(&code, value.as_str()).to_string();
    }

    // ---------- 4. expand function‑like macro calls
    for (name, (body, params)) in &fun_macros {
        let re_call = Regex::new(&format!(r"\b{}\((.*?)\)", regex::escape(name))).unwrap();

        code = re_call
            .replace_all(&code, |caps: &regex::Captures| {
                let args: Vec<&str> = caps[1].split(',').map(|s| s.trim()).collect();
                let mut expansion = body.clone();

                for (param, arg) in params.iter().zip(args.iter()) {
                    expansion = expansion.replace(param, arg);
                }
                expansion
            })
            .to_string();
    }

    // ---------- 5. diagnostic output
    println!("Collected #define constants:");
    for (k, v) in &constants {
        println!("{k} -> {v}");
    }

    code
}

/// Remove both block (`/* … */`) and line (`// …`) comments from source.
///
/// Returns the cleaned source; keeps line breaks intact.
fn remove_comments(src: &str) -> String {
    let block_re = Regex::new(r"(?s)/\*.*?\*/").unwrap(); // multi‑line `/* … */`
    let line_re = Regex::new(r"(?m)//.*$").unwrap(); // single‑line  `// …`

    let no_block = block_re.replace_all(src, "").to_string();
    line_re.replace_all(&no_block, "").to_string()
}
