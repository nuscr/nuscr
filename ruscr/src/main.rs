use std::process::ExitCode;

use ruscr::{efsm, format, Error};

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("ruscr: User error: {}", err.user_message());
            ExitCode::from(124)
        }
    }
}

fn run() -> ruscr::Result<()> {
    let mut args = std::env::args().skip(1).peekable();
    let mut filename = None;
    let mut enumerate = false;
    let mut global_actions: Vec<(String, String)> = Vec::new();
    let mut local_actions: Vec<(String, String, String)> = Vec::new();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--enum" => enumerate = true,
            "--show-global-type"
            | "--show-global-type-mpstk"
            | "--show-global-type-tex"
            | "--generate-sexp" => {
                let protocol = args
                    .next()
                    .ok_or_else(|| Error::Parser(format!("missing value for {}", arg)))?;
                global_actions.push((arg, protocol));
            }
            "--project" | "--project-mpstk" | "--project-tex" | "--fsm" => {
                let rp = args
                    .next()
                    .ok_or_else(|| Error::Parser(format!("missing value for {}", arg)))?;
                let (role, protocol) = parse_role_protocol(&rp)?;
                local_actions.push((arg, role, protocol));
            }
            "--show-global-type-protobuf"
            | "--project-protobuf"
            | "--gencode-fstar"
            | "--gencode-go"
            | "--gencode-ocaml"
            | "--gencode-ocaml-monadic"
            | "--show-solver-queries" => {
                return Err(Error::Unsupported(arg));
            }
            "-v" | "--verbose" => {}
            value if value.starts_with('-') => {
                return Err(Error::Parser(format!("unknown flag {}", value)))
            }
            value => filename = Some(value.to_string()),
        }
    }

    let filename = filename.ok_or_else(|| Error::Parser("missing FILE".to_string()))?;
    let module = ruscr::parse_file(&filename)?;
    ruscr::validate(&module)?;

    for (flag, protocol) in global_actions {
        let g = ruscr::get_global_type(&module, &protocol)?;
        match flag.as_str() {
            "--show-global-type" => println!("{}", format::show_global(&g)),
            "--show-global-type-mpstk" => println!("{}", format::show_global_mpstk(&g)),
            "--show-global-type-tex" => println!("{}", format::show_global_tex(&g)),
            "--generate-sexp" => println!("{}", ruscr::generate_sexp(&module, &protocol)?),
            _ => unreachable!(),
        }
    }

    for (flag, role, protocol) in local_actions {
        let l = ruscr::project_role(&module, &protocol, &role)?;
        match flag.as_str() {
            "--project" => println!("{}", format::show_local(&l)),
            "--project-mpstk" => println!("{}", format::show_local_mpstk(&l)),
            "--project-tex" => println!("{}", format::show_local_tex(&l)),
            "--fsm" => println!(
                "{}",
                efsm::show(&ruscr::generate_fsm(&module, &protocol, &role)?)
            ),
            _ => unreachable!(),
        }
    }

    if enumerate {
        for (protocol, role) in ruscr::enumerate(&module) {
            println!("{}@{}", role, protocol);
        }
    }

    Ok(())
}

fn parse_role_protocol(input: &str) -> ruscr::Result<(String, String)> {
    let mut parts = input.split('@');
    let role = parts.next().unwrap_or_default();
    let protocol = parts.next().unwrap_or_default();
    if role.is_empty() || protocol.is_empty() || parts.next().is_some() {
        return Err(Error::Parser(
            "Role and protocol have to be for the form role@protocol".to_string(),
        ));
    }
    Ok((role.to_string(), protocol.to_string()))
}
