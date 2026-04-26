pub mod efsm;
pub mod error;
pub mod format;
pub mod parser;
pub mod project;
pub mod syntax;

pub use error::{Error, Result};
pub use syntax::{GlobalType, LocalType, Message, Module, Protocol};

pub fn parse_string(input: &str) -> Result<Module> {
    parser::parse(input)
}

pub fn parse_file(path: &str) -> Result<Module> {
    let input = std::fs::read_to_string(path).map_err(|err| Error::Io {
        path: path.to_string(),
        message: err.to_string(),
    })?;
    parse_string(&input)
}

pub fn validate(module: &Module) -> Result<()> {
    for protocol in &module.protocols {
        let g = get_global_type(module, &protocol.name)?;
        for role in &protocol.roles {
            let local = project::project(role, &g)?;
            let _ = efsm::from_local_type(&local);
        }
    }
    Ok(())
}

pub fn protocol_names(module: &Module) -> Vec<String> {
    module.protocols.iter().map(|p| p.name.clone()).collect()
}

pub fn enumerate(module: &Module) -> Vec<(String, String)> {
    module
        .protocols
        .iter()
        .flat_map(|p| p.roles.iter().map(|r| (p.name.clone(), r.clone())))
        .collect()
}

pub fn get_global_type(module: &Module, protocol: &str) -> Result<GlobalType> {
    let protocol = module
        .protocols
        .iter()
        .find(|p| p.name == protocol)
        .ok_or_else(|| Error::ProtocolNotFound(protocol.to_string()))?;
    syntax::expand_protocol(module, protocol)
}

pub fn project_role(module: &Module, protocol: &str, role: &str) -> Result<LocalType> {
    let g = get_global_type(module, protocol)?;
    project::project(role, &g)
}

pub fn generate_fsm(module: &Module, protocol: &str, role: &str) -> Result<efsm::Efsm> {
    let local = project_role(module, protocol, role)?;
    Ok(efsm::from_local_type(&local))
}

pub fn generate_sexp(module: &Module, protocol: &str) -> Result<String> {
    let g = get_global_type(module, protocol)?;
    Ok(format::sexp_global(&syntax::normalise(&g)))
}
