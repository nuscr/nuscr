use std::collections::{BTreeMap, BTreeSet};

use crate::error::{Error, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub protocols: Vec<Protocol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Protocol {
    pub name: String,
    pub roles: Vec<String>,
    pub interactions: Vec<Interaction>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Message {
    pub label: String,
    pub payload: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Interaction {
    Message {
        message: Message,
        from: String,
        to: Vec<String>,
    },
    Rec {
        var: String,
        body: Vec<Interaction>,
    },
    Continue(String),
    Choice {
        at: String,
        branches: Vec<Vec<Interaction>>,
    },
    Do {
        protocol: String,
        roles: Vec<String>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GlobalType {
    MessageG(Message, String, String, Box<GlobalType>),
    MuG(String, Box<GlobalType>),
    TVarG(String),
    ChoiceG(String, Vec<GlobalType>),
    EndG,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LocalType {
    RecvL(Message, String, Box<LocalType>),
    SendL(Message, String, Box<LocalType>),
    ChoiceL(String, Vec<LocalType>),
    TVarL(String),
    MuL(String, Box<LocalType>),
    EndL,
}

pub fn expand_protocol(module: &Module, protocol: &Protocol) -> Result<GlobalType> {
    for role in &protocol.roles {
        if protocol.roles.iter().filter(|r| *r == role).count() > 1 {
            return Err(Error::UnboundRole(role.clone()));
        }
    }
    let key = call_key(&protocol.name, &protocol.roles);
    let rec_var = rec_var_name(&protocol.name, &protocol.roles);
    let mut active = BTreeMap::new();
    active.insert(key.clone(), rec_var.clone());
    let mut body = expand_recursive_do(
        module,
        protocol,
        &protocol.interactions,
        &protocol.roles,
        &mut active,
    )?;
    if contains_do(&protocol.interactions, &protocol.name) {
        body = vec![Interaction::Rec { var: rec_var, body }];
    }
    interactions_to_global(&protocol.roles, &body)
}

fn expand_recursive_do(
    module: &Module,
    protocol: &Protocol,
    interactions: &[Interaction],
    actual_roles: &[String],
    active: &mut BTreeMap<String, String>,
) -> Result<Vec<Interaction>> {
    let mut body = Vec::new();
    for interaction in interactions {
        match interaction {
            Interaction::Do { protocol: p, roles } => {
                let callee = find_protocol(module, p)?;
                let mapped_roles: Vec<String> = if p == &protocol.name {
                    roles
                        .iter()
                        .map(|r| map_role(r, &protocol.roles, actual_roles))
                        .collect()
                } else {
                    roles.clone()
                };
                validate_arity(callee, &mapped_roles)?;
                let key = call_key(&callee.name, &mapped_roles);
                if let Some(var) = active.get(&key) {
                    body.push(Interaction::Continue(var.clone()));
                    continue;
                }
                let rec_var = rec_var_name(&callee.name, &mapped_roles);
                active.insert(key.clone(), rec_var.clone());
                let mut expanded = expand_recursive_do(
                    module,
                    callee,
                    &callee.interactions,
                    &mapped_roles,
                    active,
                )?;
                active.remove(&key);
                if contains_continue(&expanded, &rec_var) {
                    expanded = vec![Interaction::Rec {
                        var: rec_var,
                        body: expanded,
                    }];
                }
                body.extend(expanded);
            }
            Interaction::Rec { var, body: inner } => {
                let inner = expand_recursive_do(module, protocol, inner, actual_roles, active)?;
                body.push(Interaction::Rec {
                    var: var.clone(),
                    body: inner,
                });
            }
            Interaction::Choice { at, branches } => {
                let mut out = Vec::new();
                for branch in branches {
                    out.push(expand_recursive_do(
                        module,
                        protocol,
                        branch,
                        actual_roles,
                        active,
                    )?);
                }
                body.push(Interaction::Choice {
                    at: map_role(at, &protocol.roles, actual_roles),
                    branches: out,
                });
            }
            other => body.push(substitute_interaction(other, &protocol.roles, actual_roles)),
        }
    }
    Ok(body)
}

fn contains_continue(interactions: &[Interaction], var: &str) -> bool {
    interactions.iter().any(|interaction| match interaction {
        Interaction::Continue(v) => v == var,
        Interaction::Rec { body, .. } => contains_continue(body, var),
        Interaction::Choice { branches, .. } => branches.iter().any(|b| contains_continue(b, var)),
        _ => false,
    })
}

fn find_protocol<'a>(module: &'a Module, protocol: &str) -> Result<&'a Protocol> {
    module
        .protocols
        .iter()
        .find(|candidate| candidate.name == protocol)
        .ok_or_else(|| Error::UnboundProtocol(protocol.to_string()))
}

fn validate_arity(protocol: &Protocol, roles: &[String]) -> Result<()> {
    if protocol.roles.len() == roles.len() {
        Ok(())
    } else {
        Err(Error::Parser(format!(
            "arity mismatch in do {}: expected {}, got {}",
            protocol.name,
            protocol.roles.len(),
            roles.len()
        )))
    }
}

fn call_key(protocol: &str, roles: &[String]) -> String {
    format!("{}({})", protocol, roles.join(","))
}

fn contains_do(interactions: &[Interaction], protocol: &str) -> bool {
    interactions.iter().any(|interaction| match interaction {
        Interaction::Do { protocol: p, .. } => p == protocol,
        Interaction::Rec { body, .. } => contains_do(body, protocol),
        Interaction::Choice { branches, .. } => branches.iter().any(|b| contains_do(b, protocol)),
        _ => false,
    })
}

fn rec_var_name(protocol: &str, roles: &[String]) -> String {
    format!("__{}_{}", protocol, roles.join("_"))
}

fn substitute_roles(
    interactions: &[Interaction],
    formal: &[String],
    actual: &[String],
) -> Vec<Interaction> {
    interactions
        .iter()
        .map(|interaction| substitute_interaction(interaction, formal, actual))
        .collect()
}

fn substitute_interaction(
    interaction: &Interaction,
    formal: &[String],
    actual: &[String],
) -> Interaction {
    match interaction {
        Interaction::Message { message, from, to } => Interaction::Message {
            message: message.clone(),
            from: map_role(from, formal, actual),
            to: to.iter().map(|r| map_role(r, formal, actual)).collect(),
        },
        Interaction::Rec { var, body } => Interaction::Rec {
            var: var.clone(),
            body: substitute_roles(body, formal, actual),
        },
        Interaction::Continue(var) => Interaction::Continue(var.clone()),
        Interaction::Choice { at, branches } => Interaction::Choice {
            at: map_role(at, formal, actual),
            branches: branches
                .iter()
                .map(|b| substitute_roles(b, formal, actual))
                .collect(),
        },
        Interaction::Do { protocol, roles } => Interaction::Do {
            protocol: protocol.clone(),
            roles: roles.iter().map(|r| map_role(r, formal, actual)).collect(),
        },
    }
}

fn map_role(role: &str, formal: &[String], actual: &[String]) -> String {
    formal
        .iter()
        .position(|candidate| candidate == role)
        .and_then(|idx| actual.get(idx))
        .cloned()
        .unwrap_or_else(|| role.to_string())
}

fn interactions_to_global(roles: &[String], interactions: &[Interaction]) -> Result<GlobalType> {
    fn conv(
        roles: &[String],
        interactions: &[Interaction],
        rec_stack: &mut Vec<(String, bool)>,
    ) -> Result<(GlobalType, BTreeSet<String>)> {
        if interactions.is_empty() {
            return Ok((GlobalType::EndG, BTreeSet::new()));
        }
        let (first, rest) = interactions.split_first().unwrap();
        match first {
            Interaction::Message { message, from, to } => {
                check_role(roles, from)?;
                let mut guarded_stack = rec_stack.clone();
                for (_, unguarded) in &mut guarded_stack {
                    *unguarded = false;
                }
                let (mut acc, free) = conv(roles, rest, &mut guarded_stack)?;
                for recv in to.iter().rev() {
                    check_role(roles, recv)?;
                    if recv == from {
                        return Err(Error::ReflexiveMessage(from.clone()));
                    }
                    acc = GlobalType::MessageG(
                        message.clone(),
                        from.clone(),
                        recv.clone(),
                        Box::new(acc),
                    );
                }
                Ok((acc, free))
            }
            Interaction::Rec { var, body } => {
                if !rest.is_empty() {
                    return Err(Error::Unsupported(
                        "non-tail recursive protocol".to_string(),
                    ));
                }
                rec_stack.push((var.clone(), true));
                let (body, mut free) = conv(roles, body, rec_stack)?;
                rec_stack.pop();
                if free.remove(var) {
                    Ok((GlobalType::MuG(var.clone(), Box::new(body)), free))
                } else {
                    Ok((body, free))
                }
            }
            Interaction::Continue(var) => {
                if !rest.is_empty() {
                    return Err(Error::Unsupported("non-tail continue".to_string()));
                }
                match rec_stack.iter_mut().rev().find(|(name, _)| name == var) {
                    Some((_, unguarded)) if *unguarded => {
                        Err(Error::UnguardedTypeVariable(var.clone()))
                    }
                    Some((_, _)) => {
                        let mut free = BTreeSet::new();
                        free.insert(var.clone());
                        Ok((GlobalType::TVarG(var.clone()), free))
                    }
                    None => Err(Error::UnboundRecursionName(var.clone())),
                }
            }
            Interaction::Choice { at, branches } => {
                if !rest.is_empty() {
                    return Err(Error::Unsupported("non-tail choice".to_string()));
                }
                check_role(roles, at)?;
                if branches.len() == 1 {
                    return conv(roles, &branches[0], rec_stack);
                }
                let mut choices = Vec::new();
                let mut free = BTreeSet::new();
                for branch in branches {
                    let mut branch_stack = rec_stack.clone();
                    let (choice, branch_free) = conv(roles, branch, &mut branch_stack)?;
                    choices.push(choice);
                    free.extend(branch_free);
                }
                Ok((GlobalType::ChoiceG(at.clone(), choices), free))
            }
            Interaction::Do { .. } => Err(Error::Unsupported("unexpanded do".to_string())),
        }
    }

    let (g, free) = conv(roles, interactions, &mut Vec::new())?;
    if let Some(name) = free.into_iter().next() {
        Err(Error::UnboundRecursionName(name))
    } else {
        Ok(g)
    }
}

fn check_role(roles: &[String], role: &str) -> Result<()> {
    if roles.iter().any(|r| r == role) {
        Ok(())
    } else {
        Err(Error::UnboundRole(role.to_string()))
    }
}

pub fn normalise(g: &GlobalType) -> GlobalType {
    match g {
        GlobalType::MessageG(m, s, r, next) => {
            GlobalType::MessageG(m.clone(), s.clone(), r.clone(), Box::new(normalise(next)))
        }
        GlobalType::ChoiceG(r, choices) => {
            let mut flat = Vec::new();
            for choice in choices {
                match normalise(choice) {
                    GlobalType::ChoiceG(inner_r, inner) if inner_r == *r => flat.extend(inner),
                    other => flat.push(other),
                }
            }
            GlobalType::ChoiceG(r.clone(), flat)
        }
        GlobalType::MuG(v, body) => GlobalType::MuG(v.clone(), Box::new(normalise(body))),
        other => other.clone(),
    }
}

pub fn first_label(g: &GlobalType) -> Option<String> {
    match g {
        GlobalType::MessageG(m, _, _, _) => Some(m.label.clone()),
        GlobalType::ChoiceG(_, choices) => choices.first().and_then(first_label),
        GlobalType::MuG(_, body) => first_label(body),
        _ => None,
    }
}

pub fn message_payload_map(message: &Message) -> BTreeMap<String, usize> {
    message
        .payload
        .iter()
        .enumerate()
        .map(|(idx, payload)| (payload.clone(), idx))
        .collect()
}
