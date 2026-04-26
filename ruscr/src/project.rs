use std::collections::BTreeSet;

use crate::error::{Error, Result};
use crate::format;
use crate::syntax::{GlobalType, LocalType, Message};

pub fn project(role: &str, g: &GlobalType) -> Result<LocalType> {
    project_inner(role, role, g, &mut BTreeSet::new())
}

fn project_inner(
    root_role: &str,
    role: &str,
    g: &GlobalType,
    unguarded: &mut BTreeSet<String>,
) -> Result<LocalType> {
    match g {
        GlobalType::EndG => Ok(LocalType::EndL),
        GlobalType::TVarG(var) if unguarded.contains(var) => Ok(LocalType::EndL),
        GlobalType::TVarG(var) => Ok(LocalType::TVarL(var.clone())),
        GlobalType::MuG(var, body) => {
            unguarded.insert(var.clone());
            let body = project_inner(root_role, role, body, unguarded)?;
            unguarded.remove(var);
            match body {
                LocalType::EndL | LocalType::TVarL(_) => Ok(LocalType::EndL),
                other => Ok(LocalType::MuL(var.clone(), Box::new(other))),
            }
        }
        GlobalType::MessageG(message, from, to, next) if role == from => {
            unguarded.clear();
            Ok(LocalType::SendL(
                message.clone(),
                to.clone(),
                Box::new(project_inner(root_role, role, next, unguarded)?),
            ))
        }
        GlobalType::MessageG(message, from, to, next) if role == to => {
            unguarded.clear();
            Ok(LocalType::RecvL(
                message.clone(),
                from.clone(),
                Box::new(project_inner(root_role, role, next, unguarded)?),
            ))
        }
        GlobalType::MessageG(_, _, _, next) => project_inner(root_role, role, next, unguarded),
        GlobalType::ChoiceG(choice_role, branches) => {
            check_distinct_prefix(branches)?;
            let recv_role = choice_receiver(choice_role, branches)?;
            let locals = branches
                .iter()
                .map(|branch| project_inner(root_role, role, branch, &mut unguarded.clone()))
                .collect::<Result<Vec<_>>>()?;
            if role == choice_role || role == recv_role {
                Ok(LocalType::ChoiceL(choice_role.clone(), locals))
            } else {
                let mut iter = locals.into_iter();
                let Some(mut acc) = iter.next() else {
                    return Ok(LocalType::EndL);
                };
                for local in iter {
                    acc = merge(root_role, acc, local)?;
                }
                Ok(acc)
            }
        }
    }
}

fn check_distinct_prefix(branches: &[GlobalType]) -> Result<()> {
    let mut seen = BTreeSet::new();
    for branch in branches {
        for (label, _, _) in first_messages(branch)? {
            if !seen.insert(label.clone()) {
                return Err(Error::DuplicateLabel(label));
            }
        }
    }
    Ok(())
}

fn choice_receiver<'a>(choice_role: &str, branches: &'a [GlobalType]) -> Result<&'a str> {
    let mut recv = None;
    for branch in branches {
        for (_, sender, receiver) in first_messages(branch)? {
            if sender != choice_role {
                return Err(Error::RoleMismatch {
                    expected: choice_role.to_string(),
                    actual: sender.to_string(),
                });
            }
            match recv {
                None => recv = Some(receiver),
                Some(existing) if existing == receiver => {}
                Some(existing) => {
                    return Err(Error::RoleMismatch {
                        expected: existing.to_string(),
                        actual: receiver.to_string(),
                    })
                }
            }
        }
    }
    recv.ok_or_else(|| Error::Parser("choice without message branches".to_string()))
}

fn first_messages(g: &GlobalType) -> Result<Vec<(String, &str, &str)>> {
    match g {
        GlobalType::MessageG(message, from, to, _) => Ok(vec![(message.label.clone(), from, to)]),
        GlobalType::ChoiceG(_, branches) => {
            let mut messages = Vec::new();
            for branch in branches {
                messages.extend(first_messages(branch)?);
            }
            Ok(messages)
        }
        GlobalType::MuG(_, body) => first_messages(body),
        _ => Err(Error::Parser("choice branch has no message".to_string())),
    }
}

fn merge(role: &str, left: LocalType, right: LocalType) -> Result<LocalType> {
    if left == right {
        return Ok(left);
    }
    match (left, right) {
        (LocalType::RecvL(m1, r1, n1), LocalType::RecvL(m2, r2, n2)) if r1 == r2 => {
            if m1.label == m2.label && payloads_match(&m1, &m2) {
                Ok(LocalType::RecvL(m1, r1, Box::new(merge(role, *n1, *n2)?)))
            } else if m1.label != m2.label {
                Ok(LocalType::ChoiceL(
                    r1,
                    vec![
                        LocalType::RecvL(m1, r2.clone(), n1),
                        LocalType::RecvL(m2, r2, n2),
                    ],
                ))
            } else {
                merge_err(
                    role,
                    LocalType::RecvL(m1, r1, n1),
                    LocalType::RecvL(m2, r2, n2),
                )
            }
        }
        (LocalType::SendL(m1, r1, n1), LocalType::SendL(m2, r2, n2)) if r1 == r2 && m1 == m2 => {
            Ok(LocalType::SendL(m1, r1, Box::new(merge(role, *n1, *n2)?)))
        }
        (LocalType::ChoiceL(r1, mut xs), LocalType::RecvL(m, r2, n)) if r1 == r2 => {
            xs.push(LocalType::RecvL(m, r2, n));
            Ok(LocalType::ChoiceL(r1, xs))
        }
        (LocalType::RecvL(m, r1, n), LocalType::ChoiceL(r2, mut xs)) if r1 == r2 => {
            xs.push(LocalType::RecvL(m, r1.clone(), n));
            Ok(LocalType::ChoiceL(r1, xs))
        }
        (LocalType::ChoiceL(r1, mut xs), LocalType::ChoiceL(r2, ys)) if r1 == r2 => {
            xs.extend(ys);
            Ok(LocalType::ChoiceL(r1, xs))
        }
        (left, right) => merge_err(role, left, right),
    }
}

fn payloads_match(left: &Message, right: &Message) -> bool {
    left.payload == right.payload
}

fn merge_err(role: &str, left: LocalType, right: LocalType) -> Result<LocalType> {
    Err(Error::UnableToMerge {
        left: format::show_local(&left),
        right: format::show_local(&right),
        role: role.to_string(),
    })
}
