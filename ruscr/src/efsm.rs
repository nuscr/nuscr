use crate::format;
use crate::syntax::LocalType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Transition {
    pub from: usize,
    pub label: String,
    pub to: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Efsm {
    pub start: usize,
    pub states: usize,
    pub transitions: Vec<Transition>,
}

pub fn from_local_type(local: &LocalType) -> Efsm {
    let mut builder = Builder {
        next_state: 0,
        transitions: Vec::new(),
        rec_vars: Vec::new(),
    };
    let start = builder.convert(local);
    Efsm {
        start,
        states: builder.next_state,
        transitions: builder.transitions,
    }
}

pub fn show(efsm: &Efsm) -> String {
    let mut out = String::from("digraph G {\n");
    for state in 0..efsm.states {
        out.push_str(&format!("  {};\n", state));
    }
    for transition in &efsm.transitions {
        out.push_str(&format!(
            "  {} -> {} [label=\"{}\"];\n",
            transition.from, transition.to, transition.label
        ));
    }
    out.push('}');
    out
}

struct Builder {
    next_state: usize,
    transitions: Vec<Transition>,
    rec_vars: Vec<(String, usize)>,
}

impl Builder {
    fn fresh(&mut self) -> usize {
        let state = self.next_state;
        self.next_state += 1;
        state
    }

    fn convert(&mut self, local: &LocalType) -> usize {
        match local {
            LocalType::EndL => self.fresh(),
            LocalType::TVarL(var) => self
                .rec_vars
                .iter()
                .rev()
                .find(|(name, _)| name == var)
                .map(|(_, state)| *state)
                .unwrap_or_else(|| self.fresh()),
            LocalType::MuL(var, body) => {
                let start = self.fresh();
                self.rec_vars.push((var.clone(), start));
                let body_start = self.convert(body);
                self.transitions.push(Transition {
                    from: start,
                    label: "epsilon".to_string(),
                    to: body_start,
                });
                self.rec_vars.pop();
                start
            }
            LocalType::SendL(message, role, next) => {
                let current = self.fresh();
                let next_state = self.convert(next);
                self.transitions.push(Transition {
                    from: current,
                    label: format!("{}!{}", role, format::show_message(message)),
                    to: next_state,
                });
                current
            }
            LocalType::RecvL(message, role, next) => {
                let current = self.fresh();
                let next_state = self.convert(next);
                self.transitions.push(Transition {
                    from: current,
                    label: format!("{}?{}", role, format::show_message(message)),
                    to: next_state,
                });
                current
            }
            LocalType::ChoiceL(_, branches) => {
                let current = self.fresh();
                for branch in branches {
                    let branch_state = self.convert(branch);
                    self.transitions.push(Transition {
                        from: current,
                        label: "epsilon".to_string(),
                        to: branch_state,
                    });
                }
                current
            }
        }
    }
}
