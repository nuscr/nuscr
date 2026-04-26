use crate::syntax::{GlobalType, LocalType, Message};

pub fn show_message(message: &Message) -> String {
    format!("{}({})", message.label, message.payload.join(", "))
}

pub fn show_global(g: &GlobalType) -> String {
    let mut out = String::new();
    fmt_global(g, 0, &mut out);
    out
}

fn fmt_global(g: &GlobalType, indent: usize, out: &mut String) {
    let pad = " ".repeat(indent);
    match g {
        GlobalType::MessageG(m, from, to, next) => {
            out.push_str(&format!(
                "{}{} from {} to {};\n",
                pad,
                show_message(m),
                from,
                to
            ));
            fmt_global(next, indent, out);
        }
        GlobalType::MuG(var, body) => {
            out.push_str(&format!("{}rec {} {{\n", pad, var));
            fmt_global(body, indent + 2, out);
            out.push_str(&format!("{}}}", pad));
        }
        GlobalType::TVarG(var) => out.push_str(&format!("{}continue {};", pad, var)),
        GlobalType::ChoiceG(role, branches) => {
            out.push_str(&format!("{}choice at {} {{\n", pad, role));
            for (idx, branch) in branches.iter().enumerate() {
                if idx > 0 {
                    out.push_str(&format!("\n{}}} or {{\n", pad));
                }
                fmt_global(branch, indent + 2, out);
            }
            out.push_str(&format!("{}}}", pad));
        }
        GlobalType::EndG => out.push_str(&format!("{}(end)", pad)),
    }
}

pub fn show_local(l: &LocalType) -> String {
    let mut out = String::new();
    fmt_local(l, 0, &mut out);
    out
}

fn fmt_local(l: &LocalType, indent: usize, out: &mut String) {
    let pad = " ".repeat(indent);
    match l {
        LocalType::RecvL(m, from, next) => {
            out.push_str(&format!("{}{} from {};\n", pad, show_message(m), from));
            fmt_local(next, indent, out);
        }
        LocalType::SendL(m, to, next) => {
            out.push_str(&format!("{}{} to {};\n", pad, show_message(m), to));
            fmt_local(next, indent, out);
        }
        LocalType::MuL(var, body) => {
            out.push_str(&format!("{}rec {} {{\n", pad, var));
            fmt_local(body, indent + 2, out);
            out.push_str(&format!("\n{}}}", pad));
        }
        LocalType::TVarL(var) => out.push_str(&format!("{}continue {};", pad, var)),
        LocalType::ChoiceL(role, branches) => {
            out.push_str(&format!("{}choice at {} {{\n", pad, role));
            for (idx, branch) in branches.iter().enumerate() {
                if idx > 0 {
                    out.push_str(&format!("\n{}}} or {{\n", pad));
                }
                fmt_local(branch, indent + 2, out);
            }
            out.push_str(&format!("{}}}", pad));
        }
        LocalType::EndL => out.push_str(&format!("{}(end)", pad)),
    }
}

pub fn sexp_global(g: &GlobalType) -> String {
    match g {
        GlobalType::MessageG(m, from, to, next) => format!(
            "(MessageG ((label {}) (payload ({}))) {} {} {})",
            m.label,
            m.payload.join(" "),
            from,
            to,
            sexp_global(next)
        ),
        GlobalType::MuG(var, body) => format!("(MuG {} () {})", var, sexp_global(body)),
        GlobalType::TVarG(var) => format!("(TVarG {} () <opaque>)", var),
        GlobalType::ChoiceG(role, branches) => {
            let inner = branches
                .iter()
                .map(sexp_global)
                .collect::<Vec<_>>()
                .join(" ");
            format!("(ChoiceG {} ({}))", role, inner)
        }
        GlobalType::EndG => "EndG".to_string(),
    }
}

pub fn show_global_mpstk(g: &GlobalType) -> String {
    match g {
        GlobalType::MessageG(m, from, to, next) => {
            format!(
                "{}->{}:{}({}) . {}",
                from,
                to,
                m.label,
                m.payload.join(", "),
                show_global_mpstk(next)
            )
        }
        GlobalType::MuG(var, body) => format!("mu({})({})", var, show_global_mpstk(body)),
        GlobalType::TVarG(var) => var.clone(),
        GlobalType::ChoiceG(role, branches) => {
            let rendered = branches
                .iter()
                .map(show_global_mpstk)
                .collect::<Vec<_>>()
                .join(",\n");
            format!("{}->?:{{\n{}\n}}", role, rendered)
        }
        GlobalType::EndG => "end".to_string(),
    }
    .replace("->", "→")
    .replace("mu", "μ")
}

pub fn show_local_mpstk(l: &LocalType) -> String {
    match l {
        LocalType::SendL(m, to, next) => {
            format!(
                "{}⊕{}({}) . {}",
                to,
                m.label,
                m.payload.join(", "),
                show_local_mpstk(next)
            )
        }
        LocalType::RecvL(m, from, next) => {
            format!(
                "{}&{}({}) . {}",
                from,
                m.label,
                m.payload.join(", "),
                show_local_mpstk(next)
            )
        }
        LocalType::MuL(var, body) => format!("μ({})({})", var, show_local_mpstk(body)),
        LocalType::TVarL(var) => var.clone(),
        LocalType::ChoiceL(role, branches) => {
            let rendered = branches
                .iter()
                .map(show_local_mpstk)
                .collect::<Vec<_>>()
                .join(",\n");
            format!("{}&{{\n{}\n}}", role, rendered)
        }
        LocalType::EndL => "end".to_string(),
    }
}

pub fn show_global_tex(g: &GlobalType) -> String {
    show_global_mpstk(g)
}

pub fn show_local_tex(l: &LocalType) -> String {
    show_local_mpstk(l)
}
