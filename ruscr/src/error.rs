use std::fmt;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Io {
        path: String,
        message: String,
    },
    Lexer(String),
    Parser(String),
    Unsupported(String),
    ProtocolNotFound(String),
    UnboundProtocol(String),
    UnboundRole(String),
    UnboundRecursionName(String),
    UnguardedTypeVariable(String),
    ReflexiveMessage(String),
    DuplicateLabel(String),
    RoleMismatch {
        expected: String,
        actual: String,
    },
    UnableToMerge {
        left: String,
        right: String,
        role: String,
    },
}

impl Error {
    pub fn user_message(&self) -> String {
        match self {
            Error::Io { path, message } => format!("{}: {}", path, message),
            Error::Lexer(msg) => format!("Lexer error: {}", msg),
            Error::Parser(msg) => format!("Parser error: {}", msg),
            Error::Unsupported(msg) => format!("Unsupported feature: {}", msg),
            Error::ProtocolNotFound(p) => format!("Protocol not found: {}", p),
            Error::UnboundProtocol(p) => format!("Unbound protocol {}", p),
            Error::UnboundRole(r) => format!("Unbound role {}", r),
            Error::UnboundRecursionName(n) => format!("Unbound recursion name {}", n),
            Error::UnguardedTypeVariable(n) => format!("Unguarded recursion variable {}", n),
            Error::ReflexiveMessage(r) => format!("Reflexive message involving role {}", r),
            Error::DuplicateLabel(l) => format!("Duplicate label {} in choices", l),
            Error::RoleMismatch { expected, actual } => {
                format!("Role mismatch: expected {}, got {}", expected, actual)
            }
            Error::UnableToMerge { left, right, role } => {
                format!(
                    "Unable to merge: ({}) and ({}) when projecting on role {}",
                    left, right, role
                )
            }
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.user_message())
    }
}

impl std::error::Error for Error {}
