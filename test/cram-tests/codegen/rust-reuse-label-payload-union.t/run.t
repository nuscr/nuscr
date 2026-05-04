Rust codegen currently represents reused labels with one Action variant whose
payload fields are the union of all occurrences.
  $ nuscr --gencode-rust-test=C@Reuse Reuse.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Ack { dir: Direction },
      Msg { dir: Direction, x: i64, ok: bool },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Violation {
      ConstraintFailed { expr: &'static str },
      NoMatchingTransition,
      AlreadyFailed,
  }
  
  impl std::fmt::Display for Violation {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          match self {
              Violation::ConstraintFailed { expr } => write!(f, "constraint failed: {expr}"),
              Violation::NoMatchingTransition => write!(f, "no matching transition"),
              Violation::AlreadyFailed => write!(f, "already failed"),
          }
      }
  }
  
  impl std::error::Error for Violation {}
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum ReuseState {
      S0,
      S1 { x: i64 },
      S2 { x: i64 },
      S3 { x: i64, ok: bool },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct ReuseMonitor { state: ReuseState }
  
  #[allow(unused_variables)]
  impl ReuseMonitor {
      pub fn new() -> Self {
          Self { state: ReuseState::S0 }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ack { dir: Direction::Recv, .. } => true,
              Action::Msg { dir: Direction::Send, x, ok, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (ReuseState::Error, _) => Err(Violation::AlreadyFailed),
              (ReuseState::S0, Action::Msg { dir: Direction::Send, x, .. }) => {
                  let x = *x;
                  self.state = ReuseState::S1 { x };
                  Ok(())
              }
              (ReuseState::S1 { x }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let x = *x;
                  self.state = ReuseState::S2 { x };
                  Ok(())
              }
              (ReuseState::S2 { x }, Action::Msg { dir: Direction::Send, ok, .. }) => {
                  let x = *x;
                  let ok = *ok;
                  self.state = ReuseState::S3 { x, ok };
                  Ok(())
              }
              _ => { self.state = ReuseState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

The generated monitor remains valid Rust despite requiring callers to supply
both fields for either Msg action.
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib
