nuscr generates UnnamedPayload: refinement on payload with a dummy binding
  $ nuscr --gencode-rust-test=C@UnnamedPayload UnnamedPayload.nuscr
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Add { dir: Direction, x: i64, y: i64 },
      Bye { dir: Direction },
      Sum { dir: Direction, _dummy0: i64 },
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
  enum UnnamedPayloadState {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct UnnamedPayloadMonitor { state: UnnamedPayloadState }
  
  #[allow(unused_variables)]
  impl UnnamedPayloadMonitor {
      pub fn new() -> Self {
          Self { state: UnnamedPayloadState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Sum { dir: Direction::Recv, _dummy0, .. } => true,
              Action::Add { dir: Direction::Send, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (UnnamedPayloadState::Error, _) => Err(Violation::AlreadyFailed),
              (UnnamedPayloadState::S0 { total }, Action::Add { dir: Direction::Send, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !(((x) > (0)) && ((y) > (0))) { self.state = UnnamedPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "((x) > (0)) && ((y) > (0))" }); }
                  self.state = UnnamedPayloadState::S3 { total, x, y };
                  Ok(())
              }
              (UnnamedPayloadState::S0 { total }, Action::Bye { dir: Direction::Send, .. }) => {
                  let total = *total;
                  self.state = UnnamedPayloadState::S5 { total };
                  Ok(())
              }
              (UnnamedPayloadState::S3 { total, x, y }, Action::Sum { dir: Direction::Recv, _dummy0, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let _dummy0 = *_dummy0;
                  if !((x) > (y)) { self.state = UnnamedPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (y)" }); }
                  let new_total = (total) + (x);
                  if !((new_total) < (100)) { self.state = UnnamedPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                  self.state = UnnamedPayloadState::S0 { total: new_total };
                  Ok(())
              }
              (UnnamedPayloadState::S5 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  self.state = UnnamedPayloadState::S6 { total };
                  Ok(())
              }
              _ => { self.state = UnnamedPayloadState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

  $ nuscr --gencode-rust=C@UnnamedPayload UnnamedPayload.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum UnnamedPayloadState {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct UnnamedPayloadMonitor { state: UnnamedPayloadState }
  
  #[allow(unused_variables)]
  impl UnnamedPayloadMonitor {
      pub fn new() -> Self {
          Self { state: UnnamedPayloadState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Sum { dir: Direction::Recv, _dummy0, .. } => true,
              Action::Add { dir: Direction::Send, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (UnnamedPayloadState::Error, _) => Err(Violation::AlreadyFailed),
              (UnnamedPayloadState::S0 { total }, Action::Add { dir: Direction::Send, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !(((x) > (0)) && ((y) > (0))) { self.state = UnnamedPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "((x) > (0)) && ((y) > (0))" }); }
                  self.state = UnnamedPayloadState::S3 { total, x, y };
                  Ok(())
              }
              (UnnamedPayloadState::S0 { total }, Action::Bye { dir: Direction::Send, .. }) => {
                  let total = *total;
                  self.state = UnnamedPayloadState::S5 { total };
                  Ok(())
              }
              (UnnamedPayloadState::S3 { total, x, y }, Action::Sum { dir: Direction::Recv, _dummy0, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let _dummy0 = *_dummy0;
                  if !((x) > (y)) { self.state = UnnamedPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (y)" }); }
                  let new_total = (total) + (x);
                  if !((new_total) < (100)) { self.state = UnnamedPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                  self.state = UnnamedPayloadState::S0 { total: new_total };
                  Ok(())
              }
              (UnnamedPayloadState::S5 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  self.state = UnnamedPayloadState::S6 { total };
                  Ok(())
              }
              _ => { self.state = UnnamedPayloadState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  
