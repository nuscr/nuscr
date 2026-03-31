Generate Rust monitor for Client (multi payload, cross-payload reference)
  $ nuscr --gencode-rust-test=C@MultiPayload MultiPayload.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Req { dir: Direction, a: i64, b: i64 },
      Resp { dir: Direction, d: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Violation {
      ConstraintFailed { expr: &'static str },
      NoMatchingTransition,
      AlreadyFailed,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum MultiPayloadState {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor { state: MultiPayloadState }
  
  #[allow(unused_variables)]
  impl MultiPayloadMonitor {
      pub fn new() -> Self {
          Self { state: MultiPayloadState::S0 }
      }
  
      pub const NAME: &'static str = "MultiPayload";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Resp { dir: Direction::Recv, d, .. } => true,
              Action::Req { dir: Direction::Send, a, b, .. } => {
                  let a = *a;
                  let b = *b;
                  ((a) > (0)) && (((b) > (0)) && ((b) < (a)))
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (MultiPayloadState::Error, _) => Err(Violation::AlreadyFailed),
              (MultiPayloadState::S0, Action::Req { dir: Direction::Send, a, b, .. }) => {
                  let a = *a;
                  let b = *b;
                  if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { self.state = MultiPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(a) > (0) && ((b) > (0)) && ((b) < (a))" }); }
                  self.state = MultiPayloadState::S1 { a, b };
                  Ok(())
              }
              (MultiPayloadState::S1 { a, b }, Action::Resp { dir: Direction::Recv, d, .. }) => {
                  let a = *a;
                  let b = *b;
                  let d = *d;
                  if !((d) == ((a) - (b))) { self.state = MultiPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(d) == ((a) - (b))" }); }
                  self.state = MultiPayloadState::S2 { a, b, d };
                  Ok(())
              }
              _ => { self.state = MultiPayloadState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Generate Rust monitor for Server (nested arith, cross-payload reference)
  $ nuscr --gencode-rust-test=S@MultiPayload MultiPayload.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Req { dir: Direction, a: i64, b: i64 },
      Resp { dir: Direction, d: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Violation {
      ConstraintFailed { expr: &'static str },
      NoMatchingTransition,
      AlreadyFailed,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum MultiPayloadState {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor { state: MultiPayloadState }
  
  #[allow(unused_variables)]
  impl MultiPayloadMonitor {
      pub fn new() -> Self {
          Self { state: MultiPayloadState::S0 }
      }
  
      pub const NAME: &'static str = "MultiPayload";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Req { dir: Direction::Recv, a, b, .. } => {
                  let a = *a;
                  let b = *b;
                  ((a) > (0)) && (((b) > (0)) && ((b) < (a)))
              }
              Action::Resp { dir: Direction::Send, d, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (MultiPayloadState::Error, _) => Err(Violation::AlreadyFailed),
              (MultiPayloadState::S0, Action::Req { dir: Direction::Recv, a, b, .. }) => {
                  let a = *a;
                  let b = *b;
                  if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { self.state = MultiPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(a) > (0) && ((b) > (0)) && ((b) < (a))" }); }
                  self.state = MultiPayloadState::S1 { a, b };
                  Ok(())
              }
              (MultiPayloadState::S1 { a, b }, Action::Resp { dir: Direction::Send, d, .. }) => {
                  let a = *a;
                  let b = *b;
                  let d = *d;
                  if !((d) == ((a) - (b))) { self.state = MultiPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(d) == ((a) - (b))" }); }
                  self.state = MultiPayloadState::S2 { a, b, d };
                  Ok(())
              }
              _ => { self.state = MultiPayloadState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen (no support types, not compiled)
  $ nuscr --gencode-rust=C@MultiPayload MultiPayload.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum MultiPayloadState {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor { state: MultiPayloadState }
  
  #[allow(unused_variables)]
  impl MultiPayloadMonitor {
      pub fn new() -> Self {
          Self { state: MultiPayloadState::S0 }
      }
  
      pub const NAME: &'static str = "MultiPayload";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Resp { dir: Direction::Recv, d, .. } => true,
              Action::Req { dir: Direction::Send, a, b, .. } => {
                  let a = *a;
                  let b = *b;
                  ((a) > (0)) && (((b) > (0)) && ((b) < (a)))
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (MultiPayloadState::Error, _) => Err(Violation::AlreadyFailed),
              (MultiPayloadState::S0, Action::Req { dir: Direction::Send, a, b, .. }) => {
                  let a = *a;
                  let b = *b;
                  if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { self.state = MultiPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(a) > (0) && ((b) > (0)) && ((b) < (a))" }); }
                  self.state = MultiPayloadState::S1 { a, b };
                  Ok(())
              }
              (MultiPayloadState::S1 { a, b }, Action::Resp { dir: Direction::Recv, d, .. }) => {
                  let a = *a;
                  let b = *b;
                  let d = *d;
                  if !((d) == ((a) - (b))) { self.state = MultiPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(d) == ((a) - (b))" }); }
                  self.state = MultiPayloadState::S2 { a, b, d };
                  Ok(())
              }
              _ => { self.state = MultiPayloadState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

  $ nuscr --gencode-rust=S@MultiPayload MultiPayload.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum MultiPayloadState {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor { state: MultiPayloadState }
  
  #[allow(unused_variables)]
  impl MultiPayloadMonitor {
      pub fn new() -> Self {
          Self { state: MultiPayloadState::S0 }
      }
  
      pub const NAME: &'static str = "MultiPayload";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Req { dir: Direction::Recv, a, b, .. } => {
                  let a = *a;
                  let b = *b;
                  ((a) > (0)) && (((b) > (0)) && ((b) < (a)))
              }
              Action::Resp { dir: Direction::Send, d, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (MultiPayloadState::Error, _) => Err(Violation::AlreadyFailed),
              (MultiPayloadState::S0, Action::Req { dir: Direction::Recv, a, b, .. }) => {
                  let a = *a;
                  let b = *b;
                  if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { self.state = MultiPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(a) > (0) && ((b) > (0)) && ((b) < (a))" }); }
                  self.state = MultiPayloadState::S1 { a, b };
                  Ok(())
              }
              (MultiPayloadState::S1 { a, b }, Action::Resp { dir: Direction::Send, d, .. }) => {
                  let a = *a;
                  let b = *b;
                  let d = *d;
                  if !((d) == ((a) - (b))) { self.state = MultiPayloadState::Error; return Err(Violation::ConstraintFailed { expr: "(d) == ((a) - (b))" }); }
                  self.state = MultiPayloadState::S2 { a, b, d };
                  Ok(())
              }
              _ => { self.state = MultiPayloadState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  
