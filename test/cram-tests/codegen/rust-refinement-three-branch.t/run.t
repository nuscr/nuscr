Generate Rust monitor for Client (three-branch choice)
  $ nuscr --gencode-rust-test=C@ThreeWay ThreeWay.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Ack { dir: Direction },
      Bye { dir: Direction, x: i64 },
      Low { dir: Direction, x: i64 },
      Mid { dir: Direction, x: i64 },
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
  enum ThreeWayState {
      S0 { n: i64 },
      S3 { n: i64, x: i64 },
      S5 { n: i64, x: i64 },
      S7 { n: i64, x: i64 },
      S8 { n: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct ThreeWayMonitor { state: ThreeWayState }
  
  #[allow(unused_variables)]
  impl ThreeWayMonitor {
      pub fn new() -> Self {
          Self { state: ThreeWayState::S0 { n: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ack { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, x, .. } => true,
              Action::Low { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) < (10)
              }
              Action::Mid { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  ((x) >= (10)) && ((x) < (100))
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (ThreeWayState::Error, _) => Err(Violation::AlreadyFailed),
              (ThreeWayState::S0 { n }, Action::Bye { dir: Direction::Send, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  self.state = ThreeWayState::S7 { n, x };
                  Ok(())
              }
              (ThreeWayState::S0 { n }, Action::Low { dir: Direction::Send, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  if !((x) < (10)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(x) < (10)" }); }
                  self.state = ThreeWayState::S3 { n, x };
                  Ok(())
              }
              (ThreeWayState::S0 { n }, Action::Mid { dir: Direction::Send, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  if !(((x) >= (10)) && ((x) < (100))) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "((x) >= (10)) && ((x) < (100))" }); }
                  self.state = ThreeWayState::S5 { n, x };
                  Ok(())
              }
              (ThreeWayState::S3 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let n = *n;
                  let x = *x;
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(n) >= (0)" }); }
                  self.state = ThreeWayState::S0 { n: new_n };
                  Ok(())
              }
              (ThreeWayState::S5 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let n = *n;
                  let x = *x;
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(n) >= (0)" }); }
                  self.state = ThreeWayState::S0 { n: new_n };
                  Ok(())
              }
              (ThreeWayState::S7 { n, x }, Action::Bye { dir: Direction::Send, x: x_, .. }) => {
                  let n = *n;
                  let x = *x;
                  let x_ = *x_;
                  self.state = ThreeWayState::S8 { n, x, x_ };
                  Ok(())
              }
              _ => { self.state = ThreeWayState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Generate Rust monitor for Server (three-branch choice)
  $ nuscr --gencode-rust-test=S@ThreeWay ThreeWay.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Ack { dir: Direction },
      Bye { dir: Direction, x: i64 },
      Low { dir: Direction, x: i64 },
      Mid { dir: Direction, x: i64 },
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
  enum ThreeWayState {
      S0 { n: i64 },
      S3 { n: i64, x: i64 },
      S5 { n: i64, x: i64 },
      S7 { n: i64, x: i64 },
      S8 { n: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct ThreeWayMonitor { state: ThreeWayState }
  
  #[allow(unused_variables)]
  impl ThreeWayMonitor {
      pub fn new() -> Self {
          Self { state: ThreeWayState::S0 { n: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, x, .. } => true,
              Action::Low { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) < (10)
              }
              Action::Mid { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  ((x) >= (10)) && ((x) < (100))
              }
              Action::Ack { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (ThreeWayState::Error, _) => Err(Violation::AlreadyFailed),
              (ThreeWayState::S0 { n }, Action::Bye { dir: Direction::Recv, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  self.state = ThreeWayState::S7 { n, x };
                  Ok(())
              }
              (ThreeWayState::S0 { n }, Action::Low { dir: Direction::Recv, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  if !((x) < (10)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(x) < (10)" }); }
                  self.state = ThreeWayState::S3 { n, x };
                  Ok(())
              }
              (ThreeWayState::S0 { n }, Action::Mid { dir: Direction::Recv, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  if !(((x) >= (10)) && ((x) < (100))) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "((x) >= (10)) && ((x) < (100))" }); }
                  self.state = ThreeWayState::S5 { n, x };
                  Ok(())
              }
              (ThreeWayState::S3 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
                  let n = *n;
                  let x = *x;
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(n) >= (0)" }); }
                  self.state = ThreeWayState::S0 { n: new_n };
                  Ok(())
              }
              (ThreeWayState::S5 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
                  let n = *n;
                  let x = *x;
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(n) >= (0)" }); }
                  self.state = ThreeWayState::S0 { n: new_n };
                  Ok(())
              }
              (ThreeWayState::S7 { n, x }, Action::Bye { dir: Direction::Recv, x: x_, .. }) => {
                  let n = *n;
                  let x = *x;
                  let x_ = *x_;
                  self.state = ThreeWayState::S8 { n, x, x_ };
                  Ok(())
              }
              _ => { self.state = ThreeWayState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen (no support types, not compiled)
  $ nuscr --gencode-rust=C@ThreeWay ThreeWay.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum ThreeWayState {
      S0 { n: i64 },
      S3 { n: i64, x: i64 },
      S5 { n: i64, x: i64 },
      S7 { n: i64, x: i64 },
      S8 { n: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct ThreeWayMonitor { state: ThreeWayState }
  
  #[allow(unused_variables)]
  impl ThreeWayMonitor {
      pub fn new() -> Self {
          Self { state: ThreeWayState::S0 { n: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ack { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, x, .. } => true,
              Action::Low { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) < (10)
              }
              Action::Mid { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  ((x) >= (10)) && ((x) < (100))
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (ThreeWayState::Error, _) => Err(Violation::AlreadyFailed),
              (ThreeWayState::S0 { n }, Action::Bye { dir: Direction::Send, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  self.state = ThreeWayState::S7 { n, x };
                  Ok(())
              }
              (ThreeWayState::S0 { n }, Action::Low { dir: Direction::Send, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  if !((x) < (10)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(x) < (10)" }); }
                  self.state = ThreeWayState::S3 { n, x };
                  Ok(())
              }
              (ThreeWayState::S0 { n }, Action::Mid { dir: Direction::Send, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  if !(((x) >= (10)) && ((x) < (100))) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "((x) >= (10)) && ((x) < (100))" }); }
                  self.state = ThreeWayState::S5 { n, x };
                  Ok(())
              }
              (ThreeWayState::S3 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let n = *n;
                  let x = *x;
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(n) >= (0)" }); }
                  self.state = ThreeWayState::S0 { n: new_n };
                  Ok(())
              }
              (ThreeWayState::S5 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let n = *n;
                  let x = *x;
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(n) >= (0)" }); }
                  self.state = ThreeWayState::S0 { n: new_n };
                  Ok(())
              }
              (ThreeWayState::S7 { n, x }, Action::Bye { dir: Direction::Send, x: x_, .. }) => {
                  let n = *n;
                  let x = *x;
                  let x_ = *x_;
                  self.state = ThreeWayState::S8 { n, x, x_ };
                  Ok(())
              }
              _ => { self.state = ThreeWayState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

  $ nuscr --gencode-rust=S@ThreeWay ThreeWay.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum ThreeWayState {
      S0 { n: i64 },
      S3 { n: i64, x: i64 },
      S5 { n: i64, x: i64 },
      S7 { n: i64, x: i64 },
      S8 { n: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct ThreeWayMonitor { state: ThreeWayState }
  
  #[allow(unused_variables)]
  impl ThreeWayMonitor {
      pub fn new() -> Self {
          Self { state: ThreeWayState::S0 { n: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, x, .. } => true,
              Action::Low { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) < (10)
              }
              Action::Mid { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  ((x) >= (10)) && ((x) < (100))
              }
              Action::Ack { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (ThreeWayState::Error, _) => Err(Violation::AlreadyFailed),
              (ThreeWayState::S0 { n }, Action::Bye { dir: Direction::Recv, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  self.state = ThreeWayState::S7 { n, x };
                  Ok(())
              }
              (ThreeWayState::S0 { n }, Action::Low { dir: Direction::Recv, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  if !((x) < (10)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(x) < (10)" }); }
                  self.state = ThreeWayState::S3 { n, x };
                  Ok(())
              }
              (ThreeWayState::S0 { n }, Action::Mid { dir: Direction::Recv, x, .. }) => {
                  let n = *n;
                  let x = *x;
                  if !(((x) >= (10)) && ((x) < (100))) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "((x) >= (10)) && ((x) < (100))" }); }
                  self.state = ThreeWayState::S5 { n, x };
                  Ok(())
              }
              (ThreeWayState::S3 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
                  let n = *n;
                  let x = *x;
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(n) >= (0)" }); }
                  self.state = ThreeWayState::S0 { n: new_n };
                  Ok(())
              }
              (ThreeWayState::S5 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
                  let n = *n;
                  let x = *x;
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = ThreeWayState::Error; return Err(Violation::ConstraintFailed { expr: "(n) >= (0)" }); }
                  self.state = ThreeWayState::S0 { n: new_n };
                  Ok(())
              }
              (ThreeWayState::S7 { n, x }, Action::Bye { dir: Direction::Recv, x: x_, .. }) => {
                  let n = *n;
                  let x = *x;
                  let x_ = *x_;
                  self.state = ThreeWayState::S8 { n, x, x_ };
                  Ok(())
              }
              _ => { self.state = ThreeWayState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  
