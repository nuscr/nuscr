Generate Rust monitor for Client
  $ nuscr --gencode-rust-test=C@RunningSum RunningSum.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Add { dir: Direction, x: i64, y: i64 },
      Bye { dir: Direction, x: i64 },
      Sum { dir: Direction, r: i64 },
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
  enum RunningSumState {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64, x_: i64 },
      S6 { total: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Sum { dir: Direction::Recv, r, .. } => true,
              Action::Add { dir: Direction::Send, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x_) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RunningSumState::Error, _) => Err(Violation::AlreadyFailed),
              (RunningSumState::S0 { total }, Action::Add { dir: Direction::Send, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !((x) > (0) && (y) > (0)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0) && (y) > (0)" }); }
                  self.state = RunningSumState::S3 { total, x, y };
                  Ok(())
              }
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Send, x: x_, .. }) => {
                  let total = *total;
                  let x_ = *x_;
                  if !((x_) > (0)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) > (0)" }); }
                  self.state = RunningSumState::S5 { total, x_ };
                  Ok(())
              }
              (RunningSumState::S3 { total, x, y }, Action::Sum { dir: Direction::Recv, r, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let r = *r;
                  if !((r) == ((x) + (y))) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(r) == ((x) + (y))" }); }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                  self.state = RunningSumState::S0 { total: new_total };
                  Ok(())
              }
              (RunningSumState::S5 { total, x_ }, Action::Bye { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  let x_ = *x_;
                  self.state = RunningSumState::S6 { total, x_ };
                  Ok(())
              }
              _ => { self.state = RunningSumState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Generate Rust monitor for Server
  $ nuscr --gencode-rust-test=S@RunningSum RunningSum.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Add { dir: Direction, x: i64, y: i64 },
      Bye { dir: Direction, x: i64 },
      Sum { dir: Direction, r: i64 },
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
  enum RunningSumState {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64, x_: i64 },
      S6 { total: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Add { dir: Direction::Recv, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x_) > (0)
              }
              Action::Bye { dir: Direction::Send, .. } => true,
              Action::Sum { dir: Direction::Send, r, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RunningSumState::Error, _) => Err(Violation::AlreadyFailed),
              (RunningSumState::S0 { total }, Action::Add { dir: Direction::Recv, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !((x) > (0) && (y) > (0)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0) && (y) > (0)" }); }
                  self.state = RunningSumState::S3 { total, x, y };
                  Ok(())
              }
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Recv, x: x_, .. }) => {
                  let total = *total;
                  let x_ = *x_;
                  if !((x_) > (0)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) > (0)" }); }
                  self.state = RunningSumState::S5 { total, x_ };
                  Ok(())
              }
              (RunningSumState::S3 { total, x, y }, Action::Sum { dir: Direction::Send, r, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let r = *r;
                  if !((r) == ((x) + (y))) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(r) == ((x) + (y))" }); }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                  self.state = RunningSumState::S0 { total: new_total };
                  Ok(())
              }
              (RunningSumState::S5 { total, x_ }, Action::Bye { dir: Direction::Send, .. }) => {
                  let total = *total;
                  let x_ = *x_;
                  self.state = RunningSumState::S6 { total, x_ };
                  Ok(())
              }
              _ => { self.state = RunningSumState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen (no support types, not compiled)
  $ nuscr --gencode-rust=C@RunningSum RunningSum.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RunningSumState {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64, x_: i64 },
      S6 { total: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Sum { dir: Direction::Recv, r, .. } => true,
              Action::Add { dir: Direction::Send, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x_) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RunningSumState::Error, _) => Err(Violation::AlreadyFailed),
              (RunningSumState::S0 { total }, Action::Add { dir: Direction::Send, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !((x) > (0) && (y) > (0)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0) && (y) > (0)" }); }
                  self.state = RunningSumState::S3 { total, x, y };
                  Ok(())
              }
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Send, x: x_, .. }) => {
                  let total = *total;
                  let x_ = *x_;
                  if !((x_) > (0)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) > (0)" }); }
                  self.state = RunningSumState::S5 { total, x_ };
                  Ok(())
              }
              (RunningSumState::S3 { total, x, y }, Action::Sum { dir: Direction::Recv, r, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let r = *r;
                  if !((r) == ((x) + (y))) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(r) == ((x) + (y))" }); }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                  self.state = RunningSumState::S0 { total: new_total };
                  Ok(())
              }
              (RunningSumState::S5 { total, x_ }, Action::Bye { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  let x_ = *x_;
                  self.state = RunningSumState::S6 { total, x_ };
                  Ok(())
              }
              _ => { self.state = RunningSumState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

  $ nuscr --gencode-rust=S@RunningSum RunningSum.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RunningSumState {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64, x_: i64 },
      S6 { total: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Add { dir: Direction::Recv, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x_) > (0)
              }
              Action::Bye { dir: Direction::Send, .. } => true,
              Action::Sum { dir: Direction::Send, r, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RunningSumState::Error, _) => Err(Violation::AlreadyFailed),
              (RunningSumState::S0 { total }, Action::Add { dir: Direction::Recv, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !((x) > (0) && (y) > (0)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0) && (y) > (0)" }); }
                  self.state = RunningSumState::S3 { total, x, y };
                  Ok(())
              }
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Recv, x: x_, .. }) => {
                  let total = *total;
                  let x_ = *x_;
                  if !((x_) > (0)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) > (0)" }); }
                  self.state = RunningSumState::S5 { total, x_ };
                  Ok(())
              }
              (RunningSumState::S3 { total, x, y }, Action::Sum { dir: Direction::Send, r, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let r = *r;
                  if !((r) == ((x) + (y))) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(r) == ((x) + (y))" }); }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                  self.state = RunningSumState::S0 { total: new_total };
                  Ok(())
              }
              (RunningSumState::S5 { total, x_ }, Action::Bye { dir: Direction::Send, .. }) => {
                  let total = *total;
                  let x_ = *x_;
                  self.state = RunningSumState::S6 { total, x_ };
                  Ok(())
              }
              _ => { self.state = RunningSumState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

