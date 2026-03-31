Underscored payload variable feeding rec var update in choice branch

  $ nuscr --gencode-rust-test=C@RecUpdate RecUpdate.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Err { dir: Direction },
      Ok { dir: Direction, x: i64 },
      Req { dir: Direction, x: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Violation {
      ConstraintFailed { expr: &'static str },
      NoMatchingTransition,
      AlreadyFailed,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RecUpdateState {
      S0 { total: i64 },
      S2 { total: i64, x: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RecUpdateMonitor { state: RecUpdateState }
  
  #[allow(unused_variables)]
  impl RecUpdateMonitor {
      pub fn new() -> Self {
          Self { state: RecUpdateState::S0 { total: 0 } }
      }
  
      pub const NAME: &'static str = "RecUpdate";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Err { dir: Direction::Recv, .. } => true,
              Action::Ok { dir: Direction::Recv, x, .. } => true,
              Action::Req { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RecUpdateState::Error, _) => Err(Violation::AlreadyFailed),
              (RecUpdateState::S0 { total }, Action::Req { dir: Direction::Send, x, .. }) => {
                  let total = *total;
                  let x = *x;
                  if !((x) > (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0)" }); }
                  self.state = RecUpdateState::S2 { total, x };
                  Ok(())
              }
              (RecUpdateState::S2 { total, x }, Action::Err { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  let x = *x;
                  let new_total = total;
                  if !((new_total) >= (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(total) >= (0)" }); }
                  self.state = RecUpdateState::S0 { total: new_total };
                  Ok(())
              }
              (RecUpdateState::S2 { total, x }, Action::Ok { dir: Direction::Recv, x: x_, .. }) => {
                  let total = *total;
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) == (x)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) == (x)" }); }
                  let new_total = (total) + (x_);
                  if !((new_total) >= (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(total) >= (0)" }); }
                  self.state = RecUpdateState::S0 { total: new_total };
                  Ok(())
              }
              _ => { self.state = RecUpdateState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

  $ nuscr --gencode-rust-test=S@RecUpdate RecUpdate.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Err { dir: Direction },
      Ok { dir: Direction, x: i64 },
      Req { dir: Direction, x: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Violation {
      ConstraintFailed { expr: &'static str },
      NoMatchingTransition,
      AlreadyFailed,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RecUpdateState {
      S0 { total: i64 },
      S2 { total: i64, x: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RecUpdateMonitor { state: RecUpdateState }
  
  #[allow(unused_variables)]
  impl RecUpdateMonitor {
      pub fn new() -> Self {
          Self { state: RecUpdateState::S0 { total: 0 } }
      }
  
      pub const NAME: &'static str = "RecUpdate";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Req { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              Action::Err { dir: Direction::Send, .. } => true,
              Action::Ok { dir: Direction::Send, x, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RecUpdateState::Error, _) => Err(Violation::AlreadyFailed),
              (RecUpdateState::S0 { total }, Action::Req { dir: Direction::Recv, x, .. }) => {
                  let total = *total;
                  let x = *x;
                  if !((x) > (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0)" }); }
                  self.state = RecUpdateState::S2 { total, x };
                  Ok(())
              }
              (RecUpdateState::S2 { total, x }, Action::Err { dir: Direction::Send, .. }) => {
                  let total = *total;
                  let x = *x;
                  let new_total = total;
                  if !((new_total) >= (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(total) >= (0)" }); }
                  self.state = RecUpdateState::S0 { total: new_total };
                  Ok(())
              }
              (RecUpdateState::S2 { total, x }, Action::Ok { dir: Direction::Send, x: x_, .. }) => {
                  let total = *total;
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) == (x)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) == (x)" }); }
                  let new_total = (total) + (x_);
                  if !((new_total) >= (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(total) >= (0)" }); }
                  self.state = RecUpdateState::S0 { total: new_total };
                  Ok(())
              }
              _ => { self.state = RecUpdateState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Compile both monitors
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen
  $ nuscr --gencode-rust=C@RecUpdate RecUpdate.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RecUpdateState {
      S0 { total: i64 },
      S2 { total: i64, x: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RecUpdateMonitor { state: RecUpdateState }
  
  #[allow(unused_variables)]
  impl RecUpdateMonitor {
      pub fn new() -> Self {
          Self { state: RecUpdateState::S0 { total: 0 } }
      }
  
      pub const NAME: &'static str = "RecUpdate";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Err { dir: Direction::Recv, .. } => true,
              Action::Ok { dir: Direction::Recv, x, .. } => true,
              Action::Req { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RecUpdateState::Error, _) => Err(Violation::AlreadyFailed),
              (RecUpdateState::S0 { total }, Action::Req { dir: Direction::Send, x, .. }) => {
                  let total = *total;
                  let x = *x;
                  if !((x) > (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0)" }); }
                  self.state = RecUpdateState::S2 { total, x };
                  Ok(())
              }
              (RecUpdateState::S2 { total, x }, Action::Err { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  let x = *x;
                  let new_total = total;
                  if !((new_total) >= (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(total) >= (0)" }); }
                  self.state = RecUpdateState::S0 { total: new_total };
                  Ok(())
              }
              (RecUpdateState::S2 { total, x }, Action::Ok { dir: Direction::Recv, x: x_, .. }) => {
                  let total = *total;
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) == (x)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) == (x)" }); }
                  let new_total = (total) + (x_);
                  if !((new_total) >= (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(total) >= (0)" }); }
                  self.state = RecUpdateState::S0 { total: new_total };
                  Ok(())
              }
              _ => { self.state = RecUpdateState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  
  $ nuscr --gencode-rust=S@RecUpdate RecUpdate.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RecUpdateState {
      S0 { total: i64 },
      S2 { total: i64, x: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RecUpdateMonitor { state: RecUpdateState }
  
  #[allow(unused_variables)]
  impl RecUpdateMonitor {
      pub fn new() -> Self {
          Self { state: RecUpdateState::S0 { total: 0 } }
      }
  
      pub const NAME: &'static str = "RecUpdate";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Req { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              Action::Err { dir: Direction::Send, .. } => true,
              Action::Ok { dir: Direction::Send, x, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RecUpdateState::Error, _) => Err(Violation::AlreadyFailed),
              (RecUpdateState::S0 { total }, Action::Req { dir: Direction::Recv, x, .. }) => {
                  let total = *total;
                  let x = *x;
                  if !((x) > (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0)" }); }
                  self.state = RecUpdateState::S2 { total, x };
                  Ok(())
              }
              (RecUpdateState::S2 { total, x }, Action::Err { dir: Direction::Send, .. }) => {
                  let total = *total;
                  let x = *x;
                  let new_total = total;
                  if !((new_total) >= (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(total) >= (0)" }); }
                  self.state = RecUpdateState::S0 { total: new_total };
                  Ok(())
              }
              (RecUpdateState::S2 { total, x }, Action::Ok { dir: Direction::Send, x: x_, .. }) => {
                  let total = *total;
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) == (x)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) == (x)" }); }
                  let new_total = (total) + (x_);
                  if !((new_total) >= (0)) { self.state = RecUpdateState::Error; return Err(Violation::ConstraintFailed { expr: "(total) >= (0)" }); }
                  self.state = RecUpdateState::S0 { total: new_total };
                  Ok(())
              }
              _ => { self.state = RecUpdateState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  
