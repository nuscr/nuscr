Underscored variable carried across loop iterations via rec var update

  $ nuscr --gencode-rust-test=C@RecCarry RecCarry.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Ack { dir: Direction },
      Init { dir: Direction, x: i64 },
      Step { dir: Direction, x: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Violation {
      ConstraintFailed { expr: &'static str },
      NoMatchingTransition,
      AlreadyFailed,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RecCarryState {
      S0,
      S1 { x: i64, acc: i64 },
      S3 { x: i64, acc: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RecCarryMonitor { state: RecCarryState }
  
  #[allow(unused_variables)]
  impl RecCarryMonitor {
      pub fn new() -> Self {
          Self { state: RecCarryState::S0 }
      }
  
      pub const NAME: &'static str = "RecCarry";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ack { dir: Direction::Recv, .. } => true,
              Action::Init { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              Action::Step { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x_) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RecCarryState::Error, _) => Err(Violation::AlreadyFailed),
              (RecCarryState::S0, Action::Init { dir: Direction::Send, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0)" }); }
                  self.state = RecCarryState::S1 { x, acc: x };
                  Ok(())
              }
              (RecCarryState::S1 { x, acc }, Action::Step { dir: Direction::Send, x: x_, .. }) => {
                  let x = *x;
                  let acc = *acc;
                  let x_ = *x_;
                  if !((x_) > (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) > (0)" }); }
                  self.state = RecCarryState::S3 { x, acc, x_ };
                  Ok(())
              }
              (RecCarryState::S3 { x, acc, x_ }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let x = *x;
                  let acc = *acc;
                  let x_ = *x_;
                  let new_acc = (acc) + (x_);
                  if !((new_acc) >= (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(acc) >= (0)" }); }
                  self.state = RecCarryState::S1 { x, acc: new_acc };
                  Ok(())
              }
              _ => { self.state = RecCarryState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

  $ nuscr --gencode-rust-test=S@RecCarry RecCarry.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Ack { dir: Direction },
      Init { dir: Direction, x: i64 },
      Step { dir: Direction, x: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Violation {
      ConstraintFailed { expr: &'static str },
      NoMatchingTransition,
      AlreadyFailed,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RecCarryState {
      S0,
      S1 { x: i64, acc: i64 },
      S3 { x: i64, acc: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RecCarryMonitor { state: RecCarryState }
  
  #[allow(unused_variables)]
  impl RecCarryMonitor {
      pub fn new() -> Self {
          Self { state: RecCarryState::S0 }
      }
  
      pub const NAME: &'static str = "RecCarry";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Init { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              Action::Step { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x_) > (0)
              }
              Action::Ack { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RecCarryState::Error, _) => Err(Violation::AlreadyFailed),
              (RecCarryState::S0, Action::Init { dir: Direction::Recv, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0)" }); }
                  self.state = RecCarryState::S1 { x, acc: x };
                  Ok(())
              }
              (RecCarryState::S1 { x, acc }, Action::Step { dir: Direction::Recv, x: x_, .. }) => {
                  let x = *x;
                  let acc = *acc;
                  let x_ = *x_;
                  if !((x_) > (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) > (0)" }); }
                  self.state = RecCarryState::S3 { x, acc, x_ };
                  Ok(())
              }
              (RecCarryState::S3 { x, acc, x_ }, Action::Ack { dir: Direction::Send, .. }) => {
                  let x = *x;
                  let acc = *acc;
                  let x_ = *x_;
                  let new_acc = (acc) + (x_);
                  if !((new_acc) >= (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(acc) >= (0)" }); }
                  self.state = RecCarryState::S1 { x, acc: new_acc };
                  Ok(())
              }
              _ => { self.state = RecCarryState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Compile both monitors
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen
  $ nuscr --gencode-rust=C@RecCarry RecCarry.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RecCarryState {
      S0,
      S1 { x: i64, acc: i64 },
      S3 { x: i64, acc: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RecCarryMonitor { state: RecCarryState }
  
  #[allow(unused_variables)]
  impl RecCarryMonitor {
      pub fn new() -> Self {
          Self { state: RecCarryState::S0 }
      }
  
      pub const NAME: &'static str = "RecCarry";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ack { dir: Direction::Recv, .. } => true,
              Action::Init { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              Action::Step { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x_) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RecCarryState::Error, _) => Err(Violation::AlreadyFailed),
              (RecCarryState::S0, Action::Init { dir: Direction::Send, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0)" }); }
                  self.state = RecCarryState::S1 { x, acc: x };
                  Ok(())
              }
              (RecCarryState::S1 { x, acc }, Action::Step { dir: Direction::Send, x: x_, .. }) => {
                  let x = *x;
                  let acc = *acc;
                  let x_ = *x_;
                  if !((x_) > (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) > (0)" }); }
                  self.state = RecCarryState::S3 { x, acc, x_ };
                  Ok(())
              }
              (RecCarryState::S3 { x, acc, x_ }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let x = *x;
                  let acc = *acc;
                  let x_ = *x_;
                  let new_acc = (acc) + (x_);
                  if !((new_acc) >= (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(acc) >= (0)" }); }
                  self.state = RecCarryState::S1 { x, acc: new_acc };
                  Ok(())
              }
              _ => { self.state = RecCarryState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  
  $ nuscr --gencode-rust=S@RecCarry RecCarry.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RecCarryState {
      S0,
      S1 { x: i64, acc: i64 },
      S3 { x: i64, acc: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RecCarryMonitor { state: RecCarryState }
  
  #[allow(unused_variables)]
  impl RecCarryMonitor {
      pub fn new() -> Self {
          Self { state: RecCarryState::S0 }
      }
  
      pub const NAME: &'static str = "RecCarry";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Init { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              Action::Step { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x_) > (0)
              }
              Action::Ack { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RecCarryState::Error, _) => Err(Violation::AlreadyFailed),
              (RecCarryState::S0, Action::Init { dir: Direction::Recv, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(x) > (0)" }); }
                  self.state = RecCarryState::S1 { x, acc: x };
                  Ok(())
              }
              (RecCarryState::S1 { x, acc }, Action::Step { dir: Direction::Recv, x: x_, .. }) => {
                  let x = *x;
                  let acc = *acc;
                  let x_ = *x_;
                  if !((x_) > (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(x_) > (0)" }); }
                  self.state = RecCarryState::S3 { x, acc, x_ };
                  Ok(())
              }
              (RecCarryState::S3 { x, acc, x_ }, Action::Ack { dir: Direction::Send, .. }) => {
                  let x = *x;
                  let acc = *acc;
                  let x_ = *x_;
                  let new_acc = (acc) + (x_);
                  if !((new_acc) >= (0)) { self.state = RecCarryState::Error; return Err(Violation::ConstraintFailed { expr: "(acc) >= (0)" }); }
                  self.state = RecCarryState::S1 { x, acc: new_acc };
                  Ok(())
              }
              _ => { self.state = RecCarryState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  
