Three levels of trailing-underscore disambiguation (x, x_, x__)

  $ nuscr --gencode-rust-test=C@DeepNest DeepNest.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Ping { dir: Direction, x: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Outcome {
      Transition,
      Absorbed,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Violation {
      pub reason: &'static str,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum DeepNestState {
      S0,
      S1 { x: i64 },
      S2 { x: i64, x_: i64 },
      S3 { x: i64, x_: i64, x__: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct DeepNestMonitor { state: DeepNestState }
  
  #[allow(unused_variables)]
  impl DeepNestMonitor {
      pub fn new() -> Self {
          Self { state: DeepNestState::S0 }
      }
  
      pub fn name(&self) -> &'static str {
          "DeepNest"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ping { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (DeepNestState::Error, _) => Ok(Outcome::Absorbed),
              (DeepNestState::S0, Action::Ping { dir: Direction::Send, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x) > (0)" }); }
                  self.state = DeepNestState::S1 { x };
                  Ok(Outcome::Transition)
              }
              (DeepNestState::S1 { x }, Action::Ping { dir: Direction::Send, x: x_, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) > (x)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x_) > (x)" }); }
                  self.state = DeepNestState::S2 { x, x_ };
                  Ok(Outcome::Transition)
              }
              (DeepNestState::S2 { x, x_ }, Action::Ping { dir: Direction::Send, x: x__, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  let x__ = *x__;
                  if !((x__) > (x_)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x__) > (x_)" }); }
                  self.state = DeepNestState::S3 { x, x_, x__ };
                  Ok(Outcome::Transition)
              }
              _ => { self.state = DeepNestState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  

  $ nuscr --gencode-rust-test=S@DeepNest DeepNest.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Ping { dir: Direction, x: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Outcome {
      Transition,
      Absorbed,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Violation {
      pub reason: &'static str,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum DeepNestState {
      S0,
      S1 { x: i64 },
      S2 { x: i64, x_: i64 },
      S3 { x: i64, x_: i64, x__: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct DeepNestMonitor { state: DeepNestState }
  
  #[allow(unused_variables)]
  impl DeepNestMonitor {
      pub fn new() -> Self {
          Self { state: DeepNestState::S0 }
      }
  
      pub fn name(&self) -> &'static str {
          "DeepNest"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ping { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (DeepNestState::Error, _) => Ok(Outcome::Absorbed),
              (DeepNestState::S0, Action::Ping { dir: Direction::Recv, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x) > (0)" }); }
                  self.state = DeepNestState::S1 { x };
                  Ok(Outcome::Transition)
              }
              (DeepNestState::S1 { x }, Action::Ping { dir: Direction::Recv, x: x_, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) > (x)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x_) > (x)" }); }
                  self.state = DeepNestState::S2 { x, x_ };
                  Ok(Outcome::Transition)
              }
              (DeepNestState::S2 { x, x_ }, Action::Ping { dir: Direction::Recv, x: x__, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  let x__ = *x__;
                  if !((x__) > (x_)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x__) > (x_)" }); }
                  self.state = DeepNestState::S3 { x, x_, x__ };
                  Ok(Outcome::Transition)
              }
              _ => { self.state = DeepNestState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  

Compile both monitors
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen
  $ nuscr --gencode-rust=C@DeepNest DeepNest.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum DeepNestState {
      S0,
      S1 { x: i64 },
      S2 { x: i64, x_: i64 },
      S3 { x: i64, x_: i64, x__: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct DeepNestMonitor { state: DeepNestState }
  
  #[allow(unused_variables)]
  impl DeepNestMonitor {
      pub fn new() -> Self {
          Self { state: DeepNestState::S0 }
      }
  
      pub fn name(&self) -> &'static str {
          "DeepNest"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ping { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (DeepNestState::Error, _) => Ok(Outcome::Absorbed),
              (DeepNestState::S0, Action::Ping { dir: Direction::Send, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x) > (0)" }); }
                  self.state = DeepNestState::S1 { x };
                  Ok(Outcome::Transition)
              }
              (DeepNestState::S1 { x }, Action::Ping { dir: Direction::Send, x: x_, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) > (x)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x_) > (x)" }); }
                  self.state = DeepNestState::S2 { x, x_ };
                  Ok(Outcome::Transition)
              }
              (DeepNestState::S2 { x, x_ }, Action::Ping { dir: Direction::Send, x: x__, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  let x__ = *x__;
                  if !((x__) > (x_)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x__) > (x_)" }); }
                  self.state = DeepNestState::S3 { x, x_, x__ };
                  Ok(Outcome::Transition)
              }
              _ => { self.state = DeepNestState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  
  $ nuscr --gencode-rust=S@DeepNest DeepNest.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum DeepNestState {
      S0,
      S1 { x: i64 },
      S2 { x: i64, x_: i64 },
      S3 { x: i64, x_: i64, x__: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct DeepNestMonitor { state: DeepNestState }
  
  #[allow(unused_variables)]
  impl DeepNestMonitor {
      pub fn new() -> Self {
          Self { state: DeepNestState::S0 }
      }
  
      pub fn name(&self) -> &'static str {
          "DeepNest"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ping { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (DeepNestState::Error, _) => Ok(Outcome::Absorbed),
              (DeepNestState::S0, Action::Ping { dir: Direction::Recv, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x) > (0)" }); }
                  self.state = DeepNestState::S1 { x };
                  Ok(Outcome::Transition)
              }
              (DeepNestState::S1 { x }, Action::Ping { dir: Direction::Recv, x: x_, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) > (x)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x_) > (x)" }); }
                  self.state = DeepNestState::S2 { x, x_ };
                  Ok(Outcome::Transition)
              }
              (DeepNestState::S2 { x, x_ }, Action::Ping { dir: Direction::Recv, x: x__, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  let x__ = *x__;
                  if !((x__) > (x_)) { self.state = DeepNestState::Error; return Err(Violation { reason: "guard failed: (x__) > (x_)" }); }
                  self.state = DeepNestState::S3 { x, x_, x__ };
                  Ok(Outcome::Transition)
              }
              _ => { self.state = DeepNestState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  
