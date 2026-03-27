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
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ping { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (DeepNestState::Error, _) => true,
              (DeepNestState::S0, Action::Ping { dir: Direction::Send, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S1 { x };
                  true
              }
              (DeepNestState::S1 { x }, Action::Ping { dir: Direction::Send, x: x_, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) > (x)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S2 { x, x_ };
                  true
              }
              (DeepNestState::S2 { x, x_ }, Action::Ping { dir: Direction::Send, x: x__, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  let x__ = *x__;
                  if !((x__) > (x_)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S3 { x, x_, x__ };
                  true
              }
              _ => { self.state = DeepNestState::Error; false }
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
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ping { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (DeepNestState::Error, _) => true,
              (DeepNestState::S0, Action::Ping { dir: Direction::Recv, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S1 { x };
                  true
              }
              (DeepNestState::S1 { x }, Action::Ping { dir: Direction::Recv, x: x_, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) > (x)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S2 { x, x_ };
                  true
              }
              (DeepNestState::S2 { x, x_ }, Action::Ping { dir: Direction::Recv, x: x__, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  let x__ = *x__;
                  if !((x__) > (x_)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S3 { x, x_, x__ };
                  true
              }
              _ => { self.state = DeepNestState::Error; false }
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
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ping { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (DeepNestState::Error, _) => true,
              (DeepNestState::S0, Action::Ping { dir: Direction::Send, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S1 { x };
                  true
              }
              (DeepNestState::S1 { x }, Action::Ping { dir: Direction::Send, x: x_, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) > (x)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S2 { x, x_ };
                  true
              }
              (DeepNestState::S2 { x, x_ }, Action::Ping { dir: Direction::Send, x: x__, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  let x__ = *x__;
                  if !((x__) > (x_)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S3 { x, x_, x__ };
                  true
              }
              _ => { self.state = DeepNestState::Error; false }
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
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ping { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (DeepNestState::Error, _) => true,
              (DeepNestState::S0, Action::Ping { dir: Direction::Recv, x, .. }) => {
                  let x = *x;
                  if !((x) > (0)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S1 { x };
                  true
              }
              (DeepNestState::S1 { x }, Action::Ping { dir: Direction::Recv, x: x_, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) > (x)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S2 { x, x_ };
                  true
              }
              (DeepNestState::S2 { x, x_ }, Action::Ping { dir: Direction::Recv, x: x__, .. }) => {
                  let x = *x;
                  let x_ = *x_;
                  let x__ = *x__;
                  if !((x__) > (x_)) { self.state = DeepNestState::Error; return false; }
                  self.state = DeepNestState::S3 { x, x_, x__ };
                  true
              }
              _ => { self.state = DeepNestState::Error; false }
          }
      }
  }
  
