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
  
  pub trait Monitor {
      fn new() -> Self;
      fn accepts(&self, action: &Action) -> bool;
      fn step(&mut self, action: &Action) -> bool;
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor { state: State }
  
  #[allow(unused_variables)]
  impl Monitor for MultiPayloadMonitor {
      fn new() -> Self {
          Self { state: State::S0 }
      }
  
      fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Resp { dir: Direction::Recv, d, .. } => true,
              Action::Req { dir: Direction::Send, a, b, .. } => {
                  let a = a.clone();
                  let b = b.clone();
                  ((a) > (0)) && (((b) > (0)) && ((b) < (a)))
              }
              _ => false,
          }
      }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0, Action::Req { dir: Direction::Send, a, b, .. }) => {
                  let a = a.clone();
                  let b = b.clone();
                  if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { self.state = State::Error; return false; }
                  self.state = State::S1 { a, b };
                  true
              }
              (State::S1 { a, b }, Action::Resp { dir: Direction::Recv, d, .. }) => {
                  let a = a.clone();
                  let b = b.clone();
                  let d = d.clone();
                  if !((d) == ((a) - (b))) { self.state = State::Error; return false; }
                  self.state = State::S2 { a, b, d };
                  true
              }
              _ => { self.state = State::Error; false }
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
  
  pub trait Monitor {
      fn new() -> Self;
      fn accepts(&self, action: &Action) -> bool;
      fn step(&mut self, action: &Action) -> bool;
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor { state: State }
  
  #[allow(unused_variables)]
  impl Monitor for MultiPayloadMonitor {
      fn new() -> Self {
          Self { state: State::S0 }
      }
  
      fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Req { dir: Direction::Recv, a, b, .. } => {
                  let a = a.clone();
                  let b = b.clone();
                  ((a) > (0)) && (((b) > (0)) && ((b) < (a)))
              }
              Action::Resp { dir: Direction::Send, d, .. } => true,
              _ => false,
          }
      }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0, Action::Req { dir: Direction::Recv, a, b, .. }) => {
                  let a = a.clone();
                  let b = b.clone();
                  if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { self.state = State::Error; return false; }
                  self.state = State::S1 { a, b };
                  true
              }
              (State::S1 { a, b }, Action::Resp { dir: Direction::Send, d, .. }) => {
                  let a = a.clone();
                  let b = b.clone();
                  let d = d.clone();
                  if !((d) == ((a) - (b))) { self.state = State::Error; return false; }
                  self.state = State::S2 { a, b, d };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
