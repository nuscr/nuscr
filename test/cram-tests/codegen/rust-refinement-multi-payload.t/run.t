Generate Rust monitor for Client (multi payload, cross-payload reference)
  $ nuscr --gencode-rust=C@MultiPayload MultiPayload.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  enum State {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Req,
      Resp,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Direction {
      Send,
      Recv,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub enum Value {
      Int(i64),
      Bool(bool),
      String(String),
      Unit,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Action {
      pub dir: Direction,
      pub label: Label,
      pub payloads: Vec<Value>,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor {
      state: State,
  }
  
  impl MultiPayloadMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::S0, Direction::Send, Label::Req) =>
                  match action.payloads.as_slice() {
                      [Value::Int(a), Value::Int(b)] => {
                          let a = a.clone();
                          let b = b.clone();
                          if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { return false; }
                          self.state = State::S1 { a, b };
                          true
                      }
                      _ => false
                  },
              (State::S1 { a, b }, Direction::Recv, Label::Resp) =>
                  match action.payloads.as_slice() {
                      [Value::Int(d)] => {
                          let d = d.clone();
                          if !((d) == ((a) - (b))) { return false; }
                          self.state = State::S2 { a, b, d };
                          true
                      }
                      _ => false
                  },
              _ => false
          }
      }
  }
  

Generate Rust monitor for Server (nested arith, cross-payload reference)
  $ nuscr --gencode-rust=S@MultiPayload MultiPayload.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  enum State {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Req,
      Resp,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Direction {
      Send,
      Recv,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub enum Value {
      Int(i64),
      Bool(bool),
      String(String),
      Unit,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Action {
      pub dir: Direction,
      pub label: Label,
      pub payloads: Vec<Value>,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor {
      state: State,
  }
  
  impl MultiPayloadMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::S0, Direction::Recv, Label::Req) =>
                  match action.payloads.as_slice() {
                      [Value::Int(a), Value::Int(b)] => {
                          let a = a.clone();
                          let b = b.clone();
                          if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { return false; }
                          self.state = State::S1 { a, b };
                          true
                      }
                      _ => false
                  },
              (State::S1 { a, b }, Direction::Send, Label::Resp) =>
                  match action.payloads.as_slice() {
                      [Value::Int(d)] => {
                          let d = d.clone();
                          if !((d) == ((a) - (b))) { return false; }
                          self.state = State::S2 { a, b, d };
                          true
                      }
                      _ => false
                  },
              _ => false
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
