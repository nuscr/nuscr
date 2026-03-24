Generate Rust monitor for Client (three-branch choice)
  $ nuscr --gencode-rust=C@ThreeWay ThreeWay.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { n: i64 },
      S3 { n: i64, x: i64 },
      S5 { n: i64, x: i64 },
      S7 { n: i64 },
      S8 { n: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Ack,
      Bye,
      Low,
      Mid,
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
  pub struct ThreeWayMonitor {
      state: State,
  }
  
  impl ThreeWayMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { n: 0 } }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::Error, _, _) => true,
              (State::S0 { n }, Direction::Send, Label::Low) =>
                  match action.payloads.as_slice() {
                      [Value::Int(x)] => {
                          let x = x.clone();
                          if !((x) < (10)) { self.state = State::Error; return false; }
                          self.state = State::S3 { n, x };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S0 { n }, Direction::Send, Label::Mid) =>
                  match action.payloads.as_slice() {
                      [Value::Int(x)] => {
                          let x = x.clone();
                          if !(((x) >= (10)) && ((x) < (100))) { self.state = State::Error; return false; }
                          self.state = State::S5 { n, x };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S0 { n }, Direction::Send, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S7 { n };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S3 { n, x }, Direction::Recv, Label::Ack) =>
                  match action.payloads.as_slice() {
                      [] => {
                          let new_n = (n) + (1);
                          if !((new_n) >= (0)) { self.state = State::Error; return false; }
                          self.state = State::S0 { n: new_n };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S5 { n, x }, Direction::Recv, Label::Ack) =>
                  match action.payloads.as_slice() {
                      [] => {
                          let new_n = (n) + (1);
                          if !((new_n) >= (0)) { self.state = State::Error; return false; }
                          self.state = State::S0 { n: new_n };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S7 { n }, Direction::Recv, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S8 { n };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server (three-branch choice)
  $ nuscr --gencode-rust=S@ThreeWay ThreeWay.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { n: i64 },
      S3 { n: i64, x: i64 },
      S5 { n: i64, x: i64 },
      S7 { n: i64 },
      S8 { n: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Ack,
      Bye,
      Low,
      Mid,
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
  pub struct ThreeWayMonitor {
      state: State,
  }
  
  impl ThreeWayMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { n: 0 } }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::Error, _, _) => true,
              (State::S0 { n }, Direction::Recv, Label::Low) =>
                  match action.payloads.as_slice() {
                      [Value::Int(x)] => {
                          let x = x.clone();
                          if !((x) < (10)) { self.state = State::Error; return false; }
                          self.state = State::S3 { n, x };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S0 { n }, Direction::Recv, Label::Mid) =>
                  match action.payloads.as_slice() {
                      [Value::Int(x)] => {
                          let x = x.clone();
                          if !(((x) >= (10)) && ((x) < (100))) { self.state = State::Error; return false; }
                          self.state = State::S5 { n, x };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S0 { n }, Direction::Recv, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S7 { n };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S3 { n, x }, Direction::Send, Label::Ack) =>
                  match action.payloads.as_slice() {
                      [] => {
                          let new_n = (n) + (1);
                          if !((new_n) >= (0)) { self.state = State::Error; return false; }
                          self.state = State::S0 { n: new_n };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S5 { n, x }, Direction::Send, Label::Ack) =>
                  match action.payloads.as_slice() {
                      [] => {
                          let new_n = (n) + (1);
                          if !((new_n) >= (0)) { self.state = State::Error; return false; }
                          self.state = State::S0 { n: new_n };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S7 { n }, Direction::Send, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S8 { n };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib
  warning: unused variable: `x`
    --> C_monitor.rs:82:29
     |
  82 |             (State::S3 { n, x }, Direction::Recv, Label::Ack) =>
     |                             ^ help: try ignoring the field: `x: _`
     |
     = note: `#[warn(unused_variables)]` (part of `#[warn(unused)]`) on by default
  
  warning: unused variable: `x`
    --> C_monitor.rs:92:29
     |
  92 |             (State::S5 { n, x }, Direction::Recv, Label::Ack) =>
     |                             ^ help: try ignoring the field: `x: _`
  
  warning: 2 warnings emitted
  

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
  warning: unused variable: `x`
    --> S_monitor.rs:82:29
     |
  82 |             (State::S3 { n, x }, Direction::Send, Label::Ack) =>
     |                             ^ help: try ignoring the field: `x: _`
     |
     = note: `#[warn(unused_variables)]` (part of `#[warn(unused)]`) on by default
  
  warning: unused variable: `x`
    --> S_monitor.rs:92:29
     |
  92 |             (State::S5 { n, x }, Direction::Send, Label::Ack) =>
     |                             ^ help: try ignoring the field: `x: _`
  
  warning: 2 warnings emitted
  
