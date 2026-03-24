Generate Rust monitor for Client
  $ nuscr --gencode-rust=C@Adder Adder.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0,
      S3,
      S4,
      S6,
      S7,
      Error,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Add,
      Bye,
      Sum,
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
  pub struct AdderMonitor {
      state: State,
  }
  
  impl AdderMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
                  (State::Error, _, _) => true,
              (State::S0, Direction::Send, Label::Add) =>
                  match action.payloads.as_slice() {
                      [Value::Int(_)] => {
                          self.state = State::S3;
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S0, Direction::Send, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S6;
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S3, Direction::Send, Label::Add) =>
                  match action.payloads.as_slice() {
                      [Value::Int(_)] => {
                          self.state = State::S4;
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S4, Direction::Recv, Label::Sum) =>
                  match action.payloads.as_slice() {
                      [Value::Int(_)] => {
                          self.state = State::S0;
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S6, Direction::Recv, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S7;
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server
  $ nuscr --gencode-rust=S@Adder Adder.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0,
      S3,
      S4,
      S6,
      S7,
      Error,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Add,
      Bye,
      Sum,
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
  pub struct AdderMonitor {
      state: State,
  }
  
  impl AdderMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
                  (State::Error, _, _) => true,
              (State::S0, Direction::Recv, Label::Add) =>
                  match action.payloads.as_slice() {
                      [Value::Int(_)] => {
                          self.state = State::S3;
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S0, Direction::Recv, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S6;
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S3, Direction::Recv, Label::Add) =>
                  match action.payloads.as_slice() {
                      [Value::Int(_)] => {
                          self.state = State::S4;
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S4, Direction::Send, Label::Sum) =>
                  match action.payloads.as_slice() {
                      [Value::Int(_)] => {
                          self.state = State::S0;
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S6, Direction::Send, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S7;
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

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
