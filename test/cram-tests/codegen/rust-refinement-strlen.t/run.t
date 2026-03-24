Generate Rust monitor for Client (strlen: string type + len(), documents codegen gap)
  $ nuscr --gencode-rust=C@Strlen Strlen.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { token: String },
      S3 { token: String, tok2: String },
      S5 { token: String },
      S6 { token: String },
      Error,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Ack,
      Quit,
      Update,
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
  pub struct StrlenMonitor {
      state: State,
  }
  
  impl StrlenMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { token: "init".to_string() } }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::Error, _, _) => true,
              (State::S0 { token }, Direction::Send, Label::Update) =>
                  match action.payloads.as_slice() {
                      [Value::String(tok2)] => {
                          let tok2 = tok2.clone();
                          if !(((tok2).len() as i64) >= (4)) { self.state = State::Error; return false; }
                          self.state = State::S3 { token, tok2 };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S0 { token }, Direction::Send, Label::Quit) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S5 { token };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S3 { token, tok2 }, Direction::Recv, Label::Ack) =>
                  match action.payloads.as_slice() {
                      [] => {
                          let new_token = tok2;
                          if !(((new_token).len() as i64) >= (4)) { self.state = State::Error; return false; }
                          self.state = State::S0 { token: new_token };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S5 { token }, Direction::Recv, Label::Quit) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S6 { token };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server (strlen: string type + len(), documents codegen gap)
  $ nuscr --gencode-rust=S@Strlen Strlen.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { token: String },
      S3 { token: String, tok2: String },
      S5 { token: String },
      S6 { token: String },
      Error,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Ack,
      Quit,
      Update,
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
  pub struct StrlenMonitor {
      state: State,
  }
  
  impl StrlenMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { token: "init".to_string() } }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::Error, _, _) => true,
              (State::S0 { token }, Direction::Recv, Label::Update) =>
                  match action.payloads.as_slice() {
                      [Value::String(tok2)] => {
                          let tok2 = tok2.clone();
                          if !(((tok2).len() as i64) >= (4)) { self.state = State::Error; return false; }
                          self.state = State::S3 { token, tok2 };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S0 { token }, Direction::Recv, Label::Quit) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S5 { token };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S3 { token, tok2 }, Direction::Send, Label::Ack) =>
                  match action.payloads.as_slice() {
                      [] => {
                          let new_token = tok2;
                          if !(((new_token).len() as i64) >= (4)) { self.state = State::Error; return false; }
                          self.state = State::S0 { token: new_token };
                          true
                      }
                      _ => { self.state = State::Error; false }
                  },
              (State::S5 { token }, Direction::Send, Label::Quit) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S6 { token };
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
  warning: unused variable: `token`
    --> C_monitor.rs:70:26
     |
  70 |             (State::S3 { token, tok2 }, Direction::Recv, Label::Ack) =>
     |                          ^^^^^ help: try ignoring the field: `token: _`
     |
     = note: `#[warn(unused_variables)]` (part of `#[warn(unused)]`) on by default
  
  warning: 1 warning emitted
  

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
  warning: unused variable: `token`
    --> S_monitor.rs:70:26
     |
  70 |             (State::S3 { token, tok2 }, Direction::Send, Label::Ack) =>
     |                          ^^^^^ help: try ignoring the field: `token: _`
     |
     = note: `#[warn(unused_variables)]` (part of `#[warn(unused)]`) on by default
  
  warning: 1 warning emitted
  
