Generate Rust monitor for Client
  $ nuscr --gencode-rust=C@RunningSum RunningSum.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  enum State {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
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
  pub struct RunningSumMonitor {
      state: State,
  }
  
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { total: 0 } }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::S0 { total }, Direction::Send, Label::Add) =>
                  match action.payloads.as_slice() {
                      [Value::Int(x), Value::Int(y)] => {
                          let x = x.clone();
                          let y = y.clone();
                          if !((x) > (0) && (y) > (0)) { return false; }
                          self.state = State::S3 { total, x, y };
                          true
                      }
                      _ => false
                  },
              (State::S0 { total }, Direction::Send, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S5 { total };
                          true
                      }
                      _ => false
                  },
              (State::S3 { total, x, y }, Direction::Recv, Label::Sum) =>
                  match action.payloads.as_slice() {
                      [Value::Int(r)] => {
                          let r = r.clone();
                          if !((r) == ((x) + (y))) { return false; }
                          let new_total = (total) + (r);
                          if !((new_total) < (100)) { return false; }
                          self.state = State::S0 { total: new_total };
                          true
                      }
                      _ => false
                  },
              (State::S5 { total }, Direction::Recv, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S6 { total };
                          true
                      }
                      _ => false
                  },
              _ => false
          }
      }
  }
  

Generate Rust monitor for Server
  $ nuscr --gencode-rust=S@RunningSum RunningSum.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  enum State {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
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
  pub struct RunningSumMonitor {
      state: State,
  }
  
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { total: 0 } }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::S0 { total }, Direction::Recv, Label::Add) =>
                  match action.payloads.as_slice() {
                      [Value::Int(x), Value::Int(y)] => {
                          let x = x.clone();
                          let y = y.clone();
                          if !((x) > (0) && (y) > (0)) { return false; }
                          self.state = State::S3 { total, x, y };
                          true
                      }
                      _ => false
                  },
              (State::S0 { total }, Direction::Recv, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S5 { total };
                          true
                      }
                      _ => false
                  },
              (State::S3 { total, x, y }, Direction::Send, Label::Sum) =>
                  match action.payloads.as_slice() {
                      [Value::Int(r)] => {
                          let r = r.clone();
                          if !((r) == ((x) + (y))) { return false; }
                          let new_total = (total) + (r);
                          if !((new_total) < (100)) { return false; }
                          self.state = State::S0 { total: new_total };
                          true
                      }
                      _ => false
                  },
              (State::S5 { total }, Direction::Send, Label::Bye) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S6 { total };
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

