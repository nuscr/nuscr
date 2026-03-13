Generate Rust monitor for Client (budget: rec var in send guard, subtraction update)
  $ nuscr --gencode-rust=C@Budget Budget.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #![allow(unused_variables)]
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64 },
      S6 { budget: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Done,
      Ok,
      Spend,
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
      dir: Direction,
      label: Label,
      payloads: Vec<Value>,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor {
      state: State,
  }
  
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::S0 { budget }, Direction::Send, Label::Spend) =>
                  match action.payloads.as_slice() {
                      [Value::Int(amount)] => {
                          let amount = amount.clone();
                          if !(((amount) > (0)) && ((amount) <= (budget))) { return false; }
                          self.state = State::S3 { budget, amount };
                          true
                      }
                      _ => false
                  },
              (State::S0 { budget }, Direction::Send, Label::Done) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S5 { budget };
                          true
                      }
                      _ => false
                  },
              (State::S3 { budget, amount }, Direction::Recv, Label::Ok) =>
                  match action.payloads.as_slice() {
                      [] => {
                          let new_budget = (budget) - (amount);
                          if !((new_budget) >= (0)) { return false; }
                          self.state = State::S0 { budget: new_budget };
                          true
                      }
                      _ => false
                  },
              (State::S5 { budget }, Direction::Recv, Label::Done) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S6 { budget };
                          true
                      }
                      _ => false
                  },
              _ => false
          }
      }
  }
  

Generate Rust monitor for Server (budget: rec var in send guard, subtraction update)
  $ nuscr --gencode-rust=S@Budget Budget.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #![allow(unused_variables)]
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64 },
      S6 { budget: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Done,
      Ok,
      Spend,
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
      dir: Direction,
      label: Label,
      payloads: Vec<Value>,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor {
      state: State,
  }
  
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state.clone(), &action.dir, &action.label) {
              (State::S0 { budget }, Direction::Recv, Label::Spend) =>
                  match action.payloads.as_slice() {
                      [Value::Int(amount)] => {
                          let amount = amount.clone();
                          if !(((amount) > (0)) && ((amount) <= (budget))) { return false; }
                          self.state = State::S3 { budget, amount };
                          true
                      }
                      _ => false
                  },
              (State::S0 { budget }, Direction::Recv, Label::Done) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S5 { budget };
                          true
                      }
                      _ => false
                  },
              (State::S3 { budget, amount }, Direction::Send, Label::Ok) =>
                  match action.payloads.as_slice() {
                      [] => {
                          let new_budget = (budget) - (amount);
                          if !((new_budget) >= (0)) { return false; }
                          self.state = State::S0 { budget: new_budget };
                          true
                      }
                      _ => false
                  },
              (State::S5 { budget }, Direction::Send, Label::Done) =>
                  match action.payloads.as_slice() {
                      [] => {
                          self.state = State::S6 { budget };
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
