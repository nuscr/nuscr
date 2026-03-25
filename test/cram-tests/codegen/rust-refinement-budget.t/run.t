Generate Rust monitor for Client (budget: rec var in send guard, subtraction update)
  $ nuscr --gencode-rust-test=C@Budget Budget.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Done { dir: Direction },
      Ok { dir: Direction },
      Spend { dir: Direction, amount: i64 },
  }
  
  pub trait Monitor {
      fn new() -> Self;
      fn accepts(&self, action: &Action) -> bool;
      fn step(&mut self, action: &Action) -> bool;
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64 },
      S6 { budget: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: State }
  
  #[allow(unused_variables)]
  impl Monitor for BudgetMonitor {
      fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { budget }, Action::Spend { dir: Direction::Send, amount, .. }) => {
                  let budget = budget.clone();
                  let amount = amount.clone();
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = State::Error; return false; }
                  self.state = State::S3 { budget, amount };
                  true
              }
              (State::S0 { budget }, Action::Done { dir: Direction::Send, .. }) => {
                  let budget = budget.clone();
                  self.state = State::S5 { budget };
                  true
              }
              (State::S3 { budget, amount }, Action::Ok { dir: Direction::Recv, .. }) => {
                  let budget = budget.clone();
                  let amount = amount.clone();
                  let new_budget = (budget) - (amount);
                  if !((new_budget) >= (0)) { self.state = State::Error; return false; }
                  self.state = State::S0 { budget: new_budget };
                  true
              }
              (State::S5 { budget }, Action::Done { dir: Direction::Recv, .. }) => {
                  let budget = budget.clone();
                  self.state = State::S6 { budget };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server (budget: rec var in send guard, subtraction update)
  $ nuscr --gencode-rust-test=S@Budget Budget.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Done { dir: Direction },
      Ok { dir: Direction },
      Spend { dir: Direction, amount: i64 },
  }
  
  pub trait Monitor {
      fn new() -> Self;
      fn accepts(&self, action: &Action) -> bool;
      fn step(&mut self, action: &Action) -> bool;
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64 },
      S6 { budget: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: State }
  
  #[allow(unused_variables)]
  impl Monitor for BudgetMonitor {
      fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { budget }, Action::Spend { dir: Direction::Recv, amount, .. }) => {
                  let budget = budget.clone();
                  let amount = amount.clone();
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = State::Error; return false; }
                  self.state = State::S3 { budget, amount };
                  true
              }
              (State::S0 { budget }, Action::Done { dir: Direction::Recv, .. }) => {
                  let budget = budget.clone();
                  self.state = State::S5 { budget };
                  true
              }
              (State::S3 { budget, amount }, Action::Ok { dir: Direction::Send, .. }) => {
                  let budget = budget.clone();
                  let amount = amount.clone();
                  let new_budget = (budget) - (amount);
                  if !((new_budget) >= (0)) { self.state = State::Error; return false; }
                  self.state = State::S0 { budget: new_budget };
                  true
              }
              (State::S5 { budget }, Action::Done { dir: Direction::Send, .. }) => {
                  let budget = budget.clone();
                  self.state = State::S6 { budget };
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
