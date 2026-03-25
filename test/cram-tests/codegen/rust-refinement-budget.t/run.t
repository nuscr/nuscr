Generate Rust monitor for Client (budget: rec var in send guard, subtraction update)
  $ nuscr --gencode-rust-test=C@Budget Budget.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Bye { dir: Direction, x: i64 },
      Ok { dir: Direction },
      Spend { dir: Direction, amount: i64 },
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64, x: i64 },
      S6 { budget: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: State }
  
  #[allow(unused_variables)]
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ok { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, x, .. } => {
                  let x = x.clone();
                  let x_ = x.clone();
                  (x) > (0) || (x_) == (0)
              }
              Action::Spend { dir: Direction::Send, amount, .. } => {
                  let amount = amount.clone();
                  (amount) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { budget }, Action::Spend { dir: Direction::Send, amount, .. }) => {
                  let budget = budget.clone();
                  let amount = amount.clone();
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = State::Error; return false; }
                  self.state = State::S3 { budget, amount };
                  true
              }
              (State::S0 { budget }, Action::Bye { dir: Direction::Send, x, .. }) => {
                  let budget = budget.clone();
                  let x = x.clone();
                  if !((x) > (0)) { self.state = State::Error; return false; }
                  self.state = State::S5 { budget, x };
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
              (State::S5 { budget, x }, Action::Bye { dir: Direction::Send, x: x_, .. }) => {
                  let budget = budget.clone();
                  let x = x.clone();
                  let x_ = x_.clone();
                  if !((x_) == (0)) { self.state = State::Error; return false; }
                  self.state = State::S6 { budget, x, x_ };
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
      Bye { dir: Direction, x: i64 },
      Ok { dir: Direction },
      Spend { dir: Direction, amount: i64 },
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64, x: i64 },
      S6 { budget: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: State }
  
  #[allow(unused_variables)]
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, x, .. } => {
                  let x = x.clone();
                  let x_ = x.clone();
                  (x) > (0) || (x_) == (0)
              }
              Action::Spend { dir: Direction::Recv, amount, .. } => {
                  let amount = amount.clone();
                  (amount) > (0)
              }
              Action::Ok { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { budget }, Action::Spend { dir: Direction::Recv, amount, .. }) => {
                  let budget = budget.clone();
                  let amount = amount.clone();
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = State::Error; return false; }
                  self.state = State::S3 { budget, amount };
                  true
              }
              (State::S0 { budget }, Action::Bye { dir: Direction::Recv, x, .. }) => {
                  let budget = budget.clone();
                  let x = x.clone();
                  if !((x) > (0)) { self.state = State::Error; return false; }
                  self.state = State::S5 { budget, x };
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
              (State::S5 { budget, x }, Action::Bye { dir: Direction::Recv, x: x_, .. }) => {
                  let budget = budget.clone();
                  let x = x.clone();
                  let x_ = x_.clone();
                  if !((x_) == (0)) { self.state = State::Error; return false; }
                  self.state = State::S6 { budget, x, x_ };
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

Production codegen (no support types, not compiled)
  $ nuscr --gencode-rust=C@Budget Budget.nuscr
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64, x: i64 },
      S6 { budget: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: State }
  
  #[allow(unused_variables)]
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ok { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, x, .. } => {
                  let x = x.clone();
                  let x_ = x.clone();
                  (x) > (0) || (x_) == (0)
              }
              Action::Spend { dir: Direction::Send, amount, .. } => {
                  let amount = amount.clone();
                  (amount) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { budget }, Action::Spend { dir: Direction::Send, amount, .. }) => {
                  let budget = budget.clone();
                  let amount = amount.clone();
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = State::Error; return false; }
                  self.state = State::S3 { budget, amount };
                  true
              }
              (State::S0 { budget }, Action::Bye { dir: Direction::Send, x, .. }) => {
                  let budget = budget.clone();
                  let x = x.clone();
                  if !((x) > (0)) { self.state = State::Error; return false; }
                  self.state = State::S5 { budget, x };
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
              (State::S5 { budget, x }, Action::Bye { dir: Direction::Send, x: x_, .. }) => {
                  let budget = budget.clone();
                  let x = x.clone();
                  let x_ = x_.clone();
                  if !((x_) == (0)) { self.state = State::Error; return false; }
                  self.state = State::S6 { budget, x, x_ };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

  $ nuscr --gencode-rust=S@Budget Budget.nuscr
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64, x: i64 },
      S6 { budget: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: State }
  
  #[allow(unused_variables)]
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, x, .. } => {
                  let x = x.clone();
                  let x_ = x.clone();
                  (x) > (0) || (x_) == (0)
              }
              Action::Spend { dir: Direction::Recv, amount, .. } => {
                  let amount = amount.clone();
                  (amount) > (0)
              }
              Action::Ok { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { budget }, Action::Spend { dir: Direction::Recv, amount, .. }) => {
                  let budget = budget.clone();
                  let amount = amount.clone();
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = State::Error; return false; }
                  self.state = State::S3 { budget, amount };
                  true
              }
              (State::S0 { budget }, Action::Bye { dir: Direction::Recv, x, .. }) => {
                  let budget = budget.clone();
                  let x = x.clone();
                  if !((x) > (0)) { self.state = State::Error; return false; }
                  self.state = State::S5 { budget, x };
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
              (State::S5 { budget, x }, Action::Bye { dir: Direction::Recv, x: x_, .. }) => {
                  let budget = budget.clone();
                  let x = x.clone();
                  let x_ = x_.clone();
                  if !((x_) == (0)) { self.state = State::Error; return false; }
                  self.state = State::S6 { budget, x, x_ };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  
