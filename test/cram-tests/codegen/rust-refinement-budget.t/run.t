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
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Outcome {
      Transition,
      Absorbed,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Violation {
      pub reason: &'static str,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum BudgetState {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64, x: i64 },
      S6 { budget: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: BudgetState }
  
  #[allow(unused_variables)]
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: BudgetState::S0 { budget: 1000 } }
      }
  
      pub fn name(&self) -> &'static str {
          "Budget"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ok { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x) > (0) || (x_) == (0)
              }
              Action::Spend { dir: Direction::Send, amount, .. } => {
                  let amount = *amount;
                  (amount) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (BudgetState::Error, _) => Ok(Outcome::Absorbed),
              (BudgetState::S0 { budget }, Action::Bye { dir: Direction::Send, x, .. }) => {
                  let budget = *budget;
                  let x = *x;
                  if !((x) > (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: (x) > (0)" }); }
                  self.state = BudgetState::S5 { budget, x };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S0 { budget }, Action::Spend { dir: Direction::Send, amount, .. }) => {
                  let budget = *budget;
                  let amount = *amount;
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: ((amount) > (0)) && ((amount) <= (budget))" }); }
                  self.state = BudgetState::S3 { budget, amount };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S3 { budget, amount }, Action::Ok { dir: Direction::Recv, .. }) => {
                  let budget = *budget;
                  let amount = *amount;
                  let new_budget = (budget) - (amount);
                  if !((new_budget) >= (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "refinement failed: (budget) >= (0)" }); }
                  self.state = BudgetState::S0 { budget: new_budget };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S5 { budget, x }, Action::Bye { dir: Direction::Send, x: x_, .. }) => {
                  let budget = *budget;
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) == (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: (x_) == (0)" }); }
                  self.state = BudgetState::S6 { budget, x, x_ };
                  Ok(Outcome::Transition)
              }
              _ => { self.state = BudgetState::Error; Err(Violation { reason: "no matching transition" }) }
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
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Outcome {
      Transition,
      Absorbed,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Violation {
      pub reason: &'static str,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum BudgetState {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64, x: i64 },
      S6 { budget: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: BudgetState }
  
  #[allow(unused_variables)]
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: BudgetState::S0 { budget: 1000 } }
      }
  
      pub fn name(&self) -> &'static str {
          "Budget"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x) > (0) || (x_) == (0)
              }
              Action::Spend { dir: Direction::Recv, amount, .. } => {
                  let amount = *amount;
                  (amount) > (0)
              }
              Action::Ok { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (BudgetState::Error, _) => Ok(Outcome::Absorbed),
              (BudgetState::S0 { budget }, Action::Bye { dir: Direction::Recv, x, .. }) => {
                  let budget = *budget;
                  let x = *x;
                  if !((x) > (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: (x) > (0)" }); }
                  self.state = BudgetState::S5 { budget, x };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S0 { budget }, Action::Spend { dir: Direction::Recv, amount, .. }) => {
                  let budget = *budget;
                  let amount = *amount;
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: ((amount) > (0)) && ((amount) <= (budget))" }); }
                  self.state = BudgetState::S3 { budget, amount };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S3 { budget, amount }, Action::Ok { dir: Direction::Send, .. }) => {
                  let budget = *budget;
                  let amount = *amount;
                  let new_budget = (budget) - (amount);
                  if !((new_budget) >= (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "refinement failed: (budget) >= (0)" }); }
                  self.state = BudgetState::S0 { budget: new_budget };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S5 { budget, x }, Action::Bye { dir: Direction::Recv, x: x_, .. }) => {
                  let budget = *budget;
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) == (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: (x_) == (0)" }); }
                  self.state = BudgetState::S6 { budget, x, x_ };
                  Ok(Outcome::Transition)
              }
              _ => { self.state = BudgetState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen (no support types, not compiled)
  $ nuscr --gencode-rust=C@Budget Budget.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum BudgetState {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64, x: i64 },
      S6 { budget: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: BudgetState }
  
  #[allow(unused_variables)]
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: BudgetState::S0 { budget: 1000 } }
      }
  
      pub fn name(&self) -> &'static str {
          "Budget"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ok { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x) > (0) || (x_) == (0)
              }
              Action::Spend { dir: Direction::Send, amount, .. } => {
                  let amount = *amount;
                  (amount) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (BudgetState::Error, _) => Ok(Outcome::Absorbed),
              (BudgetState::S0 { budget }, Action::Bye { dir: Direction::Send, x, .. }) => {
                  let budget = *budget;
                  let x = *x;
                  if !((x) > (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: (x) > (0)" }); }
                  self.state = BudgetState::S5 { budget, x };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S0 { budget }, Action::Spend { dir: Direction::Send, amount, .. }) => {
                  let budget = *budget;
                  let amount = *amount;
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: ((amount) > (0)) && ((amount) <= (budget))" }); }
                  self.state = BudgetState::S3 { budget, amount };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S3 { budget, amount }, Action::Ok { dir: Direction::Recv, .. }) => {
                  let budget = *budget;
                  let amount = *amount;
                  let new_budget = (budget) - (amount);
                  if !((new_budget) >= (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "refinement failed: (budget) >= (0)" }); }
                  self.state = BudgetState::S0 { budget: new_budget };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S5 { budget, x }, Action::Bye { dir: Direction::Send, x: x_, .. }) => {
                  let budget = *budget;
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) == (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: (x_) == (0)" }); }
                  self.state = BudgetState::S6 { budget, x, x_ };
                  Ok(Outcome::Transition)
              }
              _ => { self.state = BudgetState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  

  $ nuscr --gencode-rust=S@Budget Budget.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum BudgetState {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64, x: i64 },
      S6 { budget: i64, x: i64, x_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: BudgetState }
  
  #[allow(unused_variables)]
  impl BudgetMonitor {
      pub fn new() -> Self {
          Self { state: BudgetState::S0 { budget: 1000 } }
      }
  
      pub fn name(&self) -> &'static str {
          "Budget"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  let x_ = x;
                  (x) > (0) || (x_) == (0)
              }
              Action::Spend { dir: Direction::Recv, amount, .. } => {
                  let amount = *amount;
                  (amount) > (0)
              }
              Action::Ok { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (BudgetState::Error, _) => Ok(Outcome::Absorbed),
              (BudgetState::S0 { budget }, Action::Bye { dir: Direction::Recv, x, .. }) => {
                  let budget = *budget;
                  let x = *x;
                  if !((x) > (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: (x) > (0)" }); }
                  self.state = BudgetState::S5 { budget, x };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S0 { budget }, Action::Spend { dir: Direction::Recv, amount, .. }) => {
                  let budget = *budget;
                  let amount = *amount;
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: ((amount) > (0)) && ((amount) <= (budget))" }); }
                  self.state = BudgetState::S3 { budget, amount };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S3 { budget, amount }, Action::Ok { dir: Direction::Send, .. }) => {
                  let budget = *budget;
                  let amount = *amount;
                  let new_budget = (budget) - (amount);
                  if !((new_budget) >= (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "refinement failed: (budget) >= (0)" }); }
                  self.state = BudgetState::S0 { budget: new_budget };
                  Ok(Outcome::Transition)
              }
              (BudgetState::S5 { budget, x }, Action::Bye { dir: Direction::Recv, x: x_, .. }) => {
                  let budget = *budget;
                  let x = *x;
                  let x_ = *x_;
                  if !((x_) == (0)) { self.state = BudgetState::Error; return Err(Violation { reason: "guard failed: (x_) == (0)" }); }
                  self.state = BudgetState::S6 { budget, x, x_ };
                  Ok(Outcome::Transition)
              }
              _ => { self.state = BudgetState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  
