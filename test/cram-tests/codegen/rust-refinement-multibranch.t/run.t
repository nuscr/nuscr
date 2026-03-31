Generate Rust monitor for Client
  $ nuscr --gencode-rust-test=C@RunningSum RunningSum.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Bye { dir: Direction, x: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Violation {
      ConstraintFailed { expr: &'static str },
      NoMatchingTransition,
      AlreadyFailed,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RunningSumState {
      S0 { total: i64 },
      S4 { total: i64, x: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub const NAME: &'static str = "RunningSum";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) < (0) || (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RunningSumState::Error, _) => Err(Violation::AlreadyFailed),
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Send, x, .. }) => {
                  let total = *total;
                  let x = *x;
                  if (x) < (0) {
                      let new_total = (total) + (x);
                      if !((new_total) < (100)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                      self.state = RunningSumState::S0 { total: new_total };
                      Ok(())
                  } else if (x) > (0) {
                      self.state = RunningSumState::S4 { total, x };
                      Ok(())
                  } else {
                      self.state = RunningSumState::Error;
                      Err(Violation::ConstraintFailed { expr: "(x) < (0), (x) > (0)" })
                  }
              }
              _ => { self.state = RunningSumState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Generate Rust monitor for Server
  $ nuscr --gencode-rust-test=S@RunningSum RunningSum.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Bye { dir: Direction, x: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Violation {
      ConstraintFailed { expr: &'static str },
      NoMatchingTransition,
      AlreadyFailed,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RunningSumState {
      S0 { total: i64 },
      S4 { total: i64, x: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub const NAME: &'static str = "RunningSum";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) < (0) || (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RunningSumState::Error, _) => Err(Violation::AlreadyFailed),
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Recv, x, .. }) => {
                  let total = *total;
                  let x = *x;
                  if (x) < (0) {
                      let new_total = (total) + (x);
                      if !((new_total) < (100)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                      self.state = RunningSumState::S0 { total: new_total };
                      Ok(())
                  } else if (x) > (0) {
                      self.state = RunningSumState::S4 { total, x };
                      Ok(())
                  } else {
                      self.state = RunningSumState::Error;
                      Err(Violation::ConstraintFailed { expr: "(x) < (0), (x) > (0)" })
                  }
              }
              _ => { self.state = RunningSumState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen (no support types, not compiled)
  $ nuscr --gencode-rust=C@RunningSum RunningSum.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RunningSumState {
      S0 { total: i64 },
      S4 { total: i64, x: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub const NAME: &'static str = "RunningSum";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Send, x, .. } => {
                  let x = *x;
                  (x) < (0) || (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RunningSumState::Error, _) => Err(Violation::AlreadyFailed),
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Send, x, .. }) => {
                  let total = *total;
                  let x = *x;
                  if (x) < (0) {
                      let new_total = (total) + (x);
                      if !((new_total) < (100)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                      self.state = RunningSumState::S0 { total: new_total };
                      Ok(())
                  } else if (x) > (0) {
                      self.state = RunningSumState::S4 { total, x };
                      Ok(())
                  } else {
                      self.state = RunningSumState::Error;
                      Err(Violation::ConstraintFailed { expr: "(x) < (0), (x) > (0)" })
                  }
              }
              _ => { self.state = RunningSumState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

  $ nuscr --gencode-rust=S@RunningSum RunningSum.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RunningSumState {
      S0 { total: i64 },
      S4 { total: i64, x: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub const NAME: &'static str = "RunningSum";
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, x, .. } => {
                  let x = *x;
                  (x) < (0) || (x) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<(), Violation> {
          match (&self.state, action) {
              (RunningSumState::Error, _) => Err(Violation::AlreadyFailed),
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Recv, x, .. }) => {
                  let total = *total;
                  let x = *x;
                  if (x) < (0) {
                      let new_total = (total) + (x);
                      if !((new_total) < (100)) { self.state = RunningSumState::Error; return Err(Violation::ConstraintFailed { expr: "(total) < (100)" }); }
                      self.state = RunningSumState::S0 { total: new_total };
                      Ok(())
                  } else if (x) > (0) {
                      self.state = RunningSumState::S4 { total, x };
                      Ok(())
                  } else {
                      self.state = RunningSumState::Error;
                      Err(Violation::ConstraintFailed { expr: "(x) < (0), (x) > (0)" })
                  }
              }
              _ => { self.state = RunningSumState::Error; Err(Violation::NoMatchingTransition) }
          }
      }
  }
  

