Generate Rust monitor for Client
  $ nuscr --gencode-rust-test=C@RunningSum RunningSum.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Add { dir: Direction, x: i64, y: i64 },
      Bye { dir: Direction },
      Sum { dir: Direction, r: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RunningSumState {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Sum { dir: Direction::Recv, r, .. } => true,
              Action::Add { dir: Direction::Send, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (RunningSumState::Error, _) => true,
              (RunningSumState::S0 { total }, Action::Add { dir: Direction::Send, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !((x) > (0) && (y) > (0)) { self.state = RunningSumState::Error; return false; }
                  self.state = RunningSumState::S3 { total, x, y };
                  true
              }
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Send, .. }) => {
                  let total = *total;
                  self.state = RunningSumState::S5 { total };
                  true
              }
              (RunningSumState::S3 { total, x, y }, Action::Sum { dir: Direction::Recv, r, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let r = *r;
                  if !((r) == ((x) + (y))) { self.state = RunningSumState::Error; return false; }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = RunningSumState::Error; return false; }
                  self.state = RunningSumState::S0 { total: new_total };
                  true
              }
              (RunningSumState::S5 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  self.state = RunningSumState::S6 { total };
                  true
              }
              _ => { self.state = RunningSumState::Error; false }
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
      Add { dir: Direction, x: i64, y: i64 },
      Bye { dir: Direction },
      Sum { dir: Direction, r: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RunningSumState {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Add { dir: Direction::Recv, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, .. } => true,
              Action::Sum { dir: Direction::Send, r, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (RunningSumState::Error, _) => true,
              (RunningSumState::S0 { total }, Action::Add { dir: Direction::Recv, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !((x) > (0) && (y) > (0)) { self.state = RunningSumState::Error; return false; }
                  self.state = RunningSumState::S3 { total, x, y };
                  true
              }
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  self.state = RunningSumState::S5 { total };
                  true
              }
              (RunningSumState::S3 { total, x, y }, Action::Sum { dir: Direction::Send, r, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let r = *r;
                  if !((r) == ((x) + (y))) { self.state = RunningSumState::Error; return false; }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = RunningSumState::Error; return false; }
                  self.state = RunningSumState::S0 { total: new_total };
                  true
              }
              (RunningSumState::S5 { total }, Action::Bye { dir: Direction::Send, .. }) => {
                  let total = *total;
                  self.state = RunningSumState::S6 { total };
                  true
              }
              _ => { self.state = RunningSumState::Error; false }
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
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Sum { dir: Direction::Recv, r, .. } => true,
              Action::Add { dir: Direction::Send, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (RunningSumState::Error, _) => true,
              (RunningSumState::S0 { total }, Action::Add { dir: Direction::Send, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !((x) > (0) && (y) > (0)) { self.state = RunningSumState::Error; return false; }
                  self.state = RunningSumState::S3 { total, x, y };
                  true
              }
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Send, .. }) => {
                  let total = *total;
                  self.state = RunningSumState::S5 { total };
                  true
              }
              (RunningSumState::S3 { total, x, y }, Action::Sum { dir: Direction::Recv, r, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let r = *r;
                  if !((r) == ((x) + (y))) { self.state = RunningSumState::Error; return false; }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = RunningSumState::Error; return false; }
                  self.state = RunningSumState::S0 { total: new_total };
                  true
              }
              (RunningSumState::S5 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  self.state = RunningSumState::S6 { total };
                  true
              }
              _ => { self.state = RunningSumState::Error; false }
          }
      }
  }
  

  $ nuscr --gencode-rust=S@RunningSum RunningSum.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum RunningSumState {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: RunningSumState }
  
  #[allow(unused_variables)]
  impl RunningSumMonitor {
      pub fn new() -> Self {
          Self { state: RunningSumState::S0 { total: 0 } }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Add { dir: Direction::Recv, x, y, .. } => {
                  let x = *x;
                  let y = *y;
                  ((x) > (0)) && ((y) > (0))
              }
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, .. } => true,
              Action::Sum { dir: Direction::Send, r, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (RunningSumState::Error, _) => true,
              (RunningSumState::S0 { total }, Action::Add { dir: Direction::Recv, x, y, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  if !((x) > (0) && (y) > (0)) { self.state = RunningSumState::Error; return false; }
                  self.state = RunningSumState::S3 { total, x, y };
                  true
              }
              (RunningSumState::S0 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
                  let total = *total;
                  self.state = RunningSumState::S5 { total };
                  true
              }
              (RunningSumState::S3 { total, x, y }, Action::Sum { dir: Direction::Send, r, .. }) => {
                  let total = *total;
                  let x = *x;
                  let y = *y;
                  let r = *r;
                  if !((r) == ((x) + (y))) { self.state = RunningSumState::Error; return false; }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = RunningSumState::Error; return false; }
                  self.state = RunningSumState::S0 { total: new_total };
                  true
              }
              (RunningSumState::S5 { total }, Action::Bye { dir: Direction::Send, .. }) => {
                  let total = *total;
                  self.state = RunningSumState::S6 { total };
                  true
              }
              _ => { self.state = RunningSumState::Error; false }
          }
      }
  }
  

