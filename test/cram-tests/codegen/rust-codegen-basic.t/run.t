Generate Rust monitor for Client
  $ nuscr --gencode-rust-test=C@Adder Adder.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Add { dir: Direction },
      Bye { dir: Direction },
      Sum { dir: Direction },
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
  enum AdderState {
      S0,
      S3,
      S4,
      S6,
      S7,
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct AdderMonitor { state: AdderState }
  
  #[allow(unused_variables)]
  impl AdderMonitor {
      pub fn new() -> Self {
          Self { state: AdderState::S0 }
      }
  
      pub fn name(&self) -> &'static str {
          "Adder"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Sum { dir: Direction::Recv, .. } => true,
              Action::Add { dir: Direction::Send, .. } => true,
              Action::Bye { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (AdderState::Error, _) => Ok(Outcome::Absorbed),
              (AdderState::S0, Action::Add { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S3;
                  Ok(Outcome::Transition)
              }
              (AdderState::S0, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S6;
                  Ok(Outcome::Transition)
              }
              (AdderState::S3, Action::Add { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S4;
                  Ok(Outcome::Transition)
              }
              (AdderState::S4, Action::Sum { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S0;
                  Ok(Outcome::Transition)
              }
              (AdderState::S6, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S7;
                  Ok(Outcome::Transition)
              }
              _ => { self.state = AdderState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  

Generate Rust monitor for Server
  $ nuscr --gencode-rust-test=S@Adder Adder.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Add { dir: Direction },
      Bye { dir: Direction },
      Sum { dir: Direction },
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
  enum AdderState {
      S0,
      S3,
      S4,
      S6,
      S7,
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct AdderMonitor { state: AdderState }
  
  #[allow(unused_variables)]
  impl AdderMonitor {
      pub fn new() -> Self {
          Self { state: AdderState::S0 }
      }
  
      pub fn name(&self) -> &'static str {
          "Adder"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Add { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, .. } => true,
              Action::Sum { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (AdderState::Error, _) => Ok(Outcome::Absorbed),
              (AdderState::S0, Action::Add { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S3;
                  Ok(Outcome::Transition)
              }
              (AdderState::S0, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S6;
                  Ok(Outcome::Transition)
              }
              (AdderState::S3, Action::Add { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S4;
                  Ok(Outcome::Transition)
              }
              (AdderState::S4, Action::Sum { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S0;
                  Ok(Outcome::Transition)
              }
              (AdderState::S6, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S7;
                  Ok(Outcome::Transition)
              }
              _ => { self.state = AdderState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen (no support types, not compiled)
  $ nuscr --gencode-rust=C@Adder Adder.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum AdderState {
      S0,
      S3,
      S4,
      S6,
      S7,
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct AdderMonitor { state: AdderState }
  
  #[allow(unused_variables)]
  impl AdderMonitor {
      pub fn new() -> Self {
          Self { state: AdderState::S0 }
      }
  
      pub fn name(&self) -> &'static str {
          "Adder"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Sum { dir: Direction::Recv, .. } => true,
              Action::Add { dir: Direction::Send, .. } => true,
              Action::Bye { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (AdderState::Error, _) => Ok(Outcome::Absorbed),
              (AdderState::S0, Action::Add { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S3;
                  Ok(Outcome::Transition)
              }
              (AdderState::S0, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S6;
                  Ok(Outcome::Transition)
              }
              (AdderState::S3, Action::Add { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S4;
                  Ok(Outcome::Transition)
              }
              (AdderState::S4, Action::Sum { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S0;
                  Ok(Outcome::Transition)
              }
              (AdderState::S6, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S7;
                  Ok(Outcome::Transition)
              }
              _ => { self.state = AdderState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  

  $ nuscr --gencode-rust=S@Adder Adder.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum AdderState {
      S0,
      S3,
      S4,
      S6,
      S7,
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct AdderMonitor { state: AdderState }
  
  #[allow(unused_variables)]
  impl AdderMonitor {
      pub fn new() -> Self {
          Self { state: AdderState::S0 }
      }
  
      pub fn name(&self) -> &'static str {
          "Adder"
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Add { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Recv, .. } => true,
              Action::Bye { dir: Direction::Send, .. } => true,
              Action::Sum { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> Result<Outcome, Violation> {
          match (&self.state, action) {
              (AdderState::Error, _) => Ok(Outcome::Absorbed),
              (AdderState::S0, Action::Add { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S3;
                  Ok(Outcome::Transition)
              }
              (AdderState::S0, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S6;
                  Ok(Outcome::Transition)
              }
              (AdderState::S3, Action::Add { dir: Direction::Recv, .. }) => {
                  self.state = AdderState::S4;
                  Ok(Outcome::Transition)
              }
              (AdderState::S4, Action::Sum { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S0;
                  Ok(Outcome::Transition)
              }
              (AdderState::S6, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = AdderState::S7;
                  Ok(Outcome::Transition)
              }
              _ => { self.state = AdderState::Error; Err(Violation { reason: "no matching transition" }) }
          }
      }
  }
  
