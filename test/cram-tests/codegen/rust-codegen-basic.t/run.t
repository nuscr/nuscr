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
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct AdderMonitor { state: State }
  
  #[allow(unused_variables)]
  impl AdderMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 }
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
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0, Action::Add { dir: Direction::Send, .. }) => {
                  self.state = State::S3;
                  true
              }
              (State::S0, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = State::S6;
                  true
              }
              (State::S3, Action::Add { dir: Direction::Send, .. }) => {
                  self.state = State::S4;
                  true
              }
              (State::S4, Action::Sum { dir: Direction::Recv, .. }) => {
                  self.state = State::S0;
                  true
              }
              (State::S6, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = State::S7;
                  true
              }
              _ => { self.state = State::Error; false }
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
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct AdderMonitor { state: State }
  
  #[allow(unused_variables)]
  impl AdderMonitor {
      pub fn new() -> Self {
          Self { state: State::S0 }
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
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0, Action::Add { dir: Direction::Recv, .. }) => {
                  self.state = State::S3;
                  true
              }
              (State::S0, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = State::S6;
                  true
              }
              (State::S3, Action::Add { dir: Direction::Recv, .. }) => {
                  self.state = State::S4;
                  true
              }
              (State::S4, Action::Sum { dir: Direction::Send, .. }) => {
                  self.state = State::S0;
                  true
              }
              (State::S6, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = State::S7;
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
