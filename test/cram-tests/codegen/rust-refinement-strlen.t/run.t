Generate Rust monitor for Client (strlen: string type + len(), documents codegen gap)
  $ nuscr --gencode-rust-test=C@Strlen Strlen.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Ack { dir: Direction },
      Quit { dir: Direction },
      Update { dir: Direction, tok2: String },
  }
  
  pub trait Monitor {
      fn new() -> Self;
      fn accepts(&self, action: &Action) -> bool;
      fn step(&mut self, action: &Action) -> bool;
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { token: String },
      S3 { token: String, tok2: String },
      S5 { token: String },
      S6 { token: String },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct StrlenMonitor { state: State }
  
  #[allow(unused_variables)]
  impl Monitor for StrlenMonitor {
      fn new() -> Self {
          Self { state: State::S0 { token: "init".to_string() } }
      }
  
      fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Ack { dir: Direction::Recv, .. } => true,
              Action::Quit { dir: Direction::Recv, .. } => true,
              Action::Quit { dir: Direction::Send, .. } => true,
              Action::Update { dir: Direction::Send, tok2, .. } => {
                  let tok2 = tok2.clone();
                  ((tok2).len() as i64) >= (4)
              }
              _ => false,
          }
      }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { token }, Action::Update { dir: Direction::Send, tok2, .. }) => {
                  let token = token.clone();
                  let tok2 = tok2.clone();
                  if !(((tok2).len() as i64) >= (4)) { self.state = State::Error; return false; }
                  self.state = State::S3 { token, tok2 };
                  true
              }
              (State::S0 { token }, Action::Quit { dir: Direction::Send, .. }) => {
                  let token = token.clone();
                  self.state = State::S5 { token };
                  true
              }
              (State::S3 { token, tok2 }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let token = token.clone();
                  let tok2 = tok2.clone();
                  let new_token = tok2;
                  if !(((new_token).len() as i64) >= (4)) { self.state = State::Error; return false; }
                  self.state = State::S0 { token: new_token };
                  true
              }
              (State::S5 { token }, Action::Quit { dir: Direction::Recv, .. }) => {
                  let token = token.clone();
                  self.state = State::S6 { token };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server (strlen: string type + len(), documents codegen gap)
  $ nuscr --gencode-rust-test=S@Strlen Strlen.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Ack { dir: Direction },
      Quit { dir: Direction },
      Update { dir: Direction, tok2: String },
  }
  
  pub trait Monitor {
      fn new() -> Self;
      fn accepts(&self, action: &Action) -> bool;
      fn step(&mut self, action: &Action) -> bool;
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { token: String },
      S3 { token: String, tok2: String },
      S5 { token: String },
      S6 { token: String },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct StrlenMonitor { state: State }
  
  #[allow(unused_variables)]
  impl Monitor for StrlenMonitor {
      fn new() -> Self {
          Self { state: State::S0 { token: "init".to_string() } }
      }
  
      fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Quit { dir: Direction::Recv, .. } => true,
              Action::Update { dir: Direction::Recv, tok2, .. } => {
                  let tok2 = tok2.clone();
                  ((tok2).len() as i64) >= (4)
              }
              Action::Ack { dir: Direction::Send, .. } => true,
              Action::Quit { dir: Direction::Send, .. } => true,
              _ => false,
          }
      }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { token }, Action::Update { dir: Direction::Recv, tok2, .. }) => {
                  let token = token.clone();
                  let tok2 = tok2.clone();
                  if !(((tok2).len() as i64) >= (4)) { self.state = State::Error; return false; }
                  self.state = State::S3 { token, tok2 };
                  true
              }
              (State::S0 { token }, Action::Quit { dir: Direction::Recv, .. }) => {
                  let token = token.clone();
                  self.state = State::S5 { token };
                  true
              }
              (State::S3 { token, tok2 }, Action::Ack { dir: Direction::Send, .. }) => {
                  let token = token.clone();
                  let tok2 = tok2.clone();
                  let new_token = tok2;
                  if !(((new_token).len() as i64) >= (4)) { self.state = State::Error; return false; }
                  self.state = State::S0 { token: new_token };
                  true
              }
              (State::S5 { token }, Action::Quit { dir: Direction::Send, .. }) => {
                  let token = token.clone();
                  self.state = State::S6 { token };
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
