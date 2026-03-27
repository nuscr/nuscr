Interior underscores preserved, only trailing underscore stripped

  $ nuscr --gencode-rust-test=C@Interior Interior.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Req { dir: Direction, foo_bar: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum InteriorState {
      S0,
      S1 { foo_bar: i64 },
      S2 { foo_bar: i64, foo_bar_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct InteriorMonitor { state: InteriorState }
  
  #[allow(unused_variables)]
  impl InteriorMonitor {
      pub fn new() -> Self {
          Self { state: InteriorState::S0 }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Req { dir: Direction::Send, foo_bar, .. } => {
                  let foo_bar = *foo_bar;
                  (foo_bar) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (InteriorState::Error, _) => true,
              (InteriorState::S0, Action::Req { dir: Direction::Send, foo_bar, .. }) => {
                  let foo_bar = *foo_bar;
                  if !((foo_bar) > (0)) { self.state = InteriorState::Error; return false; }
                  self.state = InteriorState::S1 { foo_bar };
                  true
              }
              (InteriorState::S1 { foo_bar }, Action::Req { dir: Direction::Send, foo_bar: foo_bar_, .. }) => {
                  let foo_bar = *foo_bar;
                  let foo_bar_ = *foo_bar_;
                  if !((foo_bar_) > (foo_bar)) { self.state = InteriorState::Error; return false; }
                  self.state = InteriorState::S2 { foo_bar, foo_bar_ };
                  true
              }
              _ => { self.state = InteriorState::Error; false }
          }
      }
  }
  

  $ nuscr --gencode-rust-test=S@Interior Interior.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  pub enum Direction {
      Recv,
      Send,
  }
  
  #[allow(dead_code)]
  pub enum Action {
      Req { dir: Direction, foo_bar: i64 },
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum InteriorState {
      S0,
      S1 { foo_bar: i64 },
      S2 { foo_bar: i64, foo_bar_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct InteriorMonitor { state: InteriorState }
  
  #[allow(unused_variables)]
  impl InteriorMonitor {
      pub fn new() -> Self {
          Self { state: InteriorState::S0 }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Req { dir: Direction::Recv, foo_bar, .. } => {
                  let foo_bar = *foo_bar;
                  (foo_bar) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (InteriorState::Error, _) => true,
              (InteriorState::S0, Action::Req { dir: Direction::Recv, foo_bar, .. }) => {
                  let foo_bar = *foo_bar;
                  if !((foo_bar) > (0)) { self.state = InteriorState::Error; return false; }
                  self.state = InteriorState::S1 { foo_bar };
                  true
              }
              (InteriorState::S1 { foo_bar }, Action::Req { dir: Direction::Recv, foo_bar: foo_bar_, .. }) => {
                  let foo_bar = *foo_bar;
                  let foo_bar_ = *foo_bar_;
                  if !((foo_bar_) > (foo_bar)) { self.state = InteriorState::Error; return false; }
                  self.state = InteriorState::S2 { foo_bar, foo_bar_ };
                  true
              }
              _ => { self.state = InteriorState::Error; false }
          }
      }
  }
  

Compile both monitors
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen
  $ nuscr --gencode-rust=C@Interior Interior.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum InteriorState {
      S0,
      S1 { foo_bar: i64 },
      S2 { foo_bar: i64, foo_bar_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct InteriorMonitor { state: InteriorState }
  
  #[allow(unused_variables)]
  impl InteriorMonitor {
      pub fn new() -> Self {
          Self { state: InteriorState::S0 }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Req { dir: Direction::Send, foo_bar, .. } => {
                  let foo_bar = *foo_bar;
                  (foo_bar) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (InteriorState::Error, _) => true,
              (InteriorState::S0, Action::Req { dir: Direction::Send, foo_bar, .. }) => {
                  let foo_bar = *foo_bar;
                  if !((foo_bar) > (0)) { self.state = InteriorState::Error; return false; }
                  self.state = InteriorState::S1 { foo_bar };
                  true
              }
              (InteriorState::S1 { foo_bar }, Action::Req { dir: Direction::Send, foo_bar: foo_bar_, .. }) => {
                  let foo_bar = *foo_bar;
                  let foo_bar_ = *foo_bar_;
                  if !((foo_bar_) > (foo_bar)) { self.state = InteriorState::Error; return false; }
                  self.state = InteriorState::S2 { foo_bar, foo_bar_ };
                  true
              }
              _ => { self.state = InteriorState::Error; false }
          }
      }
  }
  
  $ nuscr --gencode-rust=S@Interior Interior.nuscr
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  #[allow(dead_code)]
  enum InteriorState {
      S0,
      S1 { foo_bar: i64 },
      S2 { foo_bar: i64, foo_bar_: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct InteriorMonitor { state: InteriorState }
  
  #[allow(unused_variables)]
  impl InteriorMonitor {
      pub fn new() -> Self {
          Self { state: InteriorState::S0 }
      }
  
      pub fn accepts(&self, action: &Action) -> bool {
          match action {
              Action::Req { dir: Direction::Recv, foo_bar, .. } => {
                  let foo_bar = *foo_bar;
                  (foo_bar) > (0)
              }
              _ => false,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (InteriorState::Error, _) => true,
              (InteriorState::S0, Action::Req { dir: Direction::Recv, foo_bar, .. }) => {
                  let foo_bar = *foo_bar;
                  if !((foo_bar) > (0)) { self.state = InteriorState::Error; return false; }
                  self.state = InteriorState::S1 { foo_bar };
                  true
              }
              (InteriorState::S1 { foo_bar }, Action::Req { dir: Direction::Recv, foo_bar: foo_bar_, .. }) => {
                  let foo_bar = *foo_bar;
                  let foo_bar_ = *foo_bar_;
                  if !((foo_bar_) > (foo_bar)) { self.state = InteriorState::Error; return false; }
                  self.state = InteriorState::S2 { foo_bar, foo_bar_ };
                  true
              }
              _ => { self.state = InteriorState::Error; false }
          }
      }
  }
  
