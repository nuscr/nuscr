Generate Rust monitor for Client (multi payload, cross-payload reference)
  $ nuscr --gencode-rust=C@MultiPayload MultiPayload.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor { state: State }
  
  impl Monitor for MultiPayloadMonitor {
      fn new() -> Self {
          Self { state: State::S0 }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0, Action::Req { dir: Direction::Send, a, b, .. }) => {
                  if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { self.state = State::Error; return false; }
                  self.state = State::S1 { a, b };
                  true
              }
              (State::S1 { a, b }, Action::Resp { dir: Direction::Recv, d, .. }) => {
                  if !((d) == ((a) - (b))) { self.state = State::Error; return false; }
                  self.state = State::S2 { a, b, d };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server (nested arith, cross-payload reference)
  $ nuscr --gencode-rust=S@MultiPayload MultiPayload.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0,
      S1 { a: i64, b: i64 },
      S2 { a: i64, b: i64, d: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct MultiPayloadMonitor { state: State }
  
  impl Monitor for MultiPayloadMonitor {
      fn new() -> Self {
          Self { state: State::S0 }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0, Action::Req { dir: Direction::Recv, a, b, .. }) => {
                  if !((a) > (0) && ((b) > (0)) && ((b) < (a))) { self.state = State::Error; return false; }
                  self.state = State::S1 { a, b };
                  true
              }
              (State::S1 { a, b }, Action::Resp { dir: Direction::Send, d, .. }) => {
                  if !((d) == ((a) - (b))) { self.state = State::Error; return false; }
                  self.state = State::S2 { a, b, d };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib
  error[E0405]: cannot find trait `Monitor` in this scope
    --> C_monitor.rs:13:6
     |
  13 | impl Monitor for MultiPayloadMonitor {
     |      ^^^^^^^ not found in this scope
  
  error[E0425]: cannot find type `Action` in this scope
     --> C_monitor.rs:18:33
      |
   18 |     fn accepts(&self, _action: &Action) -> bool { true }
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0425]: cannot find type `Action` in this scope
     --> C_monitor.rs:20:33
      |
   20 |     fn step(&mut self, action: &Action) -> bool {
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:23:25
     |
  23 |             (State::S0, Action::Req { dir: Direction::Send, a, b, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:23:44
     |
  23 |             (State::S0, Action::Req { dir: Direction::Send, a, b, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:28:34
     |
  28 |             (State::S1 { a, b }, Action::Resp { dir: Direction::Recv, d, .. }) => {
     |                                  ^^^^^^
     |                                  |
     |                                  use of undeclared type `Action`
     |                                  help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:28:54
     |
  28 |             (State::S1 { a, b }, Action::Resp { dir: Direction::Recv, d, .. }) => {
     |                                                      ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 7 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
  error[E0405]: cannot find trait `Monitor` in this scope
    --> S_monitor.rs:13:6
     |
  13 | impl Monitor for MultiPayloadMonitor {
     |      ^^^^^^^ not found in this scope
  
  error[E0425]: cannot find type `Action` in this scope
     --> S_monitor.rs:18:33
      |
   18 |     fn accepts(&self, _action: &Action) -> bool { true }
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0425]: cannot find type `Action` in this scope
     --> S_monitor.rs:20:33
      |
   20 |     fn step(&mut self, action: &Action) -> bool {
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:23:25
     |
  23 |             (State::S0, Action::Req { dir: Direction::Recv, a, b, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:23:44
     |
  23 |             (State::S0, Action::Req { dir: Direction::Recv, a, b, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:28:34
     |
  28 |             (State::S1 { a, b }, Action::Resp { dir: Direction::Send, d, .. }) => {
     |                                  ^^^^^^
     |                                  |
     |                                  use of undeclared type `Action`
     |                                  help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:28:54
     |
  28 |             (State::S1 { a, b }, Action::Resp { dir: Direction::Send, d, .. }) => {
     |                                                      ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 7 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]
