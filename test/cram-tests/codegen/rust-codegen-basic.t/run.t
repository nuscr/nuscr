Generate Rust monitor for Client
  $ nuscr --gencode-rust-test=C@Adder Adder.nuscr > C_monitor.rs
  $ cat C_monitor.rs
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
  
  impl Monitor for AdderMonitor {
      fn new() -> Self {
          Self { state: State::S0 }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
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
  
  impl Monitor for AdderMonitor {
      fn new() -> Self {
          Self { state: State::S0 }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
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
  error[E0405]: cannot find trait `Monitor` in this scope
    --> C_monitor.rs:15:6
     |
  15 | impl Monitor for AdderMonitor {
     |      ^^^^^^^ not found in this scope
  
  error[E0425]: cannot find type `Action` in this scope
     --> C_monitor.rs:20:33
      |
   20 |     fn accepts(&self, _action: &Action) -> bool { true }
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0425]: cannot find type `Action` in this scope
     --> C_monitor.rs:22:33
      |
   22 |     fn step(&mut self, action: &Action) -> bool {
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:25:25
     |
  25 |             (State::S0, Action::Add { dir: Direction::Send, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:25:44
     |
  25 |             (State::S0, Action::Add { dir: Direction::Send, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:29:25
     |
  29 |             (State::S0, Action::Bye { dir: Direction::Send, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:29:44
     |
  29 |             (State::S0, Action::Bye { dir: Direction::Send, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:33:25
     |
  33 |             (State::S3, Action::Add { dir: Direction::Send, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:33:44
     |
  33 |             (State::S3, Action::Add { dir: Direction::Send, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:37:25
     |
  37 |             (State::S4, Action::Sum { dir: Direction::Recv, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:37:44
     |
  37 |             (State::S4, Action::Sum { dir: Direction::Recv, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:41:25
     |
  41 |             (State::S6, Action::Bye { dir: Direction::Recv, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:41:44
     |
  41 |             (State::S6, Action::Bye { dir: Direction::Recv, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 13 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
  error[E0405]: cannot find trait `Monitor` in this scope
    --> S_monitor.rs:15:6
     |
  15 | impl Monitor for AdderMonitor {
     |      ^^^^^^^ not found in this scope
  
  error[E0425]: cannot find type `Action` in this scope
     --> S_monitor.rs:20:33
      |
   20 |     fn accepts(&self, _action: &Action) -> bool { true }
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0425]: cannot find type `Action` in this scope
     --> S_monitor.rs:22:33
      |
   22 |     fn step(&mut self, action: &Action) -> bool {
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:25:25
     |
  25 |             (State::S0, Action::Add { dir: Direction::Recv, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:25:44
     |
  25 |             (State::S0, Action::Add { dir: Direction::Recv, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:29:25
     |
  29 |             (State::S0, Action::Bye { dir: Direction::Recv, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:29:44
     |
  29 |             (State::S0, Action::Bye { dir: Direction::Recv, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:33:25
     |
  33 |             (State::S3, Action::Add { dir: Direction::Recv, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:33:44
     |
  33 |             (State::S3, Action::Add { dir: Direction::Recv, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:37:25
     |
  37 |             (State::S4, Action::Sum { dir: Direction::Send, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:37:44
     |
  37 |             (State::S4, Action::Sum { dir: Direction::Send, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:41:25
     |
  41 |             (State::S6, Action::Bye { dir: Direction::Send, .. }) => {
     |                         ^^^^^^
     |                         |
     |                         use of undeclared type `Action`
     |                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:41:44
     |
  41 |             (State::S6, Action::Bye { dir: Direction::Send, .. }) => {
     |                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 13 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]
