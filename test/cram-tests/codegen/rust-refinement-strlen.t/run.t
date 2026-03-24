Generate Rust monitor for Client (strlen: string type + len(), documents codegen gap)
  $ nuscr --gencode-rust=C@Strlen Strlen.nuscr > C_monitor.rs
  $ cat C_monitor.rs
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
  
  impl Monitor for StrlenMonitor {
      fn new() -> Self {
          Self { state: State::S0 { token: "init".to_string() } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { token }, Action::Update { dir: Direction::Send, tok2, .. }) => {
                  let tok2 = tok2.clone();
                  if !(((tok2).len() as i64) >= (4)) { self.state = State::Error; return false; }
                  self.state = State::S3 { token, tok2 };
                  true
              }
              (State::S0 { token }, Action::Quit { dir: Direction::Send, .. }) => {
                  self.state = State::S5 { token };
                  true
              }
              (State::S3 { token, tok2 }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let new_token = tok2;
                  if !(((new_token).len() as i64) >= (4)) { self.state = State::Error; return false; }
                  self.state = State::S0 { token: new_token };
                  true
              }
              (State::S5 { token }, Action::Quit { dir: Direction::Recv, .. }) => {
                  self.state = State::S6 { token };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server (strlen: string type + len(), documents codegen gap)
  $ nuscr --gencode-rust=S@Strlen Strlen.nuscr > S_monitor.rs
  $ cat S_monitor.rs
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
  
  impl Monitor for StrlenMonitor {
      fn new() -> Self {
          Self { state: State::S0 { token: "init".to_string() } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { token }, Action::Update { dir: Direction::Recv, tok2, .. }) => {
                  let tok2 = tok2.clone();
                  if !(((tok2).len() as i64) >= (4)) { self.state = State::Error; return false; }
                  self.state = State::S3 { token, tok2 };
                  true
              }
              (State::S0 { token }, Action::Quit { dir: Direction::Recv, .. }) => {
                  self.state = State::S5 { token };
                  true
              }
              (State::S3 { token, tok2 }, Action::Ack { dir: Direction::Send, .. }) => {
                  let new_token = tok2;
                  if !(((new_token).len() as i64) >= (4)) { self.state = State::Error; return false; }
                  self.state = State::S0 { token: new_token };
                  true
              }
              (State::S5 { token }, Action::Quit { dir: Direction::Send, .. }) => {
                  self.state = State::S6 { token };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  
Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib
  error[E0405]: cannot find trait `Monitor` in this scope
    --> C_monitor.rs:14:6
     |
  14 | impl Monitor for StrlenMonitor {
     |      ^^^^^^^ not found in this scope
  
  error[E0425]: cannot find type `Action` in this scope
     --> C_monitor.rs:19:33
      |
   19 |     fn accepts(&self, _action: &Action) -> bool { true }
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0425]: cannot find type `Action` in this scope
     --> C_monitor.rs:21:33
      |
   21 |     fn step(&mut self, action: &Action) -> bool {
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:24:35
     |
  24 |             (State::S0 { token }, Action::Update { dir: Direction::Send, tok2, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:24:57
     |
  24 |             (State::S0 { token }, Action::Update { dir: Direction::Send, tok2, .. }) => {
     |                                                         ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:30:35
     |
  30 |             (State::S0 { token }, Action::Quit { dir: Direction::Send, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:30:55
     |
  30 |             (State::S0 { token }, Action::Quit { dir: Direction::Send, .. }) => {
     |                                                       ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:34:41
     |
  34 |             (State::S3 { token, tok2 }, Action::Ack { dir: Direction::Recv, .. }) => {
     |                                         ^^^^^^
     |                                         |
     |                                         use of undeclared type `Action`
     |                                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:34:60
     |
  34 |             (State::S3 { token, tok2 }, Action::Ack { dir: Direction::Recv, .. }) => {
     |                                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:40:35
     |
  40 |             (State::S5 { token }, Action::Quit { dir: Direction::Recv, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:40:55
     |
  40 |             (State::S5 { token }, Action::Quit { dir: Direction::Recv, .. }) => {
     |                                                       ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 11 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
  error[E0405]: cannot find trait `Monitor` in this scope
    --> S_monitor.rs:14:6
     |
  14 | impl Monitor for StrlenMonitor {
     |      ^^^^^^^ not found in this scope
  
  error[E0425]: cannot find type `Action` in this scope
     --> S_monitor.rs:19:33
      |
   19 |     fn accepts(&self, _action: &Action) -> bool { true }
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0425]: cannot find type `Action` in this scope
     --> S_monitor.rs:21:33
      |
   21 |     fn step(&mut self, action: &Action) -> bool {
      |                                 ^^^^^^ help: an enum with a similar name exists: `Option`
      |
     ::: /home/remco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/option.rs:600:1
      |
  600 | pub enum Option<T> {
      | ------------------ similarly named enum `Option` defined here
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:24:35
     |
  24 |             (State::S0 { token }, Action::Update { dir: Direction::Recv, tok2, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:24:57
     |
  24 |             (State::S0 { token }, Action::Update { dir: Direction::Recv, tok2, .. }) => {
     |                                                         ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:30:35
     |
  30 |             (State::S0 { token }, Action::Quit { dir: Direction::Recv, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:30:55
     |
  30 |             (State::S0 { token }, Action::Quit { dir: Direction::Recv, .. }) => {
     |                                                       ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:34:41
     |
  34 |             (State::S3 { token, tok2 }, Action::Ack { dir: Direction::Send, .. }) => {
     |                                         ^^^^^^
     |                                         |
     |                                         use of undeclared type `Action`
     |                                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:34:60
     |
  34 |             (State::S3 { token, tok2 }, Action::Ack { dir: Direction::Send, .. }) => {
     |                                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:40:35
     |
  40 |             (State::S5 { token }, Action::Quit { dir: Direction::Send, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:40:55
     |
  40 |             (State::S5 { token }, Action::Quit { dir: Direction::Send, .. }) => {
     |                                                       ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 11 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]
