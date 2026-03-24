Generate Rust monitor for Client
  $ nuscr --gencode-rust=C@RunningSum RunningSum.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: State }
  
  impl Monitor for RunningSumMonitor {
      fn new() -> Self {
          Self { state: State::S0 { total: 0 } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { total }, Action::Add { dir: Direction::Send, x, y, .. }) => {
                  if !((x) > (0) && (y) > (0)) { self.state = State::Error; return false; }
                  self.state = State::S3 { total, x, y };
                  true
              }
              (State::S0 { total }, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = State::S5 { total };
                  true
              }
              (State::S3 { total, x, y }, Action::Sum { dir: Direction::Recv, r, .. }) => {
                  if !((r) == ((x) + (y))) { self.state = State::Error; return false; }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = State::Error; return false; }
                  self.state = State::S0 { total: new_total };
                  true
              }
              (State::S5 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = State::S6 { total };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server
  $ nuscr --gencode-rust=S@RunningSum RunningSum.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { total: i64 },
      S3 { total: i64, x: i64, y: i64 },
      S5 { total: i64 },
      S6 { total: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningSumMonitor { state: State }
  
  impl Monitor for RunningSumMonitor {
      fn new() -> Self {
          Self { state: State::S0 { total: 0 } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { total }, Action::Add { dir: Direction::Recv, x, y, .. }) => {
                  if !((x) > (0) && (y) > (0)) { self.state = State::Error; return false; }
                  self.state = State::S3 { total, x, y };
                  true
              }
              (State::S0 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = State::S5 { total };
                  true
              }
              (State::S3 { total, x, y }, Action::Sum { dir: Direction::Send, r, .. }) => {
                  if !((r) == ((x) + (y))) { self.state = State::Error; return false; }
                  let new_total = (total) + (r);
                  if !((new_total) < (100)) { self.state = State::Error; return false; }
                  self.state = State::S0 { total: new_total };
                  true
              }
              (State::S5 { total }, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = State::S6 { total };
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
  14 | impl Monitor for RunningSumMonitor {
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
  24 |             (State::S0 { total }, Action::Add { dir: Direction::Send, x, y, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:24:54
     |
  24 |             (State::S0 { total }, Action::Add { dir: Direction::Send, x, y, .. }) => {
     |                                                      ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:29:35
     |
  29 |             (State::S0 { total }, Action::Bye { dir: Direction::Send, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:29:54
     |
  29 |             (State::S0 { total }, Action::Bye { dir: Direction::Send, .. }) => {
     |                                                      ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:33:41
     |
  33 |             (State::S3 { total, x, y }, Action::Sum { dir: Direction::Recv, r, .. }) => {
     |                                         ^^^^^^
     |                                         |
     |                                         use of undeclared type `Action`
     |                                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:33:60
     |
  33 |             (State::S3 { total, x, y }, Action::Sum { dir: Direction::Recv, r, .. }) => {
     |                                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:40:35
     |
  40 |             (State::S5 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:40:54
     |
  40 |             (State::S5 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
     |                                                      ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 11 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
  error[E0405]: cannot find trait `Monitor` in this scope
    --> S_monitor.rs:14:6
     |
  14 | impl Monitor for RunningSumMonitor {
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
  24 |             (State::S0 { total }, Action::Add { dir: Direction::Recv, x, y, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:24:54
     |
  24 |             (State::S0 { total }, Action::Add { dir: Direction::Recv, x, y, .. }) => {
     |                                                      ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:29:35
     |
  29 |             (State::S0 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:29:54
     |
  29 |             (State::S0 { total }, Action::Bye { dir: Direction::Recv, .. }) => {
     |                                                      ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:33:41
     |
  33 |             (State::S3 { total, x, y }, Action::Sum { dir: Direction::Send, r, .. }) => {
     |                                         ^^^^^^
     |                                         |
     |                                         use of undeclared type `Action`
     |                                         help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:33:60
     |
  33 |             (State::S3 { total, x, y }, Action::Sum { dir: Direction::Send, r, .. }) => {
     |                                                            ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:40:35
     |
  40 |             (State::S5 { total }, Action::Bye { dir: Direction::Send, .. }) => {
     |                                   ^^^^^^
     |                                   |
     |                                   use of undeclared type `Action`
     |                                   help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:40:54
     |
  40 |             (State::S5 { total }, Action::Bye { dir: Direction::Send, .. }) => {
     |                                                      ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 11 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]

