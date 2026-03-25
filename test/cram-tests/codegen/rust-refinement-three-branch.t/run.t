Generate Rust monitor for Client (three-branch choice)
  $ nuscr --gencode-rust-test=C@ThreeWay ThreeWay.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { n: i64 },
      S3 { n: i64, x: i64 },
      S5 { n: i64, x: i64 },
      S7 { n: i64 },
      S8 { n: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct ThreeWayMonitor { state: State }
  
  impl Monitor for ThreeWayMonitor {
      fn new() -> Self {
          Self { state: State::S0 { n: 0 } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { n }, Action::Low { dir: Direction::Send, x, .. }) => {
                  if !((x) < (10)) { self.state = State::Error; return false; }
                  self.state = State::S3 { n, x };
                  true
              }
              (State::S0 { n }, Action::Mid { dir: Direction::Send, x, .. }) => {
                  if !(((x) >= (10)) && ((x) < (100))) { self.state = State::Error; return false; }
                  self.state = State::S5 { n, x };
                  true
              }
              (State::S0 { n }, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = State::S7 { n };
                  true
              }
              (State::S3 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = State::Error; return false; }
                  self.state = State::S0 { n: new_n };
                  true
              }
              (State::S5 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = State::Error; return false; }
                  self.state = State::S0 { n: new_n };
                  true
              }
              (State::S7 { n }, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = State::S8 { n };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server (three-branch choice)
  $ nuscr --gencode-rust-test=S@ThreeWay ThreeWay.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { n: i64 },
      S3 { n: i64, x: i64 },
      S5 { n: i64, x: i64 },
      S7 { n: i64 },
      S8 { n: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct ThreeWayMonitor { state: State }
  
  impl Monitor for ThreeWayMonitor {
      fn new() -> Self {
          Self { state: State::S0 { n: 0 } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { n }, Action::Low { dir: Direction::Recv, x, .. }) => {
                  if !((x) < (10)) { self.state = State::Error; return false; }
                  self.state = State::S3 { n, x };
                  true
              }
              (State::S0 { n }, Action::Mid { dir: Direction::Recv, x, .. }) => {
                  if !(((x) >= (10)) && ((x) < (100))) { self.state = State::Error; return false; }
                  self.state = State::S5 { n, x };
                  true
              }
              (State::S0 { n }, Action::Bye { dir: Direction::Recv, .. }) => {
                  self.state = State::S7 { n };
                  true
              }
              (State::S3 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = State::Error; return false; }
                  self.state = State::S0 { n: new_n };
                  true
              }
              (State::S5 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
                  let new_n = (n) + (1);
                  if !((new_n) >= (0)) { self.state = State::Error; return false; }
                  self.state = State::S0 { n: new_n };
                  true
              }
              (State::S7 { n }, Action::Bye { dir: Direction::Send, .. }) => {
                  self.state = State::S8 { n };
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
  15 | impl Monitor for ThreeWayMonitor {
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
    --> C_monitor.rs:25:31
     |
  25 |             (State::S0 { n }, Action::Low { dir: Direction::Send, x, .. }) => {
     |                               ^^^^^^
     |                               |
     |                               use of undeclared type `Action`
     |                               help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:25:50
     |
  25 |             (State::S0 { n }, Action::Low { dir: Direction::Send, x, .. }) => {
     |                                                  ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:30:31
     |
  30 |             (State::S0 { n }, Action::Mid { dir: Direction::Send, x, .. }) => {
     |                               ^^^^^^
     |                               |
     |                               use of undeclared type `Action`
     |                               help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:30:50
     |
  30 |             (State::S0 { n }, Action::Mid { dir: Direction::Send, x, .. }) => {
     |                                                  ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:35:31
     |
  35 |             (State::S0 { n }, Action::Bye { dir: Direction::Send, .. }) => {
     |                               ^^^^^^
     |                               |
     |                               use of undeclared type `Action`
     |                               help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:35:50
     |
  35 |             (State::S0 { n }, Action::Bye { dir: Direction::Send, .. }) => {
     |                                                  ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:39:34
     |
  39 |             (State::S3 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
     |                                  ^^^^^^
     |                                  |
     |                                  use of undeclared type `Action`
     |                                  help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:39:53
     |
  39 |             (State::S3 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
     |                                                     ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:45:34
     |
  45 |             (State::S5 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
     |                                  ^^^^^^
     |                                  |
     |                                  use of undeclared type `Action`
     |                                  help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:45:53
     |
  45 |             (State::S5 { n, x }, Action::Ack { dir: Direction::Recv, .. }) => {
     |                                                     ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:51:31
     |
  51 |             (State::S7 { n }, Action::Bye { dir: Direction::Recv, .. }) => {
     |                               ^^^^^^
     |                               |
     |                               use of undeclared type `Action`
     |                               help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:51:50
     |
  51 |             (State::S7 { n }, Action::Bye { dir: Direction::Recv, .. }) => {
     |                                                  ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 15 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
  error[E0405]: cannot find trait `Monitor` in this scope
    --> S_monitor.rs:15:6
     |
  15 | impl Monitor for ThreeWayMonitor {
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
    --> S_monitor.rs:25:31
     |
  25 |             (State::S0 { n }, Action::Low { dir: Direction::Recv, x, .. }) => {
     |                               ^^^^^^
     |                               |
     |                               use of undeclared type `Action`
     |                               help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:25:50
     |
  25 |             (State::S0 { n }, Action::Low { dir: Direction::Recv, x, .. }) => {
     |                                                  ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:30:31
     |
  30 |             (State::S0 { n }, Action::Mid { dir: Direction::Recv, x, .. }) => {
     |                               ^^^^^^
     |                               |
     |                               use of undeclared type `Action`
     |                               help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:30:50
     |
  30 |             (State::S0 { n }, Action::Mid { dir: Direction::Recv, x, .. }) => {
     |                                                  ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:35:31
     |
  35 |             (State::S0 { n }, Action::Bye { dir: Direction::Recv, .. }) => {
     |                               ^^^^^^
     |                               |
     |                               use of undeclared type `Action`
     |                               help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:35:50
     |
  35 |             (State::S0 { n }, Action::Bye { dir: Direction::Recv, .. }) => {
     |                                                  ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:39:34
     |
  39 |             (State::S3 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
     |                                  ^^^^^^
     |                                  |
     |                                  use of undeclared type `Action`
     |                                  help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:39:53
     |
  39 |             (State::S3 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
     |                                                     ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:45:34
     |
  45 |             (State::S5 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
     |                                  ^^^^^^
     |                                  |
     |                                  use of undeclared type `Action`
     |                                  help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:45:53
     |
  45 |             (State::S5 { n, x }, Action::Ack { dir: Direction::Send, .. }) => {
     |                                                     ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:51:31
     |
  51 |             (State::S7 { n }, Action::Bye { dir: Direction::Send, .. }) => {
     |                               ^^^^^^
     |                               |
     |                               use of undeclared type `Action`
     |                               help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:51:50
     |
  51 |             (State::S7 { n }, Action::Bye { dir: Direction::Send, .. }) => {
     |                                                  ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 15 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]
