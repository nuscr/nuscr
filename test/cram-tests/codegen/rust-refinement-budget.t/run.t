Generate Rust monitor for Client (budget: rec var in send guard, subtraction update)
  $ nuscr --gencode-rust=C@Budget Budget.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64 },
      S6 { budget: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: State }
  
  impl Monitor for BudgetMonitor {
      fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { budget }, Action::Spend { dir: Direction::Send, amount, .. }) => {
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = State::Error; return false; }
                  self.state = State::S3 { budget, amount };
                  true
              }
              (State::S0 { budget }, Action::Done { dir: Direction::Send, .. }) => {
                  self.state = State::S5 { budget };
                  true
              }
              (State::S3 { budget, amount }, Action::Ok { dir: Direction::Recv, .. }) => {
                  let new_budget = (budget) - (amount);
                  if !((new_budget) >= (0)) { self.state = State::Error; return false; }
                  self.state = State::S0 { budget: new_budget };
                  true
              }
              (State::S5 { budget }, Action::Done { dir: Direction::Recv, .. }) => {
                  self.state = State::S6 { budget };
                  true
              }
              _ => { self.state = State::Error; false }
          }
      }
  }
  

Generate Rust monitor for Server (budget: rec var in send guard, subtraction update)
  $ nuscr --gencode-rust=S@Budget Budget.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  #[allow(dead_code)]
  enum State {
      S0 { budget: i64 },
      S3 { budget: i64, amount: i64 },
      S5 { budget: i64 },
      S6 { budget: i64 },
      Error,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct BudgetMonitor { state: State }
  
  impl Monitor for BudgetMonitor {
      fn new() -> Self {
          Self { state: State::S0 { budget: 1000 } }
      }
  
      fn accepts(&self, _action: &Action) -> bool { true }
  
      fn step(&mut self, action: &Action) -> bool {
          match (&self.state, action) {
              (State::Error, _) => true,
              (State::S0 { budget }, Action::Spend { dir: Direction::Recv, amount, .. }) => {
                  if !(((amount) > (0)) && ((amount) <= (budget))) { self.state = State::Error; return false; }
                  self.state = State::S3 { budget, amount };
                  true
              }
              (State::S0 { budget }, Action::Done { dir: Direction::Recv, .. }) => {
                  self.state = State::S5 { budget };
                  true
              }
              (State::S3 { budget, amount }, Action::Ok { dir: Direction::Send, .. }) => {
                  let new_budget = (budget) - (amount);
                  if !((new_budget) >= (0)) { self.state = State::Error; return false; }
                  self.state = State::S0 { budget: new_budget };
                  true
              }
              (State::S5 { budget }, Action::Done { dir: Direction::Send, .. }) => {
                  self.state = State::S6 { budget };
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
  14 | impl Monitor for BudgetMonitor {
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
    --> C_monitor.rs:24:36
     |
  24 |             (State::S0 { budget }, Action::Spend { dir: Direction::Send, amount, .. }) => {
     |                                    ^^^^^^
     |                                    |
     |                                    use of undeclared type `Action`
     |                                    help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:24:57
     |
  24 |             (State::S0 { budget }, Action::Spend { dir: Direction::Send, amount, .. }) => {
     |                                                         ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:29:36
     |
  29 |             (State::S0 { budget }, Action::Done { dir: Direction::Send, .. }) => {
     |                                    ^^^^^^
     |                                    |
     |                                    use of undeclared type `Action`
     |                                    help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:29:56
     |
  29 |             (State::S0 { budget }, Action::Done { dir: Direction::Send, .. }) => {
     |                                                        ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:33:44
     |
  33 |             (State::S3 { budget, amount }, Action::Ok { dir: Direction::Recv, .. }) => {
     |                                            ^^^^^^
     |                                            |
     |                                            use of undeclared type `Action`
     |                                            help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:33:62
     |
  33 |             (State::S3 { budget, amount }, Action::Ok { dir: Direction::Recv, .. }) => {
     |                                                              ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> C_monitor.rs:39:36
     |
  39 |             (State::S5 { budget }, Action::Done { dir: Direction::Recv, .. }) => {
     |                                    ^^^^^^
     |                                    |
     |                                    use of undeclared type `Action`
     |                                    help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> C_monitor.rs:39:56
     |
  39 |             (State::S5 { budget }, Action::Done { dir: Direction::Recv, .. }) => {
     |                                                        ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 11 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
  error[E0405]: cannot find trait `Monitor` in this scope
    --> S_monitor.rs:14:6
     |
  14 | impl Monitor for BudgetMonitor {
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
    --> S_monitor.rs:24:36
     |
  24 |             (State::S0 { budget }, Action::Spend { dir: Direction::Recv, amount, .. }) => {
     |                                    ^^^^^^
     |                                    |
     |                                    use of undeclared type `Action`
     |                                    help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:24:57
     |
  24 |             (State::S0 { budget }, Action::Spend { dir: Direction::Recv, amount, .. }) => {
     |                                                         ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:29:36
     |
  29 |             (State::S0 { budget }, Action::Done { dir: Direction::Recv, .. }) => {
     |                                    ^^^^^^
     |                                    |
     |                                    use of undeclared type `Action`
     |                                    help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:29:56
     |
  29 |             (State::S0 { budget }, Action::Done { dir: Direction::Recv, .. }) => {
     |                                                        ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:33:44
     |
  33 |             (State::S3 { budget, amount }, Action::Ok { dir: Direction::Send, .. }) => {
     |                                            ^^^^^^
     |                                            |
     |                                            use of undeclared type `Action`
     |                                            help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:33:62
     |
  33 |             (State::S3 { budget, amount }, Action::Ok { dir: Direction::Send, .. }) => {
     |                                                              ^^^^^^^^^ use of undeclared type `Direction`
  
  error[E0433]: failed to resolve: use of undeclared type `Action`
    --> S_monitor.rs:39:36
     |
  39 |             (State::S5 { budget }, Action::Done { dir: Direction::Send, .. }) => {
     |                                    ^^^^^^
     |                                    |
     |                                    use of undeclared type `Action`
     |                                    help: an enum with a similar name exists: `Option`
  
  error[E0433]: failed to resolve: use of undeclared type `Direction`
    --> S_monitor.rs:39:56
     |
  39 |             (State::S5 { budget }, Action::Done { dir: Direction::Send, .. }) => {
     |                                                        ^^^^^^^^^ use of undeclared type `Direction`
  
  error: aborting due to 11 previous errors
  
  Some errors have detailed explanations: E0405, E0425, E0433.
  For more information about an error, try `rustc --explain E0405`.
  [1]
