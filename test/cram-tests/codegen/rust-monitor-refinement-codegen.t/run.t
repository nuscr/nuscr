Generate Rust monitor for Client
  $ nuscr --gencode-rust=C@RunningSum RunningSum.nuscr > C_monitor.rs
  $ cat C_monitor.rs
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum State {
      S0,
      S3,
      S5,
      S6,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Add,
      Bye,
      Sum,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Direction {
      Send,
      Recv,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub enum Value {
      Int(i64),
      Bool(bool),
      String(String),
      Unit,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Action {
      dir: Direction,
      label: Label,
      payloads: Vec<Value>,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningsumMonitor {
      state: State,
      total: i64,
  }
  
  impl RunningsumMonitor {
      pub fn new() -> Self {
          Self {
              state: State::S0,
              total: 0,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state, action.dir, action.label) {
              (State::S0, Direction::Send, Label::Add) => match action.payloads.as_slice() {
                  [Value::Int(x), Value::Int(y)] => { self.state = State::S3; true }
                  _ => false
              },
              (State::S0, Direction::Send, Label::Bye) => match action.payloads.as_slice() {
                  [] => { self.state = State::S5; true }
                  _ => false
              },
              (State::S3, Direction::Recv, Label::Sum) => match action.payloads.as_slice() {
                  [Value::Int(r)] => { self.state = State::S0; true }
                  _ => false
              },
              (State::S5, Direction::Recv, Label::Bye) => match action.payloads.as_slice() {
                  [] => { self.state = State::S6; true }
                  _ => false
              },
              _ => false
            }
      }
  }
  

Generate Rust monitor for Server
  $ nuscr --gencode-rust=S@RunningSum RunningSum.nuscr > S_monitor.rs
  $ cat S_monitor.rs
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  enum State {
      S0,
      S3,
      S5,
      S6,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Label {
      Add,
      Bye,
      Sum,
  }
  
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Direction {
      Send,
      Recv,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub enum Value {
      Int(i64),
      Bool(bool),
      String(String),
      Unit,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Action {
      dir: Direction,
      label: Label,
      payloads: Vec<Value>,
  }
  
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct RunningsumMonitor {
      state: State,
      total: i64,
  }
  
  impl RunningsumMonitor {
      pub fn new() -> Self {
          Self {
              state: State::S0,
              total: 0,
          }
      }
  
      pub fn step(&mut self, action: &Action) -> bool {
          match (self.state, action.dir, action.label) {
              (State::S0, Direction::Recv, Label::Add) => match action.payloads.as_slice() {
                  [Value::Int(x), Value::Int(y)] => { self.state = State::S3; true }
                  _ => false
              },
              (State::S0, Direction::Recv, Label::Bye) => match action.payloads.as_slice() {
                  [] => { self.state = State::S5; true }
                  _ => false
              },
              (State::S3, Direction::Send, Label::Sum) => match action.payloads.as_slice() {
                  [Value::Int(r)] => { self.state = State::S0; true }
                  _ => false
              },
              (State::S5, Direction::Send, Label::Bye) => match action.payloads.as_slice() {
                  [] => { self.state = State::S6; true }
                  _ => false
              },
              _ => false
            }
      }
  }
  

Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib
  warning: unused variable: `x`
    --> C_monitor.rs:54:29
     |
  54 |                 [Value::Int(x), Value::Int(y)] => { self.state = State::S3; true }
     |                             ^ help: if this is intentional, prefix it with an underscore: `_x`
     |
     = note: `#[warn(unused_variables)]` (part of `#[warn(unused)]`) on by default
  
  warning: unused variable: `y`
    --> C_monitor.rs:54:44
     |
  54 |                 [Value::Int(x), Value::Int(y)] => { self.state = State::S3; true }
     |                                            ^ help: if this is intentional, prefix it with an underscore: `_y`
  
  warning: unused variable: `r`
    --> C_monitor.rs:62:29
     |
  62 |                 [Value::Int(r)] => { self.state = State::S0; true }
     |                             ^ help: if this is intentional, prefix it with an underscore: `_r`
  
  warning: 3 warnings emitted
  

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib
  warning: unused variable: `x`
    --> S_monitor.rs:54:29
     |
  54 |                 [Value::Int(x), Value::Int(y)] => { self.state = State::S3; true }
     |                             ^ help: if this is intentional, prefix it with an underscore: `_x`
     |
     = note: `#[warn(unused_variables)]` (part of `#[warn(unused)]`) on by default
  
  warning: unused variable: `y`
    --> S_monitor.rs:54:44
     |
  54 |                 [Value::Int(x), Value::Int(y)] => { self.state = State::S3; true }
     |                                            ^ help: if this is intentional, prefix it with an underscore: `_y`
  
  warning: unused variable: `r`
    --> S_monitor.rs:62:29
     |
  62 |                 [Value::Int(r)] => { self.state = State::S0; true }
     |                             ^ help: if this is intentional, prefix it with an underscore: `_r`
  
  warning: 3 warnings emitted
  
