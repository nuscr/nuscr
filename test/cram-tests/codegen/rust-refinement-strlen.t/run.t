Generate Rust monitor for Client (strlen: string type + len(), documents codegen gap)
  $ nuscr --gencode-rust-test=C@Strlen Strlen.nuscr > C_monitor.rs
  nuscr: I'm sorry, it is unfortunate in Rust codegen use bool, int or unit, string type is not implemented (raised at lib/codegen/rust/rustexpr.ml: line 38)
  [124]
  $ cat C_monitor.rs

Generate Rust monitor for Server (strlen: string type + len(), documents codegen gap)
  $ nuscr --gencode-rust-test=S@Strlen Strlen.nuscr > S_monitor.rs
  nuscr: I'm sorry, it is unfortunate in Rust codegen use bool, int or unit, string type is not implemented (raised at lib/codegen/rust/rustexpr.ml: line 38)
  [124]
  $ cat S_monitor.rs
Compile Client monitor
  $ rustc --edition 2021 --crate-type lib C_monitor.rs -o C_monitor.rlib

Compile Server monitor
  $ rustc --edition 2021 --crate-type lib S_monitor.rs -o S_monitor.rlib

Production codegen (no support types, not compiled)
  $ nuscr --gencode-rust=C@Strlen Strlen.nuscr
  nuscr: I'm sorry, it is unfortunate in Rust codegen use bool, int or unit, string type is not implemented (raised at lib/codegen/rust/rustexpr.ml: line 38)
  [124]

  $ nuscr --gencode-rust=S@Strlen Strlen.nuscr
  nuscr: I'm sorry, it is unfortunate in Rust codegen use bool, int or unit, string type is not implemented (raised at lib/codegen/rust/rustexpr.ml: line 38)
  [124]
