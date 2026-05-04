Rust codegen rejects reused labels whose occurrences would require different
Rust Action payload fields.
  $ nuscr --gencode-rust-test=C@Reuse Reuse.nuscr > C_monitor.rs
  nuscr: User error: Rust codegen cannot reuse label 'msg' with incompatible
         payload fields at 4:3 to 4:6 in: Reuse.nuscr; every occurrence of a
         label must have the same Rust field names and base types
  [124]

  $ nuscr --gencode-rust=C@Reuse Reuse.nuscr
  nuscr: User error: Rust codegen cannot reuse label 'msg' with incompatible
         payload fields at 4:3 to 4:6 in: Reuse.nuscr; every occurrence of a
         label must have the same Rust field names and base types
  [124]
