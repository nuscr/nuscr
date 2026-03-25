Rust codegen for role with silent recursion variable should report UnImplemented
  $ nuscr --gencode-rust-test=A@SilentCounter SilentCounter.nuscr
  nuscr: I'm sorry, it is unfortunate Rust codegen for silent recursion variable 'counter' is not implemented (raised at lib/codegen/rust/rustefsm.ml: line 25)
  [124]

  $ nuscr --gencode-rust=A@SilentCounter SilentCounter.nuscr
  nuscr: I'm sorry, it is unfortunate Rust codegen for silent recursion variable 'counter' is not implemented (raised at lib/codegen/rust/rustefsm.ml: line 25)
  [124]
