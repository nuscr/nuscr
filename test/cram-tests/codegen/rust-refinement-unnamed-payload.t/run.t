nuscr rejects UnnamedPayload: refinement on payload with no variable binding
  $ nuscr --gencode-rust-test=C@UnnamedPayload UnnamedPayload.nuscr
  nuscr: User error: Parser error: An error occurred at 7:14 to 7:15 in: UnnamedPayload.nuscr
  [124]

  $ nuscr --gencode-rust=C@UnnamedPayload UnnamedPayload.nuscr
  nuscr: User error: Parser error: An error occurred at 7:14 to 7:15 in: UnnamedPayload.nuscr
  [124]
