nuscr rejects BadInit: rec var init 0 does not satisfy total < 0
  $ nuscr --gencode-rust=C@BadInit BadInit.nuscr
  nuscr: User error: Type Error: Expression 0 should be of type (total:int{total < 0})
  [124]
