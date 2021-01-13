Role C does not know the variable `curr` because it is between A and B.
Therefore an error is expected.
  $ nuscr --project C@Wrong Wrong.nuscr
  nuscr: User error: Role C does not know the value of the variable curr
  [1]
