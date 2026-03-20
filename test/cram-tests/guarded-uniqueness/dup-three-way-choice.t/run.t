Three-way choice with pairwise disjoint guards. Should be accepted.
  $ nuscr --project B@Dup Dup.nuscr
  choice at A {
    Msg(x: (x:int{x < 0})) from A;
    (end)
  } or {
    Msg(x: (x:int{x = 0})) from A;
    (end)
  } or {
    Msg(x: (x:int{x > 0})) from A;
    (end)
  }
