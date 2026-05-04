Duplicate labels with disjoint payload guards and extra state-dependent refinement should be accepted.
  $ nuscr --project B@Dup Dup.nuscr
  PrevMsg(y: int) from A;
  choice at A {
    Msg(x: (x:int{x = 0 && x > y})) from A;
    (end)
  } or {
    Msg(x: (x:int{x <> 0})) from A;
    (end)
  }
