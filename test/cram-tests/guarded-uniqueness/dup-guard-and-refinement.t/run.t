Duplicate labels where only one branch has a refinement should be rejected.
  $ nuscr --project B@Dup Dup.nuscr
  PrevMsg(y: int) from A;
  choice at A {
    Msg(x: (x:int{x = 0 && x > y})) from A;
    (end)
  } or {
    Msg(x: (x:int{x <> 0})) from A;
    (end)
  }
