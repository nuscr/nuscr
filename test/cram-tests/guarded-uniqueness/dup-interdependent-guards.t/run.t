Interdependent guards: branch 1 has x=0, y=x (so y=0), branch 2 has x<>0, y<>0.
Combined guard for branch 1: x=0 && y=x. Combined for branch 2: x<>0 && y<>0.
These are disjoint (no x,y satisfies both). Should be accepted.
  $ nuscr --project B@Dup Dup.nuscr
  choice at A {
    Msg(x: (x:int{x = 0}), y: (y:int{y = x})) from A;
    (end)
  } or {
    Msg(x: (x:int{x <> 0}), y: (y:int{y <> 0})) from A;
    (end)
  }
