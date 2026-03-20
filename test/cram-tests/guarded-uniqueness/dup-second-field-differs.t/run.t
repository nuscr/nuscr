First field guards are disjoint, second field guards overlap. The combined
guards (x=0 && y>=0) vs (x<>0 && y>=0) are still disjoint because x
distinguishes them. Should be accepted.
  $ nuscr --project B@Dup Dup.nuscr
  choice at A {
    Msg(x: (x:int{x = 0}), y: (y:int{y >= 0})) from A;
    (end)
  } or {
    Msg(x: (x:int{x <> 0}), y: (y:int{y >= 0})) from A;
    (end)
  }
