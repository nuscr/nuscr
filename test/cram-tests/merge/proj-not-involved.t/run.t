(BROKEN)
Since B is not involved in the loop, the protocol should be projectable.

  $ nuscr --project A@Example Loop.nuscr
  nuscr: User error: Unable to merge: continue Loop;
          end
         
  [1]
  $ nuscr --project B@Example Loop.nuscr
  nuscr: User error: Unable to merge: continue Loop;
          end
         
  [1]
  $ nuscr --project C@Example Loop.nuscr
  nuscr: User error: Unable to merge: continue Loop;
          end
         
  [1]
