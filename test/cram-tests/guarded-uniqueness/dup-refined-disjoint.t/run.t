Duplicate labels where only one branch has a refinement should be rejected.
  $ nuscr --project B@Dup Dup.nuscr
  nuscr: User error: Guarded choice error for label Msg at 7:5 to 7:8 in: Dup.nuscr: all branches sharing the same label must have a guard predicate (a refinement whose free variables are all payload-bound)
  [124]
