Duplicate labels with overlapping guards should be rejected (x=0 satisfies both).
  $ nuscr --project B@Dup Dup.nuscr
  nuscr: User error: Guarded choice error for label Msg at 7:5 to 7:8 in: Dup.nuscr: guard predicates overlap: there exist payload values that satisfy multiple branches
  [124]
