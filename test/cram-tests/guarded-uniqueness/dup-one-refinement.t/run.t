Duplicate labels where only one branch has a refinement should be rejected.
  $ nuscr --project B@Dup Dup.nuscr
  nuscr: User error: Duplicate label Msg in choices at 7:5 to 7:8 in: Dup.nuscr
  [124]
