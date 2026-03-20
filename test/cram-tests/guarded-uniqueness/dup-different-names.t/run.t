Duplicate labels with different variable names should be rejected even if guards would be disjoint.
  $ nuscr --project B@Dup Dup.nuscr
  nuscr: User error: Duplicate label Msg in choices at 7:5 to 7:8 in: Dup.nuscr
  [124]
