Duplicate labels with different variable names should be rejected even if guards would be disjoint.
  $ nuscr --project B@Dup Dup.nuscr
  nuscr: User error: Duplicate label Msg in choices at <unknown>
  [124]
