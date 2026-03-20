Duplicate labels with different variable names should be rejected even if guards would be disjoint.
  $ nuscr --project B@Dup Dup.nuscr
  nuscr: User error: Guarded choice error for label Msg at 7:5 to 7:8 in: Dup.nuscr: payloads must match in arity, variable names, and base types across all branches sharing the same label
  [124]
