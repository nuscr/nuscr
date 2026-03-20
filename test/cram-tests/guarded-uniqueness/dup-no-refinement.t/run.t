Duplicate labels without refinements should be rejected even with GuardedUniqueness.
  $ nuscr --project B@Dup Dup.nuscr
  nuscr: User error: Duplicate label Msg in choices at 7:5 to 7:8 in: Dup.nuscr
  [124]
