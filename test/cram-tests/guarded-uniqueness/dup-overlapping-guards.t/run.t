Duplicate labels with overlapping guards should be rejected (x=0 satisfies both).
  $ nuscr --project B@Dup Dup.nuscr
  nuscr: User error: Duplicate label Msg in choices at 7:5 to 7:8 in: Dup.nuscr
  [124]
