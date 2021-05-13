An invalid pragma should raise a user error.
(Not an internal error).

  $ nuscr --project A@Hello Invalid.nuscr
  nuscr: User error: Unknown pragma: ThisPragmaShouldNotExist
  [1]
