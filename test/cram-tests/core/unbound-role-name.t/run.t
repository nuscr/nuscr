Unbound role name in messages are not allowed.
  $ nuscr --project A@Unbound UnboundRole.nuscr
  nuscr: User error: Unbound role C at 4:19 to 4:20 in: UnboundRole.nuscr
  [124]

  $ nuscr UnboundRoleMore.nuscr
  nuscr: User error: Unbound role T at 9:17 to 9:18 in: UnboundRoleMore.nuscr
  [124]
