Unbound role name in messages are not allowed.
  $ nuscr --project A@Unbound UnboundRole.nuscr
  nuscr: User error: Unbound role C at 4:19 to 4:20 in: UnboundRole.nuscr
  [124]

  $ nuscr UnboundRoleMore.nuscr
  nuscr: Reported problem:
          (Not_found_s ("Map.find_exn: not found" T))
  [124]
