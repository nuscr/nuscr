TODO: Currently the protocol cannot be merged for C.
  $ nuscr --project C@Merge Merge.nuscr
  nuscr: User error: Unable to merge:
         (Ltype.SilentI ({ Loc.loc = 5:13 to 5:14 in: Merge.nuscr; value = "x" }, int,
            <not evaluated>))
         and
         (Ltype.SilentI ({ Loc.loc = 8:13 to 8:14 in: Merge.nuscr; value = "y" }, int,
            <not evaluated>))
  [124]
