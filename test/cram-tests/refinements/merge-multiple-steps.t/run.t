Merging should succeed after 1 step of silent prefix for C and 2 steps for D.
  $ nuscr --project C@MergeMulti Merge.nuscr
  nuscr: User error: Unable to merge:
         (Ltype.SilentI ({ Loc.loc = 5:9 to 5:10 in: Merge.nuscr; value = "x" }, int,
            <not evaluated>))
         and
         (Ltype.SilentI ({ Loc.loc = 9:9 to 9:10 in: Merge.nuscr; value = "y" }, int,
            <not evaluated>))
  [124]
  $ nuscr --project D@MergeMulti Merge.nuscr
  nuscr: User error: Unable to merge:
         (Ltype.SilentI ({ Loc.loc = 5:9 to 5:10 in: Merge.nuscr; value = "x" }, int,
            <not evaluated>))
         and
         (Ltype.SilentI ({ Loc.loc = 9:9 to 9:10 in: Merge.nuscr; value = "y" }, int,
            <not evaluated>))
  [124]
