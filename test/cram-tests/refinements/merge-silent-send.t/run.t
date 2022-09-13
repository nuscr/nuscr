MergeSend should fail, since C cannot tell how One or Two was used.
(At least in the current theory, maybe later we can try to do some weakening of
conditions)
  $ nuscr --project C@MergeSend Merge.nuscr
  nuscr: User error: Unable to merge:
         (Ltype.SilentI ({ Loc.loc = 5:9 to 5:10 in: Merge.nuscr; value = "x" }, int,
            <not evaluated>))
         and
         (Ltype.SilentI ({ Loc.loc = 8:9 to 8:10 in: Merge.nuscr; value = "y" }, int,
            <not evaluated>))
  [124]
