C gets confused by the Confuse protocol, since they cannot determine whether A
takes Foo branch or Bar branch.

  $ nuscr --project C@Confuse Confuse.nuscr
  nuscr: User error: Unable to merge:
         (Ltype.SilentI ({ Loc.loc = 5:9 to 5:10 in: Confuse.nuscr; value = "x" },
            int, <not evaluated>))
         and
         (Ltype.SilentI ({ Loc.loc = 8:9 to 8:10 in: Confuse.nuscr; value = "y" },
            int, <not evaluated>))
  [124]

  $ nuscr --project C@Confuse AlsoConfuse.nuscr
  nuscr: User error: Unable to merge:
         (Ltype.SilentI ({ Loc.loc = 5:9 to 5:10 in: AlsoConfuse.nuscr; value = "x" },
            int, <not evaluated>))
         and
         (Ltype.SilentI ({ Loc.loc = 9:9 to 9:10 in: AlsoConfuse.nuscr; value = "y" },
            int, <not evaluated>))
  [124]

C cannot receive from either A or B, so they get very confused.

  $ nuscr --project C@Confuse VeryConfuse.nuscr
  nuscr: User error: Unable to merge:
         (Ltype.SilentI ({ Loc.loc = 5:9 to 5:10 in: VeryConfuse.nuscr; value = "x" },
            int, <not evaluated>))
         and
         (Ltype.SilentI ({ Loc.loc = 9:9 to 9:10 in: VeryConfuse.nuscr; value = "y" },
            int, <not evaluated>))
  [124]

C doesn't get confused when they can be made aware of A's choice (directly).

  $ nuscr --project C@Clear Clear.nuscr
  nuscr: User error: Unable to merge:
         (Ltype.SilentI ({ Loc.loc = 5:9 to 5:10 in: Clear.nuscr; value = "x" }, int,
            <not evaluated>))
         and
         (Ltype.SilentI ({ Loc.loc = 9:9 to 9:10 in: Clear.nuscr; value = "y" }, int,
            <not evaluated>))
  [124]


C doesn't get confused when they can be made aware of A's choice (indirectly).

  $ nuscr --project C@Clear Clear.nuscr
  nuscr: User error: Unable to merge:
         (Ltype.SilentI ({ Loc.loc = 5:9 to 5:10 in: Clear.nuscr; value = "x" }, int,
            <not evaluated>))
         and
         (Ltype.SilentI ({ Loc.loc = 9:9 to 9:10 in: Clear.nuscr; value = "y" }, int,
            <not evaluated>))
  [124]

