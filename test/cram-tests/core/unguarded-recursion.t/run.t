Unguarded recursion in global protocol should raise an error.

  $ nuscr --show-global-type Unguarded UnguardedSimple.nuscr
  nuscr: User error: Unguarded type variable X at 3:14 to 3:15 in: UnguardedSimple.nuscr
  [1]

  $ nuscr --show-global-type Unguarded UnguardedDo.nuscr
  nuscr: User error: Unguarded type variable __Unguarded_A_B at 2:3 to 2:22 in: UnguardedDo.nuscr
  [1]


Although we can remove the unguarded recursion and the protocol still makes some sense, we will still report this error.
  $ nuscr --show-global-type Unguarded UnguardedChoice.nuscr
  nuscr: User error: Unguarded type variable X at 4:16 to 4:17 in: UnguardedChoice.nuscr
  [1]
