C gets confused by the Confuse protocol, since they cannot determine whether A
takes Foo branch or Bar branch.

  $ nuscr --project C@Confuse Confuse.nuscr
  nuscr: User error: Unable to merge: (silent) x(int);
         One(xx: int) from A;
         (end)
         and
         (silent) y(int);
         One(xx: int) from A;
         (end)
  [124]

  $ nuscr --project C@Confuse AlsoConfuse.nuscr
  nuscr: User error: Unable to merge: (silent) x(int);
         (silent) xxx(int);
         One(xx: int) from A;
         (end)
         and
         (silent) y(int);
         One(xx: int) from A;
         (end)
  [124]

C cannot receive from either A or B, so they get very confused.

  $ nuscr --project C@Confuse VeryConfuse.nuscr
  nuscr: User error: Unable to merge: (silent) x(int);
         (silent) xxx(int);
         One(xx: int) from B;
         (end)
         and
         (silent) y(int);
         One(xx: int) from A;
         (end)
  [124]

C doesn't get confused when they can be made aware of A's choice (directly).

  $ nuscr --project C@Clear Clear.nuscr
  choice at A {
    (silent) x(int);
    (silent) xxx(int);
    One(xx: int) from A;
    (end)
  } or {
    (silent) y(int);
    Two(yy: int) from A;
    (end)
  }


C doesn't get confused when they can be made aware of A's choice (indirectly).

  $ nuscr --project C@Clear Clear.nuscr
  choice at A {
    (silent) x(int);
    (silent) xxx(int);
    One(xx: int) from A;
    (end)
  } or {
    (silent) y(int);
    Two(yy: int) from A;
    (end)
  }

