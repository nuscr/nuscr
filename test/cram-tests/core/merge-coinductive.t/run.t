Projecting on C using a coinductive equality would be possible.

  $ nuscr --project C@Proto Proto.nuscr
  a() from A;
  rec X {
    b() from A;
    a() from A;
    continue X;
  }
  $ nuscr --project C@Proto2 Proto.nuscr
  rec X {
    a() from A;
    b() from A;
    a() from A;
    b() from A;
    continue X;
  }
