Merging across recursion (#137).

  $ nuscr --project A@Example1 Loop1.nuscr
  rec Loop {
    Foo() to B;
    choice at A {
      Bar() to B;
      continue Loop;
    } or {
      continue Loop;
    }
  }

  $ nuscr --project B@Example1 Loop1.nuscr
  rec Loop {
    Foo() from A;
    choice at A {
      Bar() from A;
      continue Loop;
    } or {
      continue Loop;
    }
  }

  $ nuscr --project A@Example2 Loop2.nuscr
  rec Loop {
    Foo() to B;
    choice at C {
      Bar() from C;
      Baz() to B;
      continue Loop;
    } or {
      Qux() from C;
      continue Loop;
    }
  }

  $ nuscr --project B@Example2 Loop2.nuscr
  rec Merge0 {
    Foo() from A;
    rec Merge1 {
      choice at A {
        Baz() from A;
        continue Merge0;
      } or {
        Foo() from A;
        continue Merge1;
      }
    }
  }

  $ nuscr --project C@Example2 Loop2.nuscr
  rec Loop {
    choice at C {
      Bar() to A;
      continue Loop;
    } or {
      Qux() to A;
      continue Loop;
    }
  }

  $ nuscr --project A@Example3 Loop3.nuscr
  rec Loop {
    Foo() to B;
    choice at C {
      Bar() from C;
      Baz() to B;
      continue Loop;
    } or {
      Qux() from C;
      Dummy() to B;
      continue Loop;
    }
  }

  $ nuscr --project B@Example3 Loop3.nuscr
  rec Loop {
    Foo() from A;
    choice at A {
      Dummy() from A;
      continue Loop;
    } or {
      Baz() from A;
      continue Loop;
    }
  }

  $ nuscr --project C@Example3 Loop3.nuscr
  rec Loop {
    choice at C {
      Bar() to A;
      continue Loop;
    } or {
      Qux() to A;
      continue Loop;
    }
  }

  $ nuscr --fsm B@Example2 Loop2.nuscr > loop.dot
  $ sed '/^[[:space:]]*$/d' loop.dot
  digraph G {
    0;
    2;
    0 -> 2 [label="A?Foo()", ];
    2 -> 0 [label="A?Baz()", ];
    2 -> 2 [label="A?Foo()", ];
    }

  $ nuscr --project C@Proto Coinductive.nuscr
  a() from A;
  rec Merge1 {
    b() from A;
    a() from A;
    continue Merge1;
  }

  $ nuscr --project C@Proto2 Coinductive.nuscr
  rec Merge0 {
    a() from A;
    b() from A;
    a() from A;
    b() from A;
    continue Merge0;
  }

Shared labels merge continuations while keeping distinct recursive binders.

  $ nuscr --project B@SameLabel SameLabel.nuscr
  Tick() from A;
  choice at A {
    Left() from A;
    rec Merge2 {
      Tick() from A;
      Left() from A;
      continue Merge2;
    }
  } or {
    Right() from A;
    rec Merge4 {
      Tick() from A;
      Right() from A;
      continue Merge4;
    }
  }

  $ nuscr --fsm B@SameLabel SameLabel.nuscr > shared.dot
  $ sed '/^[[:space:]]*$/d' shared.dot
  digraph G {
    0;
    1;
    3;
    5;
    7;
    9;
    0 -> 1 [label="A?Tick()", ];
    1 -> 3 [label="A?Left()", ];
    1 -> 7 [label="A?Right()", ];
    3 -> 5 [label="A?Tick()", ];
    5 -> 3 [label="A?Left()", ];
    7 -> 9 [label="A?Tick()", ];
    9 -> 7 [label="A?Right()", ];
    }

Invalid merges must still fail, including after a shared prefix.

  $ nuscr --project B@Sends Sends.nuscr
  nuscr: User error: Unable to merge: rec X { Tick() from A; Left() to A;
         continue X; } and rec Y { Tick() from A; Right() to A; continue Y; }
         when projecting on role B
  [124]

  $ nuscr --project B@Payloads Payloads.nuscr
  nuscr: User error: Unable to merge: rec X { Tick(int) from A; continue X; }
         and rec Y { Tick(string) from A; continue Y; } when projecting on role
         B
  [124]

  $ nuscr --project B@Senders Senders.nuscr
  nuscr: User error: Unable to merge: rec X { Tick() from A; continue X; } and
         rec Y { Tock() from C; continue Y; } when projecting on role B
  [124]

  $ nuscr --project B@Invisible Invisible.nuscr
  nuscr: User error: Unable to merge: (end) and Tock() from A; (end) when
         projecting on role B
  [124]

Unfolding parameterised recursion retains the existing error.

  $ nuscr --project B@Parameters Parameters.nuscr
  nuscr: User error: Unable to merge: Baz() from A; continue Loop [count + 1];
         and continue Loop [count + 1]; when projecting on role B
  [124]
