Example1 should be projectable.

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
  


(BROKEN)
Example2 should also be projectable.

  $ nuscr --project A@Example2 Loop2.nuscr
  nuscr: Reported problem:
          ("Stack overflow")
  [124]
  $ nuscr --project B@Example2 Loop2.nuscr
  nuscr: Reported problem:
          ("Stack overflow")
  [124]
  $ nuscr --project C@Example2 Loop2.nuscr
  nuscr: Reported problem:
          ("Stack overflow")
  [124]

Example3 should also be projectable.

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
  

