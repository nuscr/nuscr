Since B is not involved in the loop, the protocol should be projectable.

  $ nuscr --project A@Example Loop.nuscr
  Foo() to B;
  rec Loop {
    choice at A {
      Bar() to C;
      continue Loop;
    } or {
      Baz() to C;
      continue Loop;
    } or {
      Qux() to C;
      end
    }
  }
  

  $ nuscr --project B@Example Loop.nuscr
  Foo() from A;
  end
  

  $ nuscr --project C@Example Loop.nuscr
  rec Loop {
    choice at A {
      Bar() from A;
      continue Loop;
    } or {
      Baz() from A;
      continue Loop;
    } or {
      Qux() from A;
      end
    }
  }
  

