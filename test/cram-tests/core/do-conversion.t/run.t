Evidently recursive `do` can be converted into `rec` for binary protocols.

  $ nuscr --show-global-type PingPong Do1.nuscr
  rec __PingPong_A_B {
    Ping() from A to B;
    Pong() from B to A;
    continue __PingPong_A_B;
  }
  





Do with different role arguments should expand the protocol and construct a fix point.
  $ nuscr --show-global-type PingPong Do2.nuscr
  rec __PingPong_A_B {
    Ping() from A to B;
    Pong() from B to A;
    Ping() from B to A;
    Pong() from A to B;
    continue __PingPong_A_B;
  }
  





Both protocols should be projectable.

  $ nuscr --project A@PingPong Do1.nuscr
  rec __PingPong_A_B {
    Ping() to B;
    Pong() from B;
    continue __PingPong_A_B;
  }
  



  $ nuscr --project A@PingPong Do2.nuscr
  rec __PingPong_A_B {
    Ping() to B;
    Pong() from B;
    Ping() from B;
    Pong() to B;
    continue __PingPong_A_B;
  }
  




It should be also the case for 3 roles.

  $ nuscr --show-global-type Pass DoMulti1.nuscr
  rec __Pass_A_B_C {
    choice at A {
      Foo() from A to B;
      Foo() from B to C;
      continue __Pass_A_B_C;
    } or {
      Bar() from A to B;
      Bar() from B to C;
      continue __Pass_A_B_C;
    }
  }
  



Similar for non-identical calls.

  $ nuscr --show-global-type Pass DoMulti2.nuscr
  rec __Pass_A_B_C {
    choice at A {
      Foo() from A to B;
      Foo() from B to C;
      choice at B {
        Foo() from B to C;
        Foo() from C to A;
        choice at C {
          Foo() from C to A;
          Foo() from A to B;
          continue __Pass_A_B_C;
        } or {
          Bar() from C to A;
          Bar() from A to B;
          continue __Pass_A_B_C;
        }
      } or {
        Bar() from B to C;
        Bar() from C to A;
        choice at C {
          Foo() from C to A;
          Foo() from A to B;
          continue __Pass_A_B_C;
        } or {
          Bar() from C to A;
          Bar() from A to B;
          continue __Pass_A_B_C;
        }
      }
    } or {
      Bar() from A to B;
      Bar() from B to C;
      choice at B {
        Foo() from B to C;
        Foo() from C to A;
        choice at C {
          Foo() from C to A;
          Foo() from A to B;
          continue __Pass_A_B_C;
        } or {
          Bar() from C to A;
          Bar() from A to B;
          continue __Pass_A_B_C;
        }
      } or {
        Bar() from B to C;
        Bar() from C to A;
        choice at C {
          Foo() from C to A;
          Foo() from A to B;
          continue __Pass_A_B_C;
        } or {
          Bar() from C to A;
          Bar() from A to B;
          continue __Pass_A_B_C;
        }
      }
    }
  }
  


  $ nuscr --show-global-type Pass DoMulti3.nuscr
  rec __Pass_A_B_C {
    choice at A {
      Foo() from A to B;
      Foo() from B to C;
      rec __Pass_B_C_A {
        choice at B {
          Foo() from B to C;
          Foo() from C to A;
          choice at C {
            Foo() from C to A;
            Foo() from A to B;
            continue __Pass_A_B_C;
          } or {
            Bar() from C to A;
            Bar() from A to B;
            continue __Pass_B_C_A;
          }
        } or {
          Bar() from B to C;
          Bar() from C to A;
          continue __Pass_A_B_C;
        }
      }
    } or {
      Bar() from A to B;
      Bar() from B to C;
      rec __Pass_C_A_B {
        choice at C {
          Foo() from C to A;
          Foo() from A to B;
          continue __Pass_A_B_C;
        } or {
          Bar() from C to A;
          Bar() from A to B;
          choice at B {
            Foo() from B to C;
            Foo() from C to A;
            continue __Pass_C_A_B;
          } or {
            Bar() from B to C;
            Bar() from C to A;
            continue __Pass_A_B_C;
          }
        }
      }
    }
  }
  
