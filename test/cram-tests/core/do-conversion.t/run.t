Evidently recursive `do` can be converted into `rec`.

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
  
