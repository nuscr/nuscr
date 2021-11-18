TODO: Currently the protocol cannot be merged for C.
  $ nuscr --project C@Merge Merge.nuscr
  choice at B {
    (silent) y(int);
    Two() from B;
    end
  } or {
    (silent) x(int);
    One() from B;
    end
  }
  
