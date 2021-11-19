Merging should succeed after 1 step of silent prefix for C and 2 steps for D.
  $ nuscr --project C@MergeMulti Merge.nuscr
  choice at B {
    (silent) x(int);
    One(xx: int) from B;
    One(xxx: int) to D;
    end
  } or {
    (silent) y(int);
    Two(yy: int) from B;
    Two(yyy: int) to D;
    end
  }
  
  $ nuscr --project D@MergeMulti Merge.nuscr
  choice at C {
    (silent) x(int);
    (silent) xx(int);
    One(xxx: int) from C;
    end
  } or {
    (silent) y(int);
    (silent) yy(int);
    Two(yyy: int) from C;
    end
  }
  
