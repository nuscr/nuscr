Nuscr should not report an error if ValidateRefinementProgress is not
enabled.
  $ nuscr StuckDisabled.nuscr --project A@Stuck
  Num(x: int) to B;
  choice at B {
    Pos(ignore: ignore:unit{(x)>(0)}) from B;
    end
  } or {
    Neg(ignore: ignore:unit{(x)<(0)}) from B;
    end
  }
  


Nuscr should report an error if ValidateRefinementProgress is enabled.
  $ nuscr Stuck.nuscr --project A@Stuck
  Num(x: int) to B;
  choice at B {
    Pos(ignore: ignore:unit{(x)>(0)}) from B;
    end
  } or {
    Neg(ignore: ignore:unit{(x)<(0)}) from B;
    end
  }
  
