The protocol with recursion variable should be well-formed.
  $ nuscr Recursion.nuscr --project A@Recursion1
  rec X {
    Num(curr: curr:int{(curr)=(count)}) to B;
    continue X;
  }
  

  $ nuscr Recursion.nuscr --project A@Recursion2
  rec X {
    Num(curr: curr:int{(curr)=(count)}) to B;
    continue X;
  }
  
