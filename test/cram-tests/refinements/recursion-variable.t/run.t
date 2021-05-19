The protocol with recursion variable should be well-formed.
  $ nuscr Recursion.nuscr --project A@Recursion1 --show-solver-queries
  (declare-const count Int)
  (declare-const freshvar$2 Int)
  (assert (not (>= count 0)))
  (assert (= freshvar$2 0))
  (assert (= freshvar$2 count))
  (check-sat)
  
  (declare-const count Int)
  (declare-const curr Int)
  (declare-const freshvar$3 Int)
  (assert (not (>= count 0)))
  (assert (= freshvar$3 (+ count 1)))
  (assert (= freshvar$3 count))
  (assert (= curr count))
  (assert (>= count 0))
  (check-sat)
  
  rec X [count<A>: int = 0] {
    Num(curr: curr:int{(curr)=(count)}) to B;
    continue X [(count)+(1)];
  }
  



  $ nuscr Recursion.nuscr --project A@Recursion2 --show-solver-queries
  (declare-const count Int)
  (declare-const freshvar$2 Int)
  (assert (not (>= count 0)))
  (assert (= freshvar$2 0))
  (assert (= freshvar$2 count))
  (check-sat)
  
  (declare-const count Int)
  (declare-const curr Int)
  (declare-const freshvar$3 Int)
  (assert (not (>= count 0)))
  (assert (= freshvar$3 (+ count 1)))
  (assert (= freshvar$3 count))
  (assert (= curr count))
  (assert (>= count 0))
  (check-sat)
  
  rec X [count<A>: count:int{(count)>=(0)} = 0] {
    Num(curr: curr:int{(curr)=(count)}) to B;
    continue X [(count)+(1)];
  }
  

When projected on B, the recursion variable `count` should not appear.

  $ nuscr Recursion.nuscr --project B@Recursion1
  rec X [(silent) count<A>: int = 0] {
    Num(curr: curr:int{(curr)=(count)}) from A;
    continue X;
  }
  

  $ nuscr Recursion.nuscr --project B@Recursion2
  rec X [(silent) count<A>: count:int{(count)>=(0)} = 0] {
    Num(curr: curr:int{(curr)=(count)}) from A;
    continue X;
  }
  
