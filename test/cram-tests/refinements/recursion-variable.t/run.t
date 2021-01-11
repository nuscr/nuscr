The protocol with recursion variable should be well-formed.
  $ nuscr Recursion.nuscr --project A@Recursion1 --show_solver_queries
  (declare-const count Int)
  (declare-const freshvar$2 Int)
  (assert (not(>= count 0)))
  (assert (= freshvar$2 0))
  (assert (= freshvar$2 count))
  (check-sat)
  
  (declare-const count Int)
  (declare-const curr Int)
  (declare-const freshvar$3 Int)
  (assert (not(>= count 0)))
  (assert (= freshvar$3(+ count 1)))
  (assert (= freshvar$3 count))
  (assert (= curr count))
  (assert (>= count 0))
  (check-sat)
  
  rec X {
    Num(curr: curr:int{(curr)=(count)}) to B;
    continue X;
  }
  


  $ nuscr Recursion.nuscr --project A@Recursion2 --show_solver_queries
  (declare-const count Int)
  (declare-const freshvar$2 Int)
  (assert (not(>= count 0)))
  (assert (= freshvar$2 0))
  (assert (= freshvar$2 count))
  (check-sat)
  
  (declare-const count Int)
  (declare-const curr Int)
  (declare-const freshvar$3 Int)
  (assert (not(>= count 0)))
  (assert (= freshvar$3(+ count 1)))
  (assert (= freshvar$3 count))
  (assert (= curr count))
  (assert (>= count 0))
  (check-sat)
  
  rec X {
    Num(curr: curr:int{(curr)=(count)}) to B;
    continue X;
  }
  
