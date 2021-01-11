The protocol with recursion variable should be well-formed.
  $ nuscr Recursion.nuscr --project A@Recursion1 --show_solver_queries
  (declare-const freshvar$2 Int)
  (declare-const count Int)
  (assert (= freshvar$2 0))
  (assert (not(>= count 0)))
  (assert (= freshvar$2 count))
  (check-sat)
  
  (declare-const freshvar$3 Int)
  (declare-const count Int)
  (declare-const curr Int)
  (declare-const count Int)
  (assert (= freshvar$3(+ count 1)))
  (assert (not(>= count 0)))
  (assert (= freshvar$3 count))
  (assert (= curr count))
  (assert (>= count 0))
  (check-sat)
  
  nuscr: Internal Error: Solver error: z3 [|"-smt2"; "-in"|] in $TESTCASE_ROOT: exit 1
  [1]


  $ nuscr Recursion.nuscr --project A@Recursion2 --show_solver_queries
  (declare-const freshvar$2 Int)
  (declare-const count Int)
  (assert (= freshvar$2 0))
  (assert (not(>= count 0)))
  (assert (= freshvar$2 count))
  (check-sat)
  
  (declare-const freshvar$3 Int)
  (declare-const count Int)
  (declare-const curr Int)
  (declare-const count Int)
  (assert (= freshvar$3(+ count 1)))
  (assert (not(>= count 0)))
  (assert (= freshvar$3 count))
  (assert (= curr count))
  (assert (>= count 0))
  (check-sat)
  
  nuscr: Internal Error: Solver error: z3 [|"-smt2"; "-in"|] in $TESTCASE_ROOT: exit 1
  [1]
