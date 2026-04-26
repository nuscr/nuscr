Progress check with recursion variables: the rec var invariant must be
included in the SMT environment when checking progress across MuG boundaries.

Stuck with choice: both branches require n > 0, but the invariant allows n = 0.
  $ nuscr Stuck.nuscr --project A@Stuck --show-solver-queries
  (declare-const n Int)
  (declare-const x Int)
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 1)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 0)) true))))
  (assert (>= x 0))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  nuscr: User error: Protocol may be stuck due to refinements
  [124]


Stuck without choice: single branch requires n > 0, but invariant allows n = 0.
  $ nuscr StuckNoChoice.nuscr --project A@Stuck --show-solver-queries
  (declare-const n Int)
  (declare-const x Int)
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 0)) true))))
  (assert (>= x 0))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  nuscr: User error: Protocol may be stuck due to refinements
  [124]


Fixed: adding a Zero branch covering n = 0 makes progress hold.
  $ nuscr Fixed.nuscr --project A@Stuck --show-solver-queries
  (declare-const n Int)
  (declare-const x Int)
  (assert (not (exists ((v Int)) (and (and (= v n) (= v 0)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 1)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 0)) true))))
  (assert (>= x 0))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  (declare-const freshvar$0 Int)
  (declare-const n Int)
  (declare-const x Int)
  (assert (not (and (>= n 0) (<= n x))))
  (assert (= freshvar$0 0))
  (assert (= freshvar$0 n))
  (assert (>= x 0))
  (check-sat)
  
  (declare-const n Int)
  (declare-const v Int)
  (declare-const x Int)
  (assert (not (exists ((v Int)) (and (and (= v n) (= v 0)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 1)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 0)) true))))
  (assert (>= x 0))
  (assert (and (= v n) (> v 0)))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  (declare-const freshvar$1 Int)
  (declare-const n Int)
  (declare-const v Int)
  (declare-const x Int)
  (assert (not (and (>= n 0) (<= n x))))
  (assert (= freshvar$1 (+ n 1)))
  (assert (= freshvar$1 n))
  (assert (>= x 0))
  (assert (and (= v n) (> v 0)))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  (declare-const n Int)
  (declare-const v Int)
  (declare-const x Int)
  (assert (not (exists ((v Int)) (and (and (= v n) (= v 0)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 1)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 0)) true))))
  (assert (>= x 0))
  (assert (and (= v n) (> v 1)))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  (declare-const freshvar$2 Int)
  (declare-const n Int)
  (declare-const v Int)
  (declare-const x Int)
  (assert (not (and (>= n 0) (<= n x))))
  (assert (= freshvar$2 (+ n 1)))
  (assert (= freshvar$2 n))
  (assert (>= x 0))
  (assert (and (= v n) (> v 1)))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  (declare-const n Int)
  (declare-const v Int)
  (declare-const x Int)
  (assert (not (exists ((v Int)) (and (and (= v n) (= v 0)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 1)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 0)) true))))
  (assert (>= x 0))
  (assert (and (= v n) (= v 0)))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  (declare-const freshvar$3 Int)
  (declare-const n Int)
  (declare-const v Int)
  (declare-const x Int)
  (assert (not (and (>= n 0) (<= n x))))
  (assert (= freshvar$3 (+ n 1)))
  (assert (= freshvar$3 n))
  (assert (>= x 0))
  (assert (and (= v n) (= v 0)))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  (declare-const n Int)
  (declare-const x Int)
  (assert (not (exists ((v Int)) (and (and (= v n) (= v 0)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 1)) true))))
  (assert (not (exists ((v Int)) (and (and (= v n) (> v 0)) true))))
  (assert (>= x 0))
  (assert (and (>= n 0) (<= n x)))
  (check-sat)
  
  Init(x: (x:int{x >= 0})) to B;
  rec loop [n<A, B>: (n:int{n >= 0 && n <= x}) = 0] {
    choice at B {
      High(v: (v:int{v = n && v > 0})) from B;
      continue loop [n + 1];
    } or {
      Low(v: (v:int{v = n && v > 1})) from B;
      continue loop [n + 1];
    } or {
      Zero(v: (v:int{v = n && v = 0})) from B;
      continue loop [n + 1];
    }
  }









