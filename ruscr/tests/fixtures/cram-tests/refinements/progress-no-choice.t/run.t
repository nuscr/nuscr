Nuscr should not report an error if ValidateRefinementProgress is not
enabled.
  $ nuscr StuckDisabled.nuscr --project A@Stuck
  Num(x: (x:int{x >= 0})) to B;
  Pos(y: (y:int{y = x && y > 0})) from B;
  (end)

Nuscr should report an error if ValidateRefinementProgress is enabled.
  $ nuscr Stuck.nuscr --project A@Stuck --show-solver-queries
  (declare-const x Int)
  (assert (not (exists ((y Int)) (and (and (= y x) (> y 0)) true))))
  (assert (>= x 0))
  (check-sat)
  
  nuscr: User error: Protocol may be stuck due to refinements
  [124]

Nuscr should not report any error for the fixed protocol.
  $ nuscr Fixed.nuscr --project A@Stuck --show-solver-queries
  (declare-const x Int)
  (assert (not (exists ((y Int)) (and (and (= y x) (> y 0)) true))))
  (assert (> x 0))
  (check-sat)
  
  Num(x: (x:int{x > 0})) to B;
  Pos(y: (y:int{y = x && y > 0})) from B;
  (end)
