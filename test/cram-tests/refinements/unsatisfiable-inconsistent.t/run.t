Nuscr should not report an error if ValidateRefinementSatisfiability is not
enabled.
  $ nuscr UnsatisfiableDisabled.nuscr --project A@Unsat
  Hello(x: x:int{(x)>(0)}) to B;
  Bye(y: y:int{((y)>(x))&&((y)<(0))}) from B;
  end
  

Nuscr should report an error if ValidateRefinementSatisfiability is enabled.
  $ nuscr Unsatisfiable.nuscr --project A@Unsat
  nuscr: User error: Refinements cannot be satisfied
  [1]
