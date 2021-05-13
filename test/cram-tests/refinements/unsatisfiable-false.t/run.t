Nuscr should not report an error if ValidateRefinementSatisfiability is not
enabled.
  $ nuscr UnsatisfiableDisabled.nuscr --project A@Unsat
  Hello(x: x:int{false}) to B;
  end
  

Nuscr should report an error if ValidateRefinementSatisfiability is enabled.
  $ nuscr Unsatisfiable.nuscr --project A@Unsat
  nuscr: User error: Unknown pragma: ValidateRefinementSatisfiability
  [1]
