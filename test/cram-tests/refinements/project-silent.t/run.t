Project Relay protocol onto C should contain silent prefix with variable x.
  $ nuscr --project C@Relay Relay.nuscr
  (silent) x(int);
  Second(y: y:int{x = y}) from B;
  Third(z: z:int{x = z}) to D;
  (end)


Project Relay protocol onto D should contain silent prefix with variable x and y.
  $ nuscr --project D@Relay Relay.nuscr
  (silent) x(int);
  (silent) y(y:int{x = y});
  Third(z: z:int{x = z}) from C;
  (end)

When SenderValidateRefinements is turned on, C cannot validate refinements and an error is
reported.
  $ nuscr --project D@Relay Relay2.nuscr
  nuscr: User error: Role C does not know the value of the variable x
  [124]
