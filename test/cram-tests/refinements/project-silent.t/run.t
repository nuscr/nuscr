Project Relay protocol onto C should contain silent prefix with variable x.
  $ nuscr --project C@Relay Relay.nuscr
  (silent) x(int);
  Second(y: y:int{(x)=(y)}) from B;
  Third(z: z:int{(x)=(z)}) to D;
  end
  

Project Relay protocol onto D should contain silent prefix with variable x and y.
  $ nuscr --project D@Relay Relay.nuscr
  (silent) x(int);
  (silent) y(y:int{(x)=(y)});
  Third(z: z:int{(x)=(z)}) from C;
  end
  
