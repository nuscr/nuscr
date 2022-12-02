We should be able to merge continuations of sending prefixes:
  $ nuscr --project C@Merging Merging.nuscr
  nuscr: User error: Unable to merge: Baz() to B;
         Foo() from B;
         (end)
         and
         Baz() to B;
         Bar() from B;
         (end)
         when projecting on role C
  [124]
