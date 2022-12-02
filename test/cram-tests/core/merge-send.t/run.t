We should be able to merge continuations of sending prefixes:
  $ nuscr --project C@Merging Merging.nuscr
  Baz() to B;
  choice at B {
    Bar() from B;
    (end)
  } or {
    Foo() from B;
    (end)
  }
