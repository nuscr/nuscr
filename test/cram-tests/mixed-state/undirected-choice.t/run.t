Undirected choice should be valid, with B and C well-informed.

  $ nuscr --project A@Undirected Undirected.nuscr
  choice at A {
    Hi() to B;
    Hello() to C;
    end
  } or {
    Hello() to B;
    Hi() to C;
    end
  }
  
