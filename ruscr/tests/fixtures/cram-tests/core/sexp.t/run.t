Non recursive protocols can be converted into s-expressions.
  $ nuscr --generate-sexp NonRec Rec.nuscr
  (MessageG ((label Foo) (payload ())) A B EndG)
So can the recursive one (with rec)
  $ nuscr --generate-sexp Recursive Rec.nuscr
  (MessageG ((label Foo) (payload ())) A B
   (MuG X () (MessageG ((label Foo) (payload ())) A B (TVarG X () <opaque>))))
and the recursive one (with Do)
  $ nuscr --generate-sexp RecursiveDo Rec.nuscr
  (MessageG ((label Foo) (payload ())) A B
   (MuG __RecursiveDo_A_B ()
    (MessageG ((label Foo) (payload ())) A B
     (TVarG __RecursiveDo_A_B () <opaque>))))
