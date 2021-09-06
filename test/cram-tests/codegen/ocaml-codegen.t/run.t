Show EFSM for Client and Server
  $ nuscr --fsm=C@Adder Adder.nuscr
  digraph G {
    0;
    3;
    4;
    6;
    7;
    
    
    0 -> 3 [label="S!add(int)", ];
    0 -> 6 [label="S!bye()", ];
    3 -> 4 [label="S!add(int)", ];
    4 -> 0 [label="S?sum(int)", ];
    6 -> 7 [label="S?bye()", ];
    
    }
  $ nuscr --fsm=S@Adder Adder.nuscr
  digraph G {
    0;
    3;
    4;
    6;
    7;
    
    
    0 -> 3 [label="C?add(int)", ];
    0 -> 6 [label="C?bye()", ];
    3 -> 4 [label="C?add(int)", ];
    4 -> 0 [label="C!sum(int)", ];
    6 -> 7 [label="C!bye()", ];
    
    }
Generate OCaml code for Adders Client
  $ nuscr --gencode-ocaml=C@Adder Adder.nuscr > C.ml
Generate OCaml code for Adders Server
  $ nuscr --gencode-ocaml=S@Adder Adder.nuscr > S.ml
Compile
  $ dune build ./Adder.exe
