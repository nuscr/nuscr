Print FSM for C@Contract
  $ nuscr --fsm C@Contract Contract.nuscr
  digraph G {
    0;
    1;
    4;
    5;
    7;
    9;
    10;
    13;
    15;
    
    
    0 -> 1 [label="P!propose(initialPrice: int)", ];
    1 -> 4 [label="P?accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(currentPrice)})",
            ];
    1 -> 7 [label="P?counter(counterPrice: counterPrice:int{(counterPrice)<>(currentPrice)})",
            ];
    1 -> 15 [label="P?reject()", ];
    4 -> 5 [label="P!confirm()", ];
    7 -> 1 [label="P!counter(newCounterPrice: newCounterPrice:int{(counterPrice)<>(newCounterPrice)})[newCounterPrice]",
            ];
    7 -> 9 [label="P!accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(counterPrice)})",
            ];
    7 -> 13 [label="P!reject()", ];
    9 -> 10 [label="P?confirm()", ];
    
    }
Print State Variables:
  $ nuscr --gencode-fstar C@Contract Contract.nuscr
  noeq type state0 =
  unit
  noeq type state1 =
  {
  initialPrice: (int);
  currentPrice: (int)
  }
  
  noeq type state4 =
  {
  initialPrice: (int);
  currentPrice: (int);
  confirmedPrice: (confirmedPrice:int{(confirmedPrice)=(currentPrice)})
  }
  
  noeq type state5 =
  {
  initialPrice: (int);
  currentPrice: (int);
  confirmedPrice: (confirmedPrice:int{(confirmedPrice)=(currentPrice)})
  }
  
  noeq type state7 =
  {
  initialPrice: (int);
  currentPrice: (int);
  counterPrice: (counterPrice:int{(counterPrice)<>(currentPrice)})
  }
  
  noeq type state9 =
  {
  initialPrice: (int);
  currentPrice: (int);
  counterPrice: (counterPrice:int{(counterPrice)<>(currentPrice)});
  confirmedPrice: (confirmedPrice:int{(confirmedPrice)=(counterPrice)})
  }
  
  noeq type state10 =
  {
  initialPrice: (int);
  currentPrice: (int);
  counterPrice: (counterPrice:int{(counterPrice)<>(currentPrice)});
  confirmedPrice: (confirmedPrice:int{(confirmedPrice)=(counterPrice)})
  }
  
  noeq type state13 =
  {
  initialPrice: (int);
  currentPrice: (int);
  counterPrice: (counterPrice:int{(counterPrice)<>(currentPrice)})
  }
  
  noeq type state15 =
  {
  initialPrice: (int);
  currentPrice: (int)
  }
  
  noeq type state0Choice (st: state0) =
  | Choice0propose of int
  noeq type state4Choice (st: state4) =
  | Choice4confirm of unit
  noeq type state7Choice (st: state7) =
  | Choice7reject of unit
  | Choice7accept of confirmedPrice:int{(confirmedPrice)=((Mkstate7?.counterPrice st))}
  | Choice7counter of newCounterPrice:int{((Mkstate7?.counterPrice st))<>(newCounterPrice)}
  type roles =
  | P
  noeq type callbacks =
  {
  state0Send: (st: state0) -> ML (state0Choice st);
  state1Recvaccept: (st: state1) -> (confirmedPrice:int{(confirmedPrice)=((Mkstate1?.currentPrice st))}) -> ML unit;
  state1Recvcounter: (st: state1) -> (counterPrice:int{(counterPrice)<>((Mkstate1?.currentPrice st))}) -> ML unit;
  state1Recvreject: (st: state1) -> (unit) -> ML unit;
  state4Send: (st: state4) -> ML (state4Choice st);
  state7Send: (st: state7) -> ML (state7Choice st);
  state9Recvconfirm: (st: state9) -> (unit) -> ML unit
  }
  
  nuscr: Reported problem:
          "Assert_failure lib/fstarcodegen.ml:259:2"
  [1]
