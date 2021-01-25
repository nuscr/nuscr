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
  type state0 = unit
  type state1 =
  {initialPrice: (int);
  currentPrice: (int)}
  
  type state4 =
  {initialPrice: (int);
  currentPrice: (int);
  confirmedPrice: (confirmedPrice:int{(confirmedPrice)=(currentPrice)})}
  
  type state5 =
  {initialPrice: (int);
  currentPrice: (int);
  confirmedPrice: (confirmedPrice:int{(confirmedPrice)=(currentPrice)})}
  
  type state7 =
  {initialPrice: (int);
  currentPrice: (int);
  counterPrice: (counterPrice:int{(counterPrice)<>(currentPrice)})}
  
  type state9 =
  {initialPrice: (int);
  currentPrice: (int);
  counterPrice: (counterPrice:int{(counterPrice)<>(currentPrice)});
  confirmedPrice: (confirmedPrice:int{(confirmedPrice)=(counterPrice)})}
  
  type state10 =
  {initialPrice: (int);
  currentPrice: (int);
  counterPrice: (counterPrice:int{(counterPrice)<>(currentPrice)});
  confirmedPrice: (confirmedPrice:int{(confirmedPrice)=(counterPrice)})}
  
  type state13 =
  {initialPrice: (int);
  currentPrice: (int);
  counterPrice: (counterPrice:int{(counterPrice)<>(currentPrice)})}
  
  type state15 =
  {initialPrice: (int);
  currentPrice: (int)}
  
  nuscr: Reported problem:
          "Assert_failure lib/fstarcodegen.ml:109:2"
  [1]
