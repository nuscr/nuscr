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
  $ nuscr --verbose --gencode-fstar C@Contract Contract.nuscr
  { Loc.loc = 1:24 to 24:2 in: Contract.nuscr;
    value =
    { name = { Loc.loc = 3:17 to 3:25 in: Contract.nuscr; value = "Contract" };
      options = None; parameters = []; rec_parameters = [];
      roles =
      [{ Loc.loc = 3:31 to 3:32 in: Contract.nuscr; value = "C" };
        { Loc.loc = 3:39 to 3:40 in: Contract.nuscr; value = "P" }];
      split_roles =
      ([{ Loc.loc = 3:31 to 3:32 in: Contract.nuscr; value = "C" };
         { Loc.loc = 3:39 to 3:40 in: Contract.nuscr; value = "P" }],
       []);
      nested_protocols = [];
      interactions =
      [{ Loc.loc = 4:3 to 4:42 in: Contract.nuscr;
         value =
         MessageTransfer {message = propose(initialPrice: int);
           from_role =
           { Loc.loc = 4:35 to 4:36 in: Contract.nuscr; value = "C" };
           to_roles =
           [{ Loc.loc = 4:40 to 4:41 in: Contract.nuscr; value = "P" }];
           ann = None}
         };
        { Loc.loc = 5:3 to 23:4 in: Contract.nuscr;
          value =
          (Recursion (
             { Loc.loc = 5:7 to 5:11 in: Contract.nuscr; value = "Loop" },
             [{ var =
                { Loc.loc = 5:13 to 5:25 in: Contract.nuscr;
                  value = "currentPrice" };
                roles =
                [{ Loc.loc = 5:26 to 5:27 in: Contract.nuscr; value = "C" };
                  { Loc.loc = 5:29 to 5:30 in: Contract.nuscr; value = "P" }];
                ty = int;
                init =
                (Syntax.Var
                   { Loc.loc = 5:39 to 5:51 in: Contract.nuscr;
                     value = "initialPrice" })
                }
               ],
             [{ Loc.loc = 6:5 to 22:6 in: Contract.nuscr;
                value =
                (Choice (
                   { Loc.loc = 6:15 to 6:16 in: Contract.nuscr; value = "P" },
                   [[{ Loc.loc = 7:7 to 7:78 in: Contract.nuscr;
                       value =
                       MessageTransfer {
                         message =
                         accept((Syntax.Refined (
     { Loc.loc = 7:14 to 7:28 in: Contract.nuscr; value = "confirmedPrice" },
     { Loc.loc = 7:30 to 7:33 in: Contract.nuscr; value = "int" },
     (Syntax.Binop (Syntax.Eq,
        (Syntax.Var
           { Loc.loc = 7:34 to 7:48 in: Contract.nuscr;
             value = "confirmedPrice" }),
        (Syntax.Var
           { Loc.loc = 7:51 to 7:63 in: Contract.nuscr; value = "currentPrice"
             })
        ))
     )));
                         from_role =
                         { Loc.loc = 7:71 to 7:72 in: Contract.nuscr;
                           value = "P" };
                         to_roles =
                         [{ Loc.loc = 7:76 to 7:77 in: Contract.nuscr;
                            value = "C" }
                           ];
                         ann = None}
                       };
                      { Loc.loc = 8:7 to 8:29 in: Contract.nuscr;
                        value =
                        MessageTransfer {message = confirm();
                          from_role =
                          { Loc.loc = 8:22 to 8:23 in: Contract.nuscr;
                            value = "C" };
                          to_roles =
                          [{ Loc.loc = 8:27 to 8:28 in: Contract.nuscr;
                             value = "P" }
                            ];
                          ann = None}
                        }
                      ];
                     [{ Loc.loc = 10:7 to 10:76 in: Contract.nuscr;
                        value =
                        MessageTransfer {
                          message =
                          counter((Syntax.Refined (
     { Loc.loc = 10:15 to 10:27 in: Contract.nuscr; value = "counterPrice" },
     { Loc.loc = 10:29 to 10:32 in: Contract.nuscr; value = "int" },
     (Syntax.Binop (Syntax.Neq,
        (Syntax.Var
           { Loc.loc = 10:33 to 10:45 in: Contract.nuscr;
             value = "counterPrice" }),
        (Syntax.Var
           { Loc.loc = 10:49 to 10:61 in: Contract.nuscr;
             value = "currentPrice" })
        ))
     )));
                          from_role =
                          { Loc.loc = 10:69 to 10:70 in: Contract.nuscr;
                            value = "P" };
                          to_roles =
                          [{ Loc.loc = 10:74 to 10:75 in: Contract.nuscr;
                             value = "C" }
                            ];
                          ann = None}
                        };
                       { Loc.loc = 11:7 to 19:8 in: Contract.nuscr;
                         value =
                         (Choice (
                            { Loc.loc = 11:17 to 11:18 in: Contract.nuscr;
                              value = "C" },
                            [[{ Loc.loc = 12:9 to 12:80 in: Contract.nuscr;
                                value =
                                MessageTransfer {
                                  message =
                                  accept((Syntax.Refined (
     { Loc.loc = 12:16 to 12:30 in: Contract.nuscr; value = "confirmedPrice" },
     { Loc.loc = 12:32 to 12:35 in: Contract.nuscr; value = "int" },
     (Syntax.Binop (Syntax.Eq,
        (Syntax.Var
           { Loc.loc = 12:36 to 12:50 in: Contract.nuscr;
             value = "confirmedPrice" }),
        (Syntax.Var
           { Loc.loc = 12:53 to 12:65 in: Contract.nuscr;
             value = "counterPrice" })
        ))
     )));
                                  from_role =
                                  { Loc.loc = 12:73 to 12:74 in: Contract.nuscr;
                                    value = "C" };
                                  to_roles =
                                  [{ Loc.loc =
                                     12:78 to 12:79 in: Contract.nuscr;
                                     value = "P" }
                                    ];
                                  ann = None}
                                };
                               { Loc.loc = 13:9 to 13:31 in: Contract.nuscr;
                                 value =
                                 MessageTransfer {message = confirm();
                                   from_role =
                                   { Loc.loc =
                                     13:24 to 13:25 in: Contract.nuscr;
                                     value = "P" };
                                   to_roles =
                                   [{ Loc.loc =
                                      13:29 to 13:30 in: Contract.nuscr;
                                      value = "C" }
                                     ];
                                   ann = None}
                                 }
                               ];
                              [{ Loc.loc = 15:9 to 15:84 in: Contract.nuscr;
                                 value =
                                 MessageTransfer {
                                   message =
                                   counter((Syntax.Refined (
     { Loc.loc = 15:17 to 15:32 in: Contract.nuscr; value = "newCounterPrice" },
     { Loc.loc = 15:34 to 15:37 in: Contract.nuscr; value = "int" },
     (Syntax.Binop (Syntax.Neq,
        (Syntax.Var
           { Loc.loc = 15:38 to 15:50 in: Contract.nuscr;
             value = "counterPrice" }),
        (Syntax.Var
           { Loc.loc = 15:54 to 15:69 in: Contract.nuscr;
             value = "newCounterPrice" })
        ))
     )));
                                   from_role =
                                   { Loc.loc =
                                     15:77 to 15:78 in: Contract.nuscr;
                                     value = "C" };
                                   to_roles =
                                   [{ Loc.loc =
                                      15:82 to 15:83 in: Contract.nuscr;
                                      value = "P" }
                                     ];
                                   ann = None}
                                 };
                                { Loc.loc = 16:9 to 16:41 in: Contract.nuscr;
                                  value =
                                  (Continue (
                                     { Loc.loc =
                                       16:18 to 16:22 in: Contract.nuscr;
                                       value = "Loop" },
                                     [(Syntax.Var
                                         { Loc.loc =
                                           16:24 to 16:39 in: Contract.nuscr;
                                           value = "newCounterPrice" })
                                       ]
                                     ))
                                  }
                                ];
                              [{ Loc.loc = 18:9 to 18:30 in: Contract.nuscr;
                                 value =
                                 MessageTransfer {message = reject();
                                   from_role =
                                   { Loc.loc =
                                     18:23 to 18:24 in: Contract.nuscr;
                                     value = "C" };
                                   to_roles =
                                   [{ Loc.loc =
                                      18:28 to 18:29 in: Contract.nuscr;
                                      value = "P" }
                                     ];
                                   ann = None}
                                 }
                                ]
                              ]
                            ))
                         }
                       ];
                     [{ Loc.loc = 21:7 to 21:28 in: Contract.nuscr;
                        value =
                        MessageTransfer {message = reject();
                          from_role =
                          { Loc.loc = 21:21 to 21:22 in: Contract.nuscr;
                            value = "P" };
                          to_roles =
                          [{ Loc.loc = 21:26 to 21:27 in: Contract.nuscr;
                             value = "C" }
                            ];
                          ann = None}
                        }
                       ]
                     ]
                   ))
                }
               ]
             ))
          }
        ];
      ann = None }
    }
  propose(initialPrice: int) from C to P;
  rec Loop [currentPrice<C, P>: int = initialPrice] {
    choice at P {
      accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(currentPrice)}) from P to C;
      confirm() from C to P;
      end
    } or {
      counter(counterPrice: counterPrice:int{(counterPrice)<>(currentPrice)}) from P to C;
      choice at C {
        accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(counterPrice)}) from C to P;
        confirm() from P to C;
        end
      } or {
        counter(newCounterPrice: newCounterPrice:int{(counterPrice)<>(newCounterPrice)}) from C to P;
        continue Loop [newCounterPrice];
      } or {
        reject() from C to P;
        end
      }
    } or {
      reject() from P to C;
      end
    }
  }
  
  propose(initialPrice: int) to P;
  rec Loop [currentPrice<C, P>: int = initialPrice] {
    choice at P {
      accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(currentPrice)}) from P;
      confirm() to P;
      end
    } or {
      counter(counterPrice: counterPrice:int{(counterPrice)<>(currentPrice)}) from P;
      choice at C {
        accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(counterPrice)}) to P;
        confirm() from P;
        end
      } or {
        counter(newCounterPrice: newCounterPrice:int{(counterPrice)<>(newCounterPrice)}) to P;
        continue Loop [newCounterPrice];
      } or {
        reject() to P;
        end
      }
    } or {
      reject() from P;
      end
    }
  }
  
  propose(initialPrice: int) from C;
  rec Loop [currentPrice<C, P>: int = initialPrice] {
    choice at P {
      accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(currentPrice)}) to C;
      confirm() from C;
      end
    } or {
      counter(counterPrice: counterPrice:int{(counterPrice)<>(currentPrice)}) to C;
      choice at C {
        accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(counterPrice)}) from C;
        confirm() to C;
        end
      } or {
        counter(newCounterPrice: newCounterPrice:int{(counterPrice)<>(newCounterPrice)}) from C;
        continue Loop [newCounterPrice];
      } or {
        reject() from C;
        end
      }
    } or {
      reject() to C;
      end
    }
  }
  
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
    
    
    0 -> 1 [label="C?propose(initialPrice: int)", ];
    1 -> 4 [label="C!accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(currentPrice)})",
            ];
    1 -> 7 [label="C!counter(counterPrice: counterPrice:int{(counterPrice)<>(currentPrice)})",
            ];
    1 -> 15 [label="C!reject()", ];
    4 -> 5 [label="C?confirm()", ];
    7 -> 1 [label="C?counter(newCounterPrice: newCounterPrice:int{(counterPrice)<>(newCounterPrice)})[newCounterPrice]",
            ];
    7 -> 9 [label="C?accept(confirmedPrice: confirmedPrice:int{(confirmedPrice)=(counterPrice)})",
            ];
    7 -> 13 [label="C?reject()", ];
    9 -> 10 [label="C!confirm()", ];
    
    }
  State 0 has variables: (empty)
  State 1 has variables: {initialPrice: int; currentPrice: int}
  State 4 has variables: {initialPrice: int; currentPrice: int; confirmedPrice: confirmedPrice:int{(confirmedPrice)=(currentPrice)}}
  State 5 has variables: {initialPrice: int; currentPrice: int; confirmedPrice: confirmedPrice:int{(confirmedPrice)=(currentPrice)}}
  State 7 has variables: {initialPrice: int; currentPrice: int; counterPrice: counterPrice:int{(counterPrice)<>(currentPrice)}}
  State 9 has variables: {initialPrice: int; currentPrice: int; counterPrice: counterPrice:int{(counterPrice)<>(currentPrice)}; confirmedPrice: confirmedPrice:int{(confirmedPrice)=(counterPrice)}}
  State 10 has variables: {initialPrice: int; currentPrice: int; counterPrice: counterPrice:int{(counterPrice)<>(currentPrice)}; confirmedPrice: confirmedPrice:int{(confirmedPrice)=(counterPrice)}}
  State 13 has variables: {initialPrice: int; currentPrice: int; counterPrice: counterPrice:int{(counterPrice)<>(currentPrice)}}
  State 15 has variables: {initialPrice: int; currentPrice: int}
  nuscr: Reported problem:
          "Assert_failure lib/fstarcodegen.ml:86:2"
  [1]
