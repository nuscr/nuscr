Expecting recursion expressions to be correctly attached.
  $ nuscr --fsm B@Counter Counter.nuscr
  digraph G {
    0;
    5;
    6;
    
    
    0 -> 0 [label="A?Decr()[(count)-(1)]", ];
    0 -> 0 [label="A?Incr()[(count)+(1)]", ];
    0 -> 5 [label="A?Result()", ];
    5 -> 6 [label="A!Total(total: total:int{(total)=(count)})", ];
    
    }

Var info should be as follows:
0 should have rec var: count
5 should have rec var: count
6 should have rec var: count, and payload var total

  $ nuscr --verbose --gencode-fstar B@Counter Counter.nuscr
  { Loc.loc = 1:24 to 16:2 in: Counter.nuscr;
    value =
    { name = { Loc.loc = 3:17 to 3:24 in: Counter.nuscr; value = "Counter" };
      options = None; parameters = []; rec_parameters = [];
      roles =
      [{ Loc.loc = 3:30 to 3:31 in: Counter.nuscr; value = "A" };
        { Loc.loc = 3:38 to 3:39 in: Counter.nuscr; value = "B" }];
      split_roles =
      ([{ Loc.loc = 3:30 to 3:31 in: Counter.nuscr; value = "A" };
         { Loc.loc = 3:38 to 3:39 in: Counter.nuscr; value = "B" }],
       []);
      nested_protocols = [];
      interactions =
      [{ Loc.loc = 4:5 to 15:6 in: Counter.nuscr;
         value =
         (Recursion (
            { Loc.loc = 4:9 to 4:17 in: Counter.nuscr; value = "Counting" },
            [{ var =
               { Loc.loc = 4:19 to 4:24 in: Counter.nuscr; value = "count" };
               roles =
               [{ Loc.loc = 4:25 to 4:26 in: Counter.nuscr; value = "B" }];
               ty = int; init = (Syntax.Int 0) }
              ],
            [{ Loc.loc = 5:9 to 14:10 in: Counter.nuscr;
               value =
               (Choice (
                  { Loc.loc = 5:19 to 5:20 in: Counter.nuscr; value = "A" },
                  [[{ Loc.loc = 6:13 to 6:32 in: Counter.nuscr;
                      value =
                      MessageTransfer {message = Incr();
                        from_role =
                        { Loc.loc = 6:25 to 6:26 in: Counter.nuscr; value = "A"
                          };
                        to_roles =
                        [{ Loc.loc = 6:30 to 6:31 in: Counter.nuscr;
                           value = "B" }
                          ];
                        ann = None}
                      };
                     { Loc.loc = 7:13 to 7:43 in: Counter.nuscr;
                       value =
                       (Continue (
                          { Loc.loc = 7:22 to 7:30 in: Counter.nuscr;
                            value = "Counting" },
                          [(Syntax.Binop (Syntax.Add,
                              (Syntax.Var
                                 { Loc.loc = 7:32 to 7:37 in: Counter.nuscr;
                                   value = "count" }),
                              (Syntax.Int 1)))
                            ]
                          ))
                       }
                     ];
                    [{ Loc.loc = 9:13 to 9:32 in: Counter.nuscr;
                       value =
                       MessageTransfer {message = Decr();
                         from_role =
                         { Loc.loc = 9:25 to 9:26 in: Counter.nuscr;
                           value = "A" };
                         to_roles =
                         [{ Loc.loc = 9:30 to 9:31 in: Counter.nuscr;
                            value = "B" }
                           ];
                         ann = None}
                       };
                      { Loc.loc = 10:13 to 10:43 in: Counter.nuscr;
                        value =
                        (Continue (
                           { Loc.loc = 10:22 to 10:30 in: Counter.nuscr;
                             value = "Counting" },
                           [(Syntax.Binop (Syntax.Minus,
                               (Syntax.Var
                                  { Loc.loc = 10:32 to 10:37 in: Counter.nuscr;
                                    value = "count" }),
                               (Syntax.Int 1)))
                             ]
                           ))
                        }
                      ];
                    [{ Loc.loc = 12:13 to 12:34 in: Counter.nuscr;
                       value =
                       MessageTransfer {message = Result();
                         from_role =
                         { Loc.loc = 12:27 to 12:28 in: Counter.nuscr;
                           value = "A" };
                         to_roles =
                         [{ Loc.loc = 12:32 to 12:33 in: Counter.nuscr;
                            value = "B" }
                           ];
                         ann = None}
                       };
                      { Loc.loc = 13:13 to 13:58 in: Counter.nuscr;
                        value =
                        MessageTransfer {
                          message =
                          Total((Syntax.Refined (
     { Loc.loc = 13:19 to 13:24 in: Counter.nuscr; value = "total" },
     { Loc.loc = 13:26 to 13:29 in: Counter.nuscr; value = "int" },
     (Syntax.Binop (Syntax.Eq,
        (Syntax.Var
           { Loc.loc = 13:30 to 13:35 in: Counter.nuscr; value = "total" }),
        (Syntax.Var
           { Loc.loc = 13:38 to 13:43 in: Counter.nuscr; value = "count" })
        ))
     )));
                          from_role =
                          { Loc.loc = 13:51 to 13:52 in: Counter.nuscr;
                            value = "B" };
                          to_roles =
                          [{ Loc.loc = 13:56 to 13:57 in: Counter.nuscr;
                             value = "A" }
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
  rec Counting [count<B>: int = 0] {
    choice at A {
      Incr() from A to B;
      continue Counting [(count)+(1)];
    } or {
      Decr() from A to B;
      continue Counting [(count)-(1)];
    } or {
      Result() from A to B;
      Total(total: total:int{(total)=(count)}) from B to A;
      end
    }
  }
  
  rec Counting [(silent) count<B>: int = 0] {
    choice at A {
      Incr() to B;
      continue Counting;
    } or {
      Decr() to B;
      continue Counting;
    } or {
      Result() to B;
      Total(total: total:int{(total)=(count)}) from B;
      end
    }
  }
  
  rec Counting [count<B>: int = 0] {
    choice at A {
      Incr() from A;
      continue Counting [(count)+(1)];
    } or {
      Decr() from A;
      continue Counting [(count)-(1)];
    } or {
      Result() from A;
      Total(total: total:int{(total)=(count)}) to A;
      end
    }
  }
  
  digraph G {
    0;
    5;
    6;
    
    
    0 -> 0 [label="B!Decr()", ];
    0 -> 0 [label="B!Incr()", ];
    0 -> 5 [label="B!Result()", ];
    5 -> 6 [label="B?Total(total: total:int{(total)=(count)})", ];
    
    }
  digraph G {
    0;
    5;
    6;
    
    
    0 -> 0 [label="A?Decr()[(count)-(1)]", ];
    0 -> 0 [label="A?Incr()[(count)+(1)]", ];
    0 -> 5 [label="A?Result()", ];
    5 -> 6 [label="A!Total(total: total:int{(total)=(count)})", ];
    
    }
  State 0 has variables: {count: int}
  State 5 has variables: {count: int}
  State 6 has variables: {count: int; total: total:int{(total)=(count)}}
  nuscr: Reported problem:
          "Assert_failure lib/fstarcodegen.ml:86:2"
  [1]
