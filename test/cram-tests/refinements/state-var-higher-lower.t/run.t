Show HigherLower FSM and State variables.
  $ nuscr --gencode-fstar B@HigherLower --verbose HigherLower.nuscr
  { Loc.loc = 1:24 to 24:2 in: HigherLower.nuscr;
    value =
    { name =
      { Loc.loc = 3:17 to 3:28 in: HigherLower.nuscr; value = "HigherLower" };
      options = None; parameters = []; rec_parameters = [];
      roles =
      [{ Loc.loc = 3:34 to 3:35 in: HigherLower.nuscr; value = "A" };
        { Loc.loc = 3:42 to 3:43 in: HigherLower.nuscr; value = "B" };
        { Loc.loc = 3:50 to 3:51 in: HigherLower.nuscr; value = "C" }];
      split_roles =
      ([{ Loc.loc = 3:34 to 3:35 in: HigherLower.nuscr; value = "A" };
         { Loc.loc = 3:42 to 3:43 in: HigherLower.nuscr; value = "B" };
         { Loc.loc = 3:50 to 3:51 in: HigherLower.nuscr; value = "C" }],
       []);
      nested_protocols = [];
      interactions =
      [{ Loc.loc = 4:3 to 4:47 in: HigherLower.nuscr;
         value =
         MessageTransfer {
           message =
           start((Syntax.Refined (
     { Loc.loc = 4:9 to 4:10 in: HigherLower.nuscr; value = "n" },
     { Loc.loc = 4:11 to 4:14 in: HigherLower.nuscr; value = "int" },
     (Syntax.RawExpr.Binop (Syntax.RawExpr.And,
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Leq, (Syntax.RawExpr.Int 0),
           (Syntax.RawExpr.Var
              { Loc.loc = 4:20 to 4:21 in: HigherLower.nuscr; value = "n" })
           )),
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Lt,
           (Syntax.RawExpr.Var
              { Loc.loc = 4:25 to 4:26 in: HigherLower.nuscr; value = "n" }),
           (Syntax.RawExpr.Int 100)))
        ))
     )));
           from_role =
           { Loc.loc = 4:40 to 4:41 in: HigherLower.nuscr; value = "A" };
           to_roles =
           [{ Loc.loc = 4:45 to 4:46 in: HigherLower.nuscr; value = "B" }];
           ann = None}
         };
        { Loc.loc = 5:3 to 5:37 in: HigherLower.nuscr;
          value =
          MessageTransfer {
            message =
            limit((Syntax.Refined (
     { Loc.loc = 5:9 to 5:11 in: HigherLower.nuscr; value = "t0" },
     { Loc.loc = 5:12 to 5:15 in: HigherLower.nuscr; value = "int" },
     (Syntax.RawExpr.Binop (Syntax.RawExpr.Lt, (Syntax.RawExpr.Int 0),
        (Syntax.RawExpr.Var
           { Loc.loc = 5:20 to 5:22 in: HigherLower.nuscr; value = "t0" })
        ))
     )));
            from_role =
            { Loc.loc = 5:30 to 5:31 in: HigherLower.nuscr; value = "A" };
            to_roles =
            [{ Loc.loc = 5:35 to 5:36 in: HigherLower.nuscr; value = "B" }];
            ann = None}
          };
        { Loc.loc = 6:3 to 23:4 in: HigherLower.nuscr;
          value =
          (Recursion (
             { Loc.loc = 6:7 to 6:11 in: HigherLower.nuscr; value = "Loop" },
             [{ var =
                { Loc.loc = 6:13 to 6:14 in: HigherLower.nuscr; value = "t" };
                roles =
                [{ Loc.loc = 6:15 to 6:16 in: HigherLower.nuscr; value = "B" }];
                ty =
                (Syntax.Refined (
     { Loc.loc = 6:20 to 6:21 in: HigherLower.nuscr; value = "t" },
     { Loc.loc = 6:23 to 6:26 in: HigherLower.nuscr; value = "int" },
     (Syntax.RawExpr.Binop (Syntax.RawExpr.Lt, (Syntax.RawExpr.Int 0),
        (Syntax.RawExpr.Var
           { Loc.loc = 6:31 to 6:32 in: HigherLower.nuscr; value = "t" })
        ))
     ));
                init =
                (Syntax.RawExpr.Var
                   { Loc.loc = 6:37 to 6:39 in: HigherLower.nuscr; value = "t0"
                     })
                }
               ],
             [{ Loc.loc = 7:5 to 7:49 in: HigherLower.nuscr;
                value =
                MessageTransfer {
                  message =
                  guess((Syntax.Refined (
     { Loc.loc = 7:11 to 7:12 in: HigherLower.nuscr; value = "x" },
     { Loc.loc = 7:13 to 7:16 in: HigherLower.nuscr; value = "int" },
     (Syntax.RawExpr.Binop (Syntax.RawExpr.And,
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Leq, (Syntax.RawExpr.Int 0),
           (Syntax.RawExpr.Var
              { Loc.loc = 7:22 to 7:23 in: HigherLower.nuscr; value = "x" })
           )),
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Lt,
           (Syntax.RawExpr.Var
              { Loc.loc = 7:27 to 7:28 in: HigherLower.nuscr; value = "x" }),
           (Syntax.RawExpr.Int 100)))
        ))
     )));
                  from_role =
                  { Loc.loc = 7:42 to 7:43 in: HigherLower.nuscr; value = "C" };
                  to_roles =
                  [{ Loc.loc = 7:47 to 7:48 in: HigherLower.nuscr; value = "B"
                     }
                    ];
                  ann = None}
                };
               { Loc.loc = 8:5 to 22:6 in: HigherLower.nuscr;
                 value =
                 (Choice (
                    { Loc.loc = 8:15 to 8:16 in: HigherLower.nuscr; value = "B"
                      },
                    [[{ Loc.loc = 9:9 to 9:57 in: HigherLower.nuscr;
                        value =
                        MessageTransfer {
                          message =
                          higher((Syntax.Refined (
     { Loc.loc = 9:16 to 9:22 in: HigherLower.nuscr; value = "ignore" },
     { Loc.loc = 9:23 to 9:27 in: HigherLower.nuscr; value = "unit" },
     (Syntax.RawExpr.Binop (Syntax.RawExpr.And,
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Gt,
           (Syntax.RawExpr.Var
              { Loc.loc = 9:28 to 9:29 in: HigherLower.nuscr; value = "n" }),
           (Syntax.RawExpr.Var
              { Loc.loc = 9:32 to 9:33 in: HigherLower.nuscr; value = "x" })
           )),
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Gt,
           (Syntax.RawExpr.Var
              { Loc.loc = 9:37 to 9:38 in: HigherLower.nuscr; value = "t" }),
           (Syntax.RawExpr.Int 1)))
        ))
     )));
                          from_role =
                          { Loc.loc = 9:50 to 9:51 in: HigherLower.nuscr;
                            value = "B" };
                          to_roles =
                          [{ Loc.loc = 9:55 to 9:56 in: HigherLower.nuscr;
                             value = "C" }
                            ];
                          ann = None}
                        };
                       { Loc.loc = 10:9 to 10:30 in: HigherLower.nuscr;
                         value =
                         MessageTransfer {message = higher();
                           from_role =
                           { Loc.loc = 10:23 to 10:24 in: HigherLower.nuscr;
                             value = "B" };
                           to_roles =
                           [{ Loc.loc = 10:28 to 10:29 in: HigherLower.nuscr;
                              value = "A" }
                             ];
                           ann = None}
                         };
                       { Loc.loc = 11:9 to 11:31 in: HigherLower.nuscr;
                         value =
                         (Continue (
                            { Loc.loc = 11:18 to 11:22 in: HigherLower.nuscr;
                              value = "Loop" },
                            [(Syntax.RawExpr.Binop (Syntax.RawExpr.Minus,
                                (Syntax.RawExpr.Var
                                   { Loc.loc =
                                     11:24 to 11:25 in: HigherLower.nuscr;
                                     value = "t" }),
                                (Syntax.RawExpr.Int 1)))
                              ]
                            ))
                         }
                       ];
                      [{ Loc.loc = 13:9 to 13:43 in: HigherLower.nuscr;
                         value =
                         MessageTransfer {
                           message =
                           win((Syntax.Refined (
     { Loc.loc = 13:13 to 13:19 in: HigherLower.nuscr; value = "ignore" },
     { Loc.loc = 13:20 to 13:24 in: HigherLower.nuscr; value = "unit" },
     (Syntax.RawExpr.Binop (Syntax.RawExpr.Eq,
        (Syntax.RawExpr.Var
           { Loc.loc = 13:25 to 13:26 in: HigherLower.nuscr; value = "n" }),
        (Syntax.RawExpr.Var
           { Loc.loc = 13:27 to 13:28 in: HigherLower.nuscr; value = "x" })
        ))
     )));
                           from_role =
                           { Loc.loc = 13:36 to 13:37 in: HigherLower.nuscr;
                             value = "B" };
                           to_roles =
                           [{ Loc.loc = 13:41 to 13:42 in: HigherLower.nuscr;
                              value = "C" }
                             ];
                           ann = None}
                         };
                        { Loc.loc = 14:9 to 14:28 in: HigherLower.nuscr;
                          value =
                          MessageTransfer {message = lose();
                            from_role =
                            { Loc.loc = 14:21 to 14:22 in: HigherLower.nuscr;
                              value = "B" };
                            to_roles =
                            [{ Loc.loc = 14:26 to 14:27 in: HigherLower.nuscr;
                               value = "A" }
                              ];
                            ann = None}
                          }
                        ];
                      [{ Loc.loc = 16:9 to 16:56 in: HigherLower.nuscr;
                         value =
                         MessageTransfer {
                           message =
                           lower((Syntax.Refined (
     { Loc.loc = 16:15 to 16:21 in: HigherLower.nuscr; value = "ignore" },
     { Loc.loc = 16:22 to 16:26 in: HigherLower.nuscr; value = "unit" },
     (Syntax.RawExpr.Binop (Syntax.RawExpr.And,
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Lt,
           (Syntax.RawExpr.Var
              { Loc.loc = 16:27 to 16:28 in: HigherLower.nuscr; value = "n" }),
           (Syntax.RawExpr.Var
              { Loc.loc = 16:31 to 16:32 in: HigherLower.nuscr; value = "x" })
           )),
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Gt,
           (Syntax.RawExpr.Var
              { Loc.loc = 16:36 to 16:37 in: HigherLower.nuscr; value = "t" }),
           (Syntax.RawExpr.Int 1)))
        ))
     )));
                           from_role =
                           { Loc.loc = 16:49 to 16:50 in: HigherLower.nuscr;
                             value = "B" };
                           to_roles =
                           [{ Loc.loc = 16:54 to 16:55 in: HigherLower.nuscr;
                              value = "C" }
                             ];
                           ann = None}
                         };
                        { Loc.loc = 17:9 to 17:29 in: HigherLower.nuscr;
                          value =
                          MessageTransfer {message = lower();
                            from_role =
                            { Loc.loc = 17:22 to 17:23 in: HigherLower.nuscr;
                              value = "B" };
                            to_roles =
                            [{ Loc.loc = 17:27 to 17:28 in: HigherLower.nuscr;
                               value = "A" }
                              ];
                            ann = None}
                          };
                        { Loc.loc = 18:9 to 18:31 in: HigherLower.nuscr;
                          value =
                          (Continue (
                             { Loc.loc = 18:18 to 18:22 in: HigherLower.nuscr;
                               value = "Loop" },
                             [(Syntax.RawExpr.Binop (Syntax.RawExpr.Minus,
                                 (Syntax.RawExpr.Var
                                    { Loc.loc =
                                      18:24 to 18:25 in: HigherLower.nuscr;
                                      value = "t" }),
                                 (Syntax.RawExpr.Int 1)))
                               ]
                             ))
                          }
                        ];
                      [{ Loc.loc = 20:9 to 20:54 in: HigherLower.nuscr;
                         value =
                         MessageTransfer {
                           message =
                           lose((Syntax.Refined (
     { Loc.loc = 20:14 to 20:20 in: HigherLower.nuscr; value = "ignore" },
     { Loc.loc = 20:21 to 20:25 in: HigherLower.nuscr; value = "unit" },
     (Syntax.RawExpr.Binop (Syntax.RawExpr.And,
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Neq,
           (Syntax.RawExpr.Var
              { Loc.loc = 20:26 to 20:27 in: HigherLower.nuscr; value = "n" }),
           (Syntax.RawExpr.Var
              { Loc.loc = 20:29 to 20:30 in: HigherLower.nuscr; value = "x" })
           )),
        (Syntax.RawExpr.Binop (Syntax.RawExpr.Eq,
           (Syntax.RawExpr.Var
              { Loc.loc = 20:34 to 20:35 in: HigherLower.nuscr; value = "t" }),
           (Syntax.RawExpr.Int 1)))
        ))
     )));
                           from_role =
                           { Loc.loc = 20:47 to 20:48 in: HigherLower.nuscr;
                             value = "B" };
                           to_roles =
                           [{ Loc.loc = 20:52 to 20:53 in: HigherLower.nuscr;
                              value = "C" }
                             ];
                           ann = None}
                         };
                        { Loc.loc = 21:9 to 21:27 in: HigherLower.nuscr;
                          value =
                          MessageTransfer {message = win();
                            from_role =
                            { Loc.loc = 21:20 to 21:21 in: HigherLower.nuscr;
                              value = "B" };
                            to_roles =
                            [{ Loc.loc = 21:25 to 21:26 in: HigherLower.nuscr;
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
  start(n: n:int{((0)<=(n))&&((n)<(100))}) from A to B;
  limit(t0: t0:int{(0)<(t0)}) from A to B;
  rec Loop [t<B>: t:int{(0)<(t)} = t0] {
    guess(x: x:int{((0)<=(x))&&((x)<(100))}) from C to B;
    choice at B {
      higher(ignore: ignore:unit{((n)>(x))&&((t)>(1))}) from B to C;
      higher() from B to A;
      continue Loop [(t)-(1)];
    } or {
      win(ignore: ignore:unit{(n)=(x)}) from B to C;
      lose() from B to A;
      end
    } or {
      lower(ignore: ignore:unit{((n)<(x))&&((t)>(1))}) from B to C;
      lower() from B to A;
      continue Loop [(t)-(1)];
    } or {
      lose(ignore: ignore:unit{((n)<>(x))&&((t)=(1))}) from B to C;
      win() from B to A;
      end
    }
  }
  
  nuscr: User error: Unable to merge: (silent) ignore(ignore:unit{((n)>(x))&&((t)>(1))});
         higher() from B;
         continue Loop;
          (silent) ignore(ignore:unit{(n)=(x)});
         lose() from B;
         end
         
  [1]
