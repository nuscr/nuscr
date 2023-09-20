Show HigherLower FSM and State variables.
  $ nuscr --gencode-fstar B@HigherLower --verbose HigherLower.nuscr
  { Loc.loc = 1:24 to 24:2 in: HigherLower.nuscr;
    value =
    { name =
      { Loc.loc = 3:17 to 3:28 in: HigherLower.nuscr; value = "HigherLower" };
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
     (Syntax.Exprs.Binop (Syntax.Exprs.And,
        (Syntax.Exprs.Binop (Syntax.Exprs.Leq, (Syntax.Exprs.Int 0),
           (Syntax.Exprs.Var
              { Loc.loc = 4:20 to 4:21 in: HigherLower.nuscr; value = "n" })
           )),
        (Syntax.Exprs.Binop (Syntax.Exprs.Lt,
           (Syntax.Exprs.Var
              { Loc.loc = 4:25 to 4:26 in: HigherLower.nuscr; value = "n" }),
           (Syntax.Exprs.Int 100)))
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
     (Syntax.Exprs.Binop (Syntax.Exprs.Lt, (Syntax.Exprs.Int 0),
        (Syntax.Exprs.Var
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
     (Syntax.Exprs.Binop (Syntax.Exprs.Lt, (Syntax.Exprs.Int 0),
        (Syntax.Exprs.Var
           { Loc.loc = 6:31 to 6:32 in: HigherLower.nuscr; value = "t" })
        ))
     ));
                init =
                (Syntax.Exprs.Var
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
     (Syntax.Exprs.Binop (Syntax.Exprs.And,
        (Syntax.Exprs.Binop (Syntax.Exprs.Leq, (Syntax.Exprs.Int 0),
           (Syntax.Exprs.Var
              { Loc.loc = 7:22 to 7:23 in: HigherLower.nuscr; value = "x" })
           )),
        (Syntax.Exprs.Binop (Syntax.Exprs.Lt,
           (Syntax.Exprs.Var
              { Loc.loc = 7:27 to 7:28 in: HigherLower.nuscr; value = "x" }),
           (Syntax.Exprs.Int 100)))
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
     (Syntax.Exprs.Binop (Syntax.Exprs.And,
        (Syntax.Exprs.Binop (Syntax.Exprs.Gt,
           (Syntax.Exprs.Var
              { Loc.loc = 9:28 to 9:29 in: HigherLower.nuscr; value = "n" }),
           (Syntax.Exprs.Var
              { Loc.loc = 9:32 to 9:33 in: HigherLower.nuscr; value = "x" })
           )),
        (Syntax.Exprs.Binop (Syntax.Exprs.Gt,
           (Syntax.Exprs.Var
              { Loc.loc = 9:37 to 9:38 in: HigherLower.nuscr; value = "t" }),
           (Syntax.Exprs.Int 1)))
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
                            [(Syntax.Exprs.Binop (Syntax.Exprs.Minus,
                                (Syntax.Exprs.Var
                                   { Loc.loc =
                                     11:24 to 11:25 in: HigherLower.nuscr;
                                     value = "t" }),
                                (Syntax.Exprs.Int 1)))
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
     (Syntax.Exprs.Binop (Syntax.Exprs.Eq,
        (Syntax.Exprs.Var
           { Loc.loc = 13:25 to 13:26 in: HigherLower.nuscr; value = "n" }),
        (Syntax.Exprs.Var
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
     (Syntax.Exprs.Binop (Syntax.Exprs.And,
        (Syntax.Exprs.Binop (Syntax.Exprs.Lt,
           (Syntax.Exprs.Var
              { Loc.loc = 16:27 to 16:28 in: HigherLower.nuscr; value = "n" }),
           (Syntax.Exprs.Var
              { Loc.loc = 16:31 to 16:32 in: HigherLower.nuscr; value = "x" })
           )),
        (Syntax.Exprs.Binop (Syntax.Exprs.Gt,
           (Syntax.Exprs.Var
              { Loc.loc = 16:36 to 16:37 in: HigherLower.nuscr; value = "t" }),
           (Syntax.Exprs.Int 1)))
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
                             [(Syntax.Exprs.Binop (Syntax.Exprs.Minus,
                                 (Syntax.Exprs.Var
                                    { Loc.loc =
                                      18:24 to 18:25 in: HigherLower.nuscr;
                                      value = "t" }),
                                 (Syntax.Exprs.Int 1)))
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
     (Syntax.Exprs.Binop (Syntax.Exprs.And,
        (Syntax.Exprs.Binop (Syntax.Exprs.Neq,
           (Syntax.Exprs.Var
              { Loc.loc = 20:26 to 20:27 in: HigherLower.nuscr; value = "n" }),
           (Syntax.Exprs.Var
              { Loc.loc = 20:29 to 20:30 in: HigherLower.nuscr; value = "x" })
           )),
        (Syntax.Exprs.Binop (Syntax.Exprs.Eq,
           (Syntax.Exprs.Var
              { Loc.loc = 20:34 to 20:35 in: HigherLower.nuscr; value = "t" }),
           (Syntax.Exprs.Int 1)))
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
  start(n: (n:int{0 <= n && n < 100})) from A to B;
  limit(t0: (t0:int{0 < t0})) from A to B;
  rec Loop [t<B>: (t:int{0 < t}) = t0] {
    guess(x: (x:int{0 <= x && x < 100})) from C to B;
    choice at B {
      higher(ignore: (ignore:unit{n > x && t > 1})) from B to C;
      higher() from B to A;
      continue Loop [t - 1];
    } or {
      win(ignore: (ignore:unit{n = x})) from B to C;
      lose() from B to A;
      (end)
    } or {
      lower(ignore: (ignore:unit{n < x && t > 1})) from B to C;
      lower() from B to A;
      continue Loop [t - 1];
    } or {
      lose(ignore: (ignore:unit{n <> x && t = 1})) from B to C;
      win() from B to A;
      (end)
    }
  }
  start(n: (n:int{0 <= n && n < 100})) to B;
  limit(t0: (t0:int{0 < t0})) to B;
  rec Loop [(silent) t<B>: (t:int{0 < t}) = t0] {
    (silent) x((x:int{0 <= x && x < 100}));
    choice at B {
      (silent) ignore((ignore:unit{n < x && t > 1}));
      lower() from B;
      continue Loop;
    } or {
      (silent) ignore((ignore:unit{n > x && t > 1}));
      higher() from B;
      continue Loop;
    } or {
      (silent) ignore((ignore:unit{n = x}));
      lose() from B;
      (end)
    } or {
      (silent) ignore((ignore:unit{n <> x && t = 1}));
      win() from B;
      (end)
    }
  }
  start(n: (n:int{0 <= n && n < 100})) from A;
  limit(t0: (t0:int{0 < t0})) from A;
  rec Loop [t<B>: (t:int{0 < t}) = t0] {
    guess(x: (x:int{0 <= x && x < 100})) from C;
    choice at B {
      higher(ignore: (ignore:unit{n > x && t > 1})) to C;
      higher() to A;
      continue Loop [t - 1];
    } or {
      win(ignore: (ignore:unit{n = x})) to C;
      lose() to A;
      (end)
    } or {
      lower(ignore: (ignore:unit{n < x && t > 1})) to C;
      lower() to A;
      continue Loop [t - 1];
    } or {
      lose(ignore: (ignore:unit{n <> x && t = 1})) to C;
      win() to A;
      (end)
    }
  }
  (silent) n((n:int{0 <= n && n < 100}));
  (silent) t0((t0:int{0 < t0}));
  rec Loop [(silent) t<B>: (t:int{0 < t}) = t0] {
    guess(x: (x:int{0 <= x && x < 100})) to B;
    choice at B {
      higher(ignore: (ignore:unit{n > x && t > 1})) from B;
      continue Loop;
    } or {
      win(ignore: (ignore:unit{n = x})) from B;
      (end)
    } or {
      lower(ignore: (ignore:unit{n < x && t > 1})) from B;
      continue Loop;
    } or {
      lose(ignore: (ignore:unit{n <> x && t = 1})) from B;
      (end)
    }
  }
  digraph G {
    0;
    1;
    2 [label="!silent t<B>: (t:int{0 < t}) =
  t0", ];
    7;
    9;
    
    
    0 -> 1 [label="B!start(n: (n:int{0 <= n && n < 100}))", ];
    1 -> 2 [label="B!limit(t0: (t0:int{0 < t0}))", ];
    2 -> 2 [label="B?higher()<ignore: (ignore:unit{n > x && t > 1})>", ];
    2 -> 2 [label="B?lower()<ignore: (ignore:unit{n < x && t > 1}), x: (x:int{0 <= x && x < 100})>",
            ];
    2 -> 7 [label="B?lose()<ignore: (ignore:unit{n = x})>", ];
    2 -> 9 [label="B?win()<ignore: (ignore:unit{n <> x && t = 1})>", ];
    
    }
  
  digraph G {
    0;
    1;
    2 [label="t<B>: (t:int{0 < t}) =
  t0", ];
    4;
    6;
    8;
    9;
    11;
    13;
    14;
    
    
    0 -> 1 [label="A?start(n: (n:int{0 <= n && n < 100}))", ];
    1 -> 2 [label="A?limit(t0: (t0:int{0 < t0}))", ];
    2 -> 4 [label="C?guess(x: (x:int{0 <= x && x < 100}))", ];
    4 -> 6 [label="C!higher(ignore: (ignore:unit{n > x && t > 1}))", ];
    4 -> 8 [label="C!win(ignore: (ignore:unit{n = x}))", ];
    4 -> 11 [label="C!lower(ignore: (ignore:unit{n < x && t > 1}))", ];
    4 -> 13 [label="C!lose(ignore: (ignore:unit{n <> x && t = 1}))", ];
    6 -> 2 [label="A!higher()[t - 1]", ];
    8 -> 9 [label="A!lose()", ];
    11 -> 2 [label="A!lower()[t - 1]", ];
    13 -> 14 [label="A!win()", ];
    
    }
  
  digraph G {
    0 [label="!silent t<B>: (t:int{0 < t}) =
  t0", ];
    2;
    5;
    8;
    
    
    0 -> 2 [label="B!guess(x: (x:int{0 <= x && x < 100}))<t0: (t0:int{0 < t0}), n: (n:int{0 <= n && n < 100})>",
            ];
    2 -> 0 [label="B?higher(ignore: (ignore:unit{n > x && t > 1}))", ];
    2 -> 0 [label="B?lower(ignore: (ignore:unit{n < x && t > 1}))", ];
    2 -> 5 [label="B?win(ignore: (ignore:unit{n = x}))", ];
    2 -> 8 [label="B?lose(ignore: (ignore:unit{n <> x && t = 1}))", ];
    
    }
  
  State 0 has variables: (empty)
  State 1 has variables: {n: (n:int{0 <= n && n < 100})}
  State 2 has variables: {n: (n:int{0 <= n && n < 100}); t0: (t0:int{0 < t0}); t: (t:int{0 < t})}
  State 4 has variables: {n: (n:int{0 <= n && n < 100}); t0: (t0:int{0 < t0}); t: (t:int{0 < t}); x: (x:int{0 <= x && x < 100})}
  State 6 has variables: {n: (n:int{0 <= n && n < 100}); t0: (t0:int{0 < t0}); t: (t:int{0 < t}); x: (x:int{0 <= x && x < 100}); ignore: (ignore:unit{n > x && t > 1})}
  State 8 has variables: {n: (n:int{0 <= n && n < 100}); t0: (t0:int{0 < t0}); t: (t:int{0 < t}); x: (x:int{0 <= x && x < 100}); ignore: (ignore:unit{n = x})}
  State 9 has variables: {n: (n:int{0 <= n && n < 100}); t0: (t0:int{0 < t0}); t: (t:int{0 < t}); x: (x:int{0 <= x && x < 100}); ignore: (ignore:unit{n = x})}
  State 11 has variables: {n: (n:int{0 <= n && n < 100}); t0: (t0:int{0 < t0}); t: (t:int{0 < t}); x: (x:int{0 <= x && x < 100}); ignore: (ignore:unit{n < x && t > 1})}
  State 13 has variables: {n: (n:int{0 <= n && n < 100}); t0: (t0:int{0 < t0}); t: (t:int{0 < t}); x: (x:int{0 <= x && x < 100}); ignore: (ignore:unit{n <> x && t = 1})}
  State 14 has variables: {n: (n:int{0 <= n && n < 100}); t0: (t0:int{0 < t0}); t: (t:int{0 < t}); x: (x:int{0 <= x && x < 100}); ignore: (ignore:unit{n <> x && t = 1})}
  module Generated
  open FStar.All
  open FStar.Ghost
  open FStar.Error
  noeq type state0 =
  {
  _dumState0: unit
  }
  
  noeq type state1 =
  {
  _dumState1: unit;
  n: (n:int{((0)<=(n))&&((n)<(100))})
  }
  
  noeq type state2 =
  {
  _dumState2: unit;
  n: (n:int{((0)<=(n))&&((n)<(100))});
  t0: (t0:int{(0)<(t0)});
  t: (t:int{(0)<(t)})
  }
  
  noeq type state4 =
  {
  _dumState4: unit;
  n: (n:int{((0)<=(n))&&((n)<(100))});
  t0: (t0:int{(0)<(t0)});
  t: (t:int{(0)<(t)});
  x: (x:int{((0)<=(x))&&((x)<(100))})
  }
  
  noeq type state6 =
  {
  _dumState6: unit;
  n: (n:int{((0)<=(n))&&((n)<(100))});
  t0: (t0:int{(0)<(t0)});
  t: (t:int{(0)<(t)});
  x: (x:int{((0)<=(x))&&((x)<(100))});
  ignore: (ignore:unit{((n)>(x))&&((t)>(1))})
  }
  
  noeq type state8 =
  {
  _dumState8: unit;
  n: (n:int{((0)<=(n))&&((n)<(100))});
  t0: (t0:int{(0)<(t0)});
  t: (t:int{(0)<(t)});
  x: (x:int{((0)<=(x))&&((x)<(100))});
  ignore: (ignore:unit{(n)=(x)})
  }
  
  noeq type state9 =
  {
  _dumState9: unit;
  n: (n:int{((0)<=(n))&&((n)<(100))});
  t0: (t0:int{(0)<(t0)});
  t: (t:int{(0)<(t)});
  x: (x:int{((0)<=(x))&&((x)<(100))});
  ignore: (ignore:unit{(n)=(x)})
  }
  
  noeq type state11 =
  {
  _dumState11: unit;
  n: (n:int{((0)<=(n))&&((n)<(100))});
  t0: (t0:int{(0)<(t0)});
  t: (t:int{(0)<(t)});
  x: (x:int{((0)<=(x))&&((x)<(100))});
  ignore: (ignore:unit{((n)<(x))&&((t)>(1))})
  }
  
  noeq type state13 =
  {
  _dumState13: unit;
  n: (n:int{((0)<=(n))&&((n)<(100))});
  t0: (t0:int{(0)<(t0)});
  t: (t:int{(0)<(t)});
  x: (x:int{((0)<=(x))&&((x)<(100))});
  ignore: (ignore:unit{((n)<>(x))&&((t)=(1))})
  }
  
  noeq type state14 =
  {
  _dumState14: unit;
  n: (n:int{((0)<=(n))&&((n)<(100))});
  t0: (t0:int{(0)<(t0)});
  t: (t:int{(0)<(t)});
  x: (x:int{((0)<=(x))&&((x)<(100))});
  ignore: (ignore:unit{((n)<>(x))&&((t)=(1))})
  }
  
  noeq type state4Choice (st: state4) =
  | Choice4lose of ignore:unit{(((Mkstate4?.n st))<>((Mkstate4?.x st)))&&(((Mkstate4?.t st))=(1))}
  | Choice4lower of ignore:unit{(((Mkstate4?.n st))<((Mkstate4?.x st)))&&(((Mkstate4?.t st))>(1))}
  | Choice4win of ignore:unit{((Mkstate4?.n st))=((Mkstate4?.x st))}
  | Choice4higher of ignore:unit{(((Mkstate4?.n st))>((Mkstate4?.x st)))&&(((Mkstate4?.t st))>(1))}
  noeq type state6Choice (st: state6) =
  | Choice6higher of unit
  noeq type state8Choice (st: state8) =
  | Choice8lose of unit
  noeq type state11Choice (st: state11) =
  | Choice11lower of unit
  noeq type state13Choice (st: state13) =
  | Choice13win of unit
  type roles =
  | A
  | C
  noeq type callbacks =
  {
  state0Recvstart: (st: state0) -> (n:int{((0)<=(n))&&((n)<(100))}) -> ML unit;
  state1Recvlimit: (st: state1) -> (t0:int{(0)<(t0)}) -> ML unit;
  state2Recvguess: (st: state2) -> (x:int{((0)<=(x))&&((x)<(100))}) -> ML unit;
  state4Send: (st: state4) -> ML (state4Choice st);
  state6Send: (st: state6) -> ML (state6Choice st);
  state8Send: (st: state8) -> ML (state8Choice st);
  state11Send: (st: state11) -> ML (state11Choice st);
  state13Send: (st: state13) -> ML (state13Choice st)
  }
  
  noeq type comms =
  {
  send_int: int -> ML unit;
  recv_int: unit -> ML int;
  send_string: string -> ML unit;
  recv_string: unit -> ML string;
  send_unit: unit -> ML unit;
  recv_unit: unit -> ML unit
  }
  
  let run (comms: roles -> comms) (callbacks: callbacks) : ML unit =
  
  let rec runState0 (st: state0): ML unit =
  let conn = comms A in
  match conn.recv_string () with
  | "start" ->
  let n = conn.recv_int () in
  assume (((0)<=(n))&&((n)<(100)));
  let () = callbacks.state0Recvstart st n in
  let nextState =
  {
  _dumState1= ();
  n= n
  }
  
  in
  runState1 nextState
  | _ -> unexpected "Unexpected label"
  and runState1 (st: state1): ML unit =
  let conn = comms A in
  match conn.recv_string () with
  | "limit" ->
  let t0 = conn.recv_int () in
  assume ((0)<(t0));
  let () = callbacks.state1Recvlimit st t0 in
  let nextState =
  {
  _dumState2= ();
  n= (Mkstate1?.n st);
  t0= t0;
  t= t0
  }
  
  in
  runState2 nextState
  | _ -> unexpected "Unexpected label"
  and runState2 (st: state2): ML unit =
  let conn = comms C in
  match conn.recv_string () with
  | "guess" ->
  let x = conn.recv_int () in
  assume (((0)<=(x))&&((x)<(100)));
  let () = callbacks.state2Recvguess st x in
  let nextState =
  {
  _dumState4= ();
  n= (Mkstate2?.n st);
  t0= (Mkstate2?.t0 st);
  t= (Mkstate2?.t st);
  x= x
  }
  
  in
  runState4 nextState
  | _ -> unexpected "Unexpected label"
  and runState4 (st: state4): ML unit =
  let conn = comms C in
  match callbacks.state4Send st with
  | Choice4higher ignore ->
  let () = conn.send_string "higher" in
  let () = conn.send_unit ignore in
  let nextState =
  {
  _dumState6= ();
  n= (Mkstate4?.n st);
  t0= (Mkstate4?.t0 st);
  t= (Mkstate4?.t st);
  x= (Mkstate4?.x st);
  ignore= ignore
  }
  
  in
  runState6 nextState
  | Choice4win ignore ->
  let () = conn.send_string "win" in
  let () = conn.send_unit ignore in
  let nextState =
  {
  _dumState8= ();
  n= (Mkstate4?.n st);
  t0= (Mkstate4?.t0 st);
  t= (Mkstate4?.t st);
  x= (Mkstate4?.x st);
  ignore= ignore
  }
  
  in
  runState8 nextState
  | Choice4lower ignore ->
  let () = conn.send_string "lower" in
  let () = conn.send_unit ignore in
  let nextState =
  {
  _dumState11= ();
  n= (Mkstate4?.n st);
  t0= (Mkstate4?.t0 st);
  t= (Mkstate4?.t st);
  x= (Mkstate4?.x st);
  ignore= ignore
  }
  
  in
  runState11 nextState
  | Choice4lose ignore ->
  let () = conn.send_string "lose" in
  let () = conn.send_unit ignore in
  let nextState =
  {
  _dumState13= ();
  n= (Mkstate4?.n st);
  t0= (Mkstate4?.t0 st);
  t= (Mkstate4?.t st);
  x= (Mkstate4?.x st);
  ignore= ignore
  }
  
  in
  runState13 nextState
  and runState6 (st: state6): ML unit =
  let conn = comms A in
  match callbacks.state6Send st with
  | Choice6higher _unit ->
  let () = conn.send_string "higher" in
  let () = conn.send_unit _unit in
  let nextState =
  {
  _dumState2= ();
  n= (Mkstate6?.n st);
  t0= (Mkstate6?.t0 st);
  t= ((Mkstate6?.t st))-(1)
  }
  
  in
  runState2 nextState
  and runState8 (st: state8): ML unit =
  let conn = comms A in
  match callbacks.state8Send st with
  | Choice8lose _unit ->
  let () = conn.send_string "lose" in
  let () = conn.send_unit _unit in
  let nextState =
  {
  _dumState9= ();
  n= (Mkstate8?.n st);
  t0= (Mkstate8?.t0 st);
  t= (Mkstate8?.t st);
  x= (Mkstate8?.x st);
  ignore= (Mkstate8?.ignore st)
  }
  
  in
  runState9 nextState
  and runState9 (st: state9): ML unit =
  ()
  and runState11 (st: state11): ML unit =
  let conn = comms A in
  match callbacks.state11Send st with
  | Choice11lower _unit ->
  let () = conn.send_string "lower" in
  let () = conn.send_unit _unit in
  let nextState =
  {
  _dumState2= ();
  n= (Mkstate11?.n st);
  t0= (Mkstate11?.t0 st);
  t= ((Mkstate11?.t st))-(1)
  }
  
  in
  runState2 nextState
  and runState13 (st: state13): ML unit =
  let conn = comms A in
  match callbacks.state13Send st with
  | Choice13win _unit ->
  let () = conn.send_string "win" in
  let () = conn.send_unit _unit in
  let nextState =
  {
  _dumState14= ();
  n= (Mkstate13?.n st);
  t0= (Mkstate13?.t0 st);
  t= (Mkstate13?.t st);
  x= (Mkstate13?.x st);
  ignore= (Mkstate13?.ignore st)
  }
  
  in
  runState14 nextState
  and runState14 (st: state14): ML unit =
  ()
  in
  
  let initState: state0 =
  {
  _dumState0= ()
  }
  
  in
  runState0 initState
