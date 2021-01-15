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
