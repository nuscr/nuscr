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

  $ nuscr --gencode-fstar B@Counter Counter.nuscr
  type state0 =
  {count: (int)}
  
  type state5 =
  {count: (int)}
  
  type state6 =
  {count: (int);
  total: (total:int{(total)=(count)})}
  
  noeq type state5Choice (st: state5) =
  | Choice5Total of total:int{(total)=((Mkstate5?.count st))}
  nuscr: Reported problem:
          "Assert_failure lib/fstarcodegen.ml:190:2"
  [1]
