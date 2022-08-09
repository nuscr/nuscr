Expecting recursion expressions to be correctly attached.
  $ nuscr --fsm B@Counter Counter.nuscr
  digraph G {
    0 [label="count<B>: int = 0", ];
    5;
    6;
    
    
    0 -> 0 [label="A?Decr()[count - 1]", ];
    0 -> 0 [label="A?Incr()[count + 1]", ];
    0 -> 5 [label="A?Result()", ];
    5 -> 6 [label="A!Total(sum: sum:int{sum = count})", ];
    
    }

Var info should be as follows:
0 should have rec var: count
5 should have rec var: count
6 should have rec var: count, and payload var total

  $ nuscr --gencode-fstar B@Counter Counter.nuscr
  module Generated
  open FStar.All
  open FStar.Ghost
  open FStar.Error
  noeq type state0 =
  {
  _dumState0: unit;
  count: (int)
  }
  
  noeq type state5 =
  {
  _dumState5: unit;
  count: (int)
  }
  
  noeq type state6 =
  {
  _dumState6: unit;
  count: (int);
  sum: (sum:int{(sum)=(count)})
  }
  
  noeq type state5Choice (st: state5) =
  | Choice5Total of sum:int{(sum)=((Mkstate5?.count st))}
  type roles =
  | A
  noeq type callbacks =
  {
  state0RecvDecr: (st: state0) -> (unit) -> ML unit;
  state0RecvIncr: (st: state0) -> (unit) -> ML unit;
  state0RecvResult: (st: state0) -> (unit) -> ML unit;
  state5Send: (st: state5) -> ML (state5Choice st)
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
  | "Decr" ->
  let _unit = conn.recv_unit () in
  let () = callbacks.state0RecvDecr st _unit in
  let nextState =
  {
  _dumState0= ();
  count= ((Mkstate0?.count st))-(1)
  }
  
  in
  runState0 nextState
  | "Incr" ->
  let _unit = conn.recv_unit () in
  let () = callbacks.state0RecvIncr st _unit in
  let nextState =
  {
  _dumState0= ();
  count= ((Mkstate0?.count st))+(1)
  }
  
  in
  runState0 nextState
  | "Result" ->
  let _unit = conn.recv_unit () in
  let () = callbacks.state0RecvResult st _unit in
  let nextState =
  {
  _dumState5= ();
  count= (Mkstate0?.count st)
  }
  
  in
  runState5 nextState
  | _ -> unexpected "Unexpected label"
  and runState5 (st: state5): ML unit =
  let conn = comms A in
  match callbacks.state5Send st with
  | Choice5Total sum ->
  let () = conn.send_string "Total" in
  let () = conn.send_int sum in
  let nextState =
  {
  _dumState6= ();
  count= (Mkstate5?.count st);
  sum= sum
  }
  
  in
  runState6 nextState
  and runState6 (st: state6): ML unit =
  ()
  in
  
  let initState: state0 =
  {
  _dumState0= ();
  count= 0
  }
  
  in
  runState0 initState

A should have some erased variables.
  $ nuscr --gencode-fstar A@Counter Counter.nuscr
  module Generated
  open FStar.All
  open FStar.Ghost
  open FStar.Error
  noeq type state0 =
  {
  _dumState0: unit;
  count: erased (int)
  }
  
  noeq type state5 =
  {
  _dumState5: unit;
  count: erased (int)
  }
  
  noeq type state6 =
  {
  _dumState6: unit;
  count: erased (int);
  sum: (sum:int{(sum)=((reveal count))})
  }
  
  noeq type state0Choice (st: state0) =
  | Choice0Result of unit
  | Choice0Incr of unit
  | Choice0Decr of unit
  type roles =
  | B
  noeq type callbacks =
  {
  state0Send: (st: state0) -> ML (state0Choice st);
  state5RecvTotal: (st: state5) -> (sum:int{(sum)=((reveal (Mkstate5?.count st)))}) -> ML unit
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
  let conn = comms B in
  match callbacks.state0Send st with
  | Choice0Decr _unit ->
  let () = conn.send_string "Decr" in
  let () = conn.send_unit _unit in
  let nextState =
  {
  _dumState0= ();
  count= (assume false; hide 0)
  }
  
  in
  runState0 nextState
  | Choice0Incr _unit ->
  let () = conn.send_string "Incr" in
  let () = conn.send_unit _unit in
  let nextState =
  {
  _dumState0= ();
  count= (assume false; hide 0)
  }
  
  in
  runState0 nextState
  | Choice0Result _unit ->
  let () = conn.send_string "Result" in
  let () = conn.send_unit _unit in
  let nextState =
  {
  _dumState5= ();
  count= (reveal (Mkstate0?.count st))
  }
  
  in
  runState5 nextState
  and runState5 (st: state5): ML unit =
  let conn = comms B in
  match conn.recv_string () with
  | "Total" ->
  let sum = conn.recv_int () in
  assume ((sum)=((reveal (Mkstate5?.count st))));
  let () = callbacks.state5RecvTotal st sum in
  let nextState =
  {
  _dumState6= ();
  count= (reveal (Mkstate5?.count st));
  sum= sum
  }
  
  in
  runState6 nextState
  | _ -> unexpected "Unexpected label"
  and runState6 (st: state6): ML unit =
  ()
  in
  
  let initState: state0 =
  {
  _dumState0= ();
  count= (assume false; hide 0)
  }
  
  in
  runState0 initState
