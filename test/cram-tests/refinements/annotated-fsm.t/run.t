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
  noeq type state0 =
  {
  count: (int)
  }
  
  noeq type state5 =
  {
  count: (int)
  }
  
  noeq type state6 =
  {
  count: (int);
  total: (total:int{(total)=(count)})
  }
  
  noeq type state5Choice (st: state5) =
  | Choice5Total of total:int{(total)=((Mkstate5?.count st))}
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
  assert false (* TODO *)
  | "Incr" ->
  let _unit = conn.recv_unit () in
  let () = callbacks.state0RecvIncr st _unit in
  assert false (* TODO *)
  | "Result" ->
  let _unit = conn.recv_unit () in
  let () = callbacks.state0RecvResult st _unit in
  assert false (* TODO *)
  | _ -> unexpected "Unexpected label"
  and runState5 (st: state5): ML unit =
  let conn = comms A in
  match callbacks.state5Send st with
  | Choice5Total total ->
  let () = conn.send_string "Total" in
  let () = conn.send_int total in
  assert false (* TODO *)
  and runState6 (st: state6): ML unit =
  ()
  in
  
  let initState: state0 =
  {
  count = 0
  }
  
  in
  runState0 initState
  nuscr: Reported problem:
          "Assert_failure lib/fstarcodegen.ml:466:2"
  [1]