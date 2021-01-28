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
  let conn = comms P in
  match callbacks.state0Send st with
  | Choice0propose initialPrice ->
  let () = conn.send_string "propose" in
  let () = conn.send_int initialPrice in
  assert false (* TODO *)
  and runState1 (st: state1): ML unit =
  let conn = comms P in
  match conn.recv_string () with
  | "accept" ->
  let confirmedPrice = conn.recv_int () in
  let () = callbacks.state1Recvaccept st confirmedPrice in
  assert false (* TODO *)
  | "counter" ->
  let counterPrice = conn.recv_int () in
  let () = callbacks.state1Recvcounter st counterPrice in
  assert false (* TODO *)
  | "reject" ->
  let _unit = conn.recv_unit () in
  let () = callbacks.state1Recvreject st _unit in
  assert false (* TODO *)
  | _ -> unexpected "Unexpected label"
  and runState4 (st: state4): ML unit =
  let conn = comms P in
  match callbacks.state4Send st with
  | Choice4confirm _unit ->
  let () = conn.send_string "confirm" in
  let () = conn.send_unit _unit in
  assert false (* TODO *)
  and runState5 (st: state5): ML unit =
  ()
  and runState7 (st: state7): ML unit =
  let conn = comms P in
  match callbacks.state7Send st with
  | Choice7counter newCounterPrice ->
  let () = conn.send_string "counter" in
  let () = conn.send_int newCounterPrice in
  assert false (* TODO *)
  | Choice7accept confirmedPrice ->
  let () = conn.send_string "accept" in
  let () = conn.send_int confirmedPrice in
  assert false (* TODO *)
  | Choice7reject _unit ->
  let () = conn.send_string "reject" in
  let () = conn.send_unit _unit in
  assert false (* TODO *)
  and runState9 (st: state9): ML unit =
  let conn = comms P in
  match conn.recv_string () with
  | "confirm" ->
  let _unit = conn.recv_unit () in
  let () = callbacks.state9Recvconfirm st _unit in
  assert false (* TODO *)
  | _ -> unexpected "Unexpected label"
  and runState10 (st: state10): ML unit =
  ()
  and runState13 (st: state13): ML unit =
  ()
  and runState15 (st: state15): ML unit =
  ()
  in
  
  let initState: state0 =
  ()
  in
  runState0 initState
  nuscr: Reported problem:
          "Assert_failure lib/fstarcodegen.ml:475:2"
  [1]
