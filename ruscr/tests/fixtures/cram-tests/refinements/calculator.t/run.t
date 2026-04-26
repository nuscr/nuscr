Generate code for C@Calc. Adding this test because of a regression in recursion without recursion variables.
  $ nuscr --gencode-fstar C@Calc Calculator.nuscr
  module Generated
  open FStar.All
  open FStar.Ghost
  open FStar.Error
  noeq type state0 =
  {
  _dumState0: unit
  }
  
  noeq type state3 =
  {
  _dumState3: unit
  }
  
  noeq type state4 =
  {
  _dumState4: unit
  }
  
  noeq type state6 =
  {
  _dumState6: unit;
  x: (int)
  }
  
  noeq type state7 =
  {
  _dumState7: unit;
  x: (int);
  y: (int)
  }
  
  noeq type state0Choice (st: state0) =
  | Choice0Add of int
  | Choice0Bye of unit
  noeq type state6Choice (st: state6) =
  | Choice6Add of int
  type roles =
  | S
  noeq type callbacks =
  {
  state0Send: (st: state0) -> ML (state0Choice st);
  state3RecvBye: (st: state3) -> (unit) -> ML unit;
  state6Send: (st: state6) -> ML (state6Choice st);
  state7RecvSum: (st: state7) -> (z:int{(z)=(((Mkstate7?.x st))+((Mkstate7?.y st)))}) -> ML unit
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
  let conn = comms S in
  match callbacks.state0Send st with
  | Choice0Bye _unit ->
  let () = conn.send_string "Bye" in
  let () = conn.send_unit _unit in
  let nextState =
  {
  _dumState3= ()
  }
  
  in
  runState3 nextState
  | Choice0Add x ->
  let () = conn.send_string "Add" in
  let () = conn.send_int x in
  let nextState =
  {
  _dumState6= ();
  x= x
  }
  
  in
  runState6 nextState
  and runState3 (st: state3): ML unit =
  let conn = comms S in
  match conn.recv_string () with
  | "Bye" ->
  let _unit = conn.recv_unit () in
  let () = callbacks.state3RecvBye st _unit in
  let nextState =
  {
  _dumState4= ()
  }
  
  in
  runState4 nextState
  | _ -> unexpected "Unexpected label"
  and runState4 (st: state4): ML unit =
  ()
  and runState6 (st: state6): ML unit =
  let conn = comms S in
  match callbacks.state6Send st with
  | Choice6Add y ->
  let () = conn.send_string "Add" in
  let () = conn.send_int y in
  let nextState =
  {
  _dumState7= ();
  x= (Mkstate6?.x st);
  y= y
  }
  
  in
  runState7 nextState
  and runState7 (st: state7): ML unit =
  let conn = comms S in
  match conn.recv_string () with
  | "Sum" ->
  let z = conn.recv_int () in
  assume ((z)=(((Mkstate7?.x st))+((Mkstate7?.y st))));
  let () = callbacks.state7RecvSum st z in
  let nextState =
  {
  _dumState0= ()
  }
  
  in
  runState0 nextState
  | _ -> unexpected "Unexpected label"
  in
  
  let initState: state0 =
  {
  _dumState0= ()
  }
  
  in
  runState0 initState
