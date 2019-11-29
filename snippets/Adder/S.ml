module type Callbacks = sig
  type nonrec t

  val state1Receiveadd : t -> int -> t

  val state1Receivebye : t -> unit -> t

  val state3Receiveadd : t -> int -> t

  val state4Send : t -> t * [`sum of int]

  val state6Send : t -> t * [`bye of unit]
end

module Impl_Adder_S (CB : Callbacks) = struct
  type nonrec comms =
    { send_int: int -> unit
    ; send_string: string -> unit
    ; send_unit: unit -> unit
    ; recv_int: unit -> int
    ; recv_string: unit -> string
    ; recv_unit: unit -> unit }

  let run (router : [`C] -> comms) (env : CB.t) =
    let open CB in
    let rec run_state_1 env =
      let comms = router `C in
      match comms.recv_string () with
      | "bye" ->
          let payload = comms.recv_unit () in
          let env = state1Receivebye env payload in
          run_state_6 env
      | "add" ->
          let payload = comms.recv_int () in
          let env = state1Receiveadd env payload in
          run_state_3 env
      | _ -> assert false
    and run_state_3 env =
      let comms = router `C in
      match comms.recv_string () with
      | "add" ->
          let payload = comms.recv_int () in
          let env = state3Receiveadd env payload in
          run_state_4 env
      | _ -> assert false
    and run_state_4 env =
      let comms = router `C in
      match state4Send env with
      | env, `sum payload ->
          comms.send_string "sum" ; comms.send_int payload ; run_state_1 env
    and run_state_6 env =
      let comms = router `C in
      match state6Send env with
      | env, `bye payload ->
          comms.send_string "bye" ; comms.send_unit () ; run_state_7 env
    and run_state_7 env = env in
    run_state_1 env
end
