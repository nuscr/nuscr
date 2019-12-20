module type Callbacks = sig
  type nonrec t

  val state1Send : t -> t * [`bye of unit | `add of int]

  val state3Send : t -> t * [`add of int]

  val state4Receivesum : t -> int -> t

  val state6Receivebye : t -> unit -> t
end

module Impl_Adder_C (CB : Callbacks) = struct
  type nonrec comms =
    { send_int: int -> unit
    ; send_string: string -> unit
    ; send_unit: unit -> unit
    ; recv_int: unit -> int
    ; recv_string: unit -> string
    ; recv_unit: unit -> unit }

  let run (router : [`S] -> comms) (env : CB.t) =
    let open CB in
    let rec run_state_1 env =
      let comms = router `S in
      match state1Send env with
      | env, `bye payload ->
          comms.send_string "bye" ; comms.send_unit () ; run_state_6 env
      | env, `add payload ->
          comms.send_string "add" ; comms.send_int payload ; run_state_3 env
    and run_state_3 env =
      let comms = router `S in
      match state3Send env with
      | env, `add payload ->
          comms.send_string "add" ; comms.send_int payload ; run_state_4 env
    and run_state_4 env =
      let comms = router `S in
      let label = comms.recv_string () in
      match label with
      | "sum" ->
          let payload = comms.recv_int () in
          let env = state4Receivesum env payload in
          run_state_1 env
      | _ -> assert false
    and run_state_6 env =
      let comms = router `S in
      let label = comms.recv_string () in
      match label with
      | "bye" ->
          let payload = comms.recv_unit () in
          let env = state6Receivebye env payload in
          run_state_7 env
      | _ -> assert false
    and run_state_7 env = env in
    run_state_1 env
end
