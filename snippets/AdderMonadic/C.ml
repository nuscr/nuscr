module type Monad = sig
  type nonrec 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Callbacks = sig
  type nonrec t

  val state1Send : t -> t * [`bye of unit | `add of int]

  val state3Send : t -> t * [`add of int]

  val state4Receivesum : t -> int -> t

  val state6Receivebye : t -> unit -> t
end

module Impl_Adder_C (CB : Callbacks) (M : Monad) = struct
  type nonrec comms =
    { send_int: int -> unit M.t
    ; send_string: string -> unit M.t
    ; send_unit: unit -> unit M.t
    ; recv_int: unit -> int M.t
    ; recv_string: unit -> string M.t
    ; recv_unit: unit -> unit M.t }

  let run (router : [`S] -> comms) (env : CB.t) =
    let open CB in
    let rec run_state_1 env =
      let comms = router `S in
      match state1Send env with
      | env, `bye payload ->
          let ret = comms.send_string "bye" in
          M.bind ret (fun () ->
              let ret = comms.send_unit () in
              M.bind ret (fun () -> run_state_6 env))
      | env, `add payload ->
          let ret = comms.send_string "add" in
          M.bind ret (fun () ->
              let ret = comms.send_int payload in
              M.bind ret (fun () -> run_state_3 env))
    and run_state_3 env =
      let comms = router `S in
      match state3Send env with
      | env, `add payload ->
          let ret = comms.send_string "add" in
          M.bind ret (fun () ->
              let ret = comms.send_int payload in
              M.bind ret (fun () -> run_state_4 env))
    and run_state_4 env =
      let comms = router `S in
      let label = comms.recv_string () in
      M.bind label (fun label ->
          match label with
          | "sum" ->
              let payload = comms.recv_int () in
              M.bind payload (fun payload ->
                  let env = state4Receivesum env payload in
                  run_state_1 env)
          | _ -> assert false)
    and run_state_6 env =
      let comms = router `S in
      let label = comms.recv_string () in
      M.bind label (fun label ->
          match label with
          | "bye" ->
              let payload = comms.recv_unit () in
              M.bind payload (fun payload ->
                  let env = state6Receivebye env payload in
                  run_state_7 env)
          | _ -> assert false)
    and run_state_7 env = M.return env in
    run_state_1 env
end
