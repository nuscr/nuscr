module type Monad  =
  sig
    type nonrec 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
module type Callbacks  =
  sig
    type nonrec t
    val state1Receiveadd : t -> int -> t
    val state1Receivebye : t -> unit -> t
    val state3Receiveadd : t -> int -> t
    val state4Send : t -> (t * [ `sum of int ])
    val state6Send : t -> (t * [ `bye of unit ])
  end
module Impl_Adder_S(CB:Callbacks)(M:Monad) =
  struct
    type nonrec comms =
      {
      send_int: int -> unit M.t ;
      send_string: string -> unit M.t ;
      send_unit: unit -> unit M.t ;
      recv_int: unit -> int M.t ;
      recv_string: unit -> string M.t ;
      recv_unit: unit -> unit M.t }
    let run (router : [ `C ] -> comms) (env : CB.t) =
      let open CB in
        let rec run_state_1 env =
          let comms = router `C in
          let label = comms.recv_string () in
          M.bind label
            (fun label ->
               match label with
               | "bye" ->
                   let payload = comms.recv_unit () in
                   M.bind payload
                     (fun payload ->
                        let env = state1Receivebye env payload in
                        run_state_6 env)
               | "add" ->
                   let payload = comms.recv_int () in
                   M.bind payload
                     (fun payload ->
                        let env = state1Receiveadd env payload in
                        run_state_3 env)
               | _ -> assert false)
        and run_state_3 env =
          let comms = router `C in
          let label = comms.recv_string () in
          M.bind label
            (fun label ->
               match label with
               | "add" ->
                   let payload = comms.recv_int () in
                   M.bind payload
                     (fun payload ->
                        let env = state3Receiveadd env payload in
                        run_state_4 env)
               | _ -> assert false)
        and run_state_4 env =
          let comms = router `C in
          match state4Send env with
          | (env, `sum payload) ->
              let ret = comms.send_string "sum" in
              M.bind ret
                (fun () ->
                   let ret = comms.send_int payload in
                   M.bind ret (fun () -> run_state_1 env))
        and run_state_6 env =
          let comms = router `C in
          match state6Send env with
          | (env, `bye payload) ->
              let ret = comms.send_string "bye" in
              M.bind ret
                (fun () ->
                   let ret = comms.send_unit () in
                   M.bind ret (fun () -> run_state_7 env))
        and run_state_7 env = M.return env in run_state_1 env
  end
