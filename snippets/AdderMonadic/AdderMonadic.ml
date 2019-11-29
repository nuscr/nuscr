module CB_C = struct
  let num1 = ref 0

  let num2 = ref 0

  type t = int

  let state1Send = function
    | 0 ->
        Printf.printf "C: Sending bye\n" ;
        (0, `bye ())
    | x ->
        Printf.printf "C: Sending add\n" ;
        let num = Random.int 100 in
        num1 := num ;
        (x - 1, `add num)

  let state3Send x =
    let num = Random.int 100 in
    num2 := num ;
    (x, `add num)

  let state4Receivesum x sum =
    Printf.printf "C: Sum of %d and %d is %d\n" !num1 !num2 sum ;
    x

  let state6Receivebye x () =
    Printf.printf "C: Received Bye\n" ;
    x
end

module CB_S = struct
  type t = int * int

  let state1Receivebye env () = env

  let state1Receiveadd (_, y) x = (x, y)

  let state3Receiveadd (x, _) y = (x, y)

  let state4Send (x, y) = ((0, 0), `sum (x + y))

  let state6Send env = (env, `bye ())
end

module CI = C.Impl_Adder_C (CB_C) (Lwt)
module SI = S.Impl_Adder_S (CB_S) (Lwt)

type payload = Integer of int | String of string | Unit

let c_to_s : payload Lwt_mvar.t = Lwt_mvar.create_empty ()

let s_to_c : payload Lwt_mvar.t = Lwt_mvar.create_empty ()

let put x y = Lwt_mvar.put y x

let get = Lwt_mvar.take

let put_int x = put (Integer x)

let put_str s = put (String s)

let put_unit () = put Unit

let get_int v =
  let v = get v in
  Lwt.bind v (function
    | Integer i -> Lwt.return i
    | _ -> failwith "Type mismatch")

let get_str v =
  let v = get v in
  Lwt.bind v (function
    | String s -> Lwt.return s
    | _ -> failwith "Type mismatch")

let get_unit v =
  let v = get v in
  Lwt.bind v (function
    | Unit -> Lwt.return ()
    | _ -> failwith "Type mismatch")

let commC : CI.comms =
  { send_int= (fun v -> put_int v c_to_s)
  ; send_string= (fun v -> put_str v c_to_s)
  ; send_unit= (fun _ -> put_unit () c_to_s)
  ; recv_int= (fun _ -> get_int s_to_c)
  ; recv_string= (fun _ -> get_str s_to_c)
  ; recv_unit= (fun _ -> get_unit s_to_c) }

let commS : SI.comms =
  { send_int= (fun v -> put_int v s_to_c)
  ; send_string= (fun v -> put_str v s_to_c)
  ; send_unit= (fun _ -> put_unit () s_to_c)
  ; recv_int= (fun _ -> get_int c_to_s)
  ; recv_string= (fun _ -> get_str c_to_s)
  ; recv_unit= (fun _ -> get_unit c_to_s) }

let () =
  Lwt_main.run
    (let c = CI.run (fun _ -> commC) 10 in
     let c () = Lwt.bind c (fun _ -> Lwt.return ()) in
     let s = SI.run (fun _ -> commS) (0, 0) in
     let s () = Lwt.bind s (fun _ -> Lwt.return ()) in
     let list = [c; s] in
     Lwt_list.iter_p (fun f -> f ()) list)
