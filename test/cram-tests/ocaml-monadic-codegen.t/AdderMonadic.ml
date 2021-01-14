module CB_C = struct
  let num1 = ref 0

  let num2 = ref 0

  type t = int

  let state0Send = function
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

  let state0Receiveadd (_, y) x = (x, y)
  let state0Receivebye env () = env

  let state3Receiveadd (x, _) y = (x, y)

  let state4Send (x, y) = ((0, 0), `sum (x + y))

  let state6Send env = (env, `bye ())
end
(* module CI = C.Impl_Adder_C (CB_C) (Lwt)
 * module SI = S.Impl_Adder_S (CB_S) (Lwt) *)

module Identity = struct
  type 'a t = 'a

  let return x = x

  let bind x f = f x
end

module CI = C.Impl_Adder_C (CB_C) (Identity)
module SI = S.Impl_Adder_S (CB_S) (Identity)

type payload = Integer of int | String of string | Unit

(* let c_to_s : payload Lwt_mvar.t = Lwt_mvar.create_empty ()
 *
 * let s_to_c : payload Lwt_mvar.t = Lwt_mvar.create_empty ()
 *
 * let put x y = Lwt_mvar.put y x
 *
 * let get = Lwt_mvar.take
 *
 * let put_int x = put (Integer x)
 *
 * let put_str s = put (String s)
 *
 * let put_unit () = put Unit
 *
 * let get_int v =
 *   let v = get v in
 *   Lwt.bind v (function
 *     | Integer i -> Lwt.return i
 *     | _ -> failwith "Type mismatch")
 *
 * let get_str v =
 *   let v = get v in
 *   Lwt.bind v (function
 *     | String s -> Lwt.return s
 *     | _ -> failwith "Type mismatch")
 *
 * let get_unit v =
 *   let v = get v in
 *   Lwt.bind v (function
 *     | Unit -> Lwt.return ()
 *     | _ -> failwith "Type mismatch")
 *
 * let commC : CI.comms =
 *   { send_int= (fun v -> put_int v c_to_s)
 *   ; send_string= (fun v -> put_str v c_to_s)
 *   ; send_unit= (fun _ -> put_unit () c_to_s)
 *   ; recv_int= (fun _ -> get_int s_to_c)
 *   ; recv_string= (fun _ -> get_str s_to_c)
 *   ; recv_unit= (fun _ -> get_unit s_to_c) }
 *
 * let commS : SI.comms =
 *   { send_int= (fun v -> put_int v s_to_c)
 *   ; send_string= (fun v -> put_str v s_to_c)
 *   ; send_unit= (fun _ -> put_unit () s_to_c)
 *   ; recv_int= (fun _ -> get_int c_to_s)
 *   ; recv_string= (fun _ -> get_str c_to_s)
 *   ; recv_unit= (fun _ -> get_unit c_to_s) }
 *
 * let () =
 *   Lwt_main.run
 *     (let c = CI.run (fun _ -> commC) 10 in
 *      let c () = Lwt.bind c (fun _ -> Lwt.return ()) in
 *      let s = SI.run (fun _ -> commS) (0, 0) in
 *      let s () = Lwt.bind s (fun _ -> Lwt.return ()) in
 *      let list = [c; s] in
 *      Lwt_list.iter_p (fun f -> f ()) list)
 *)
let c_to_s : payload Queue.t = Queue.create ()

let s_to_c : payload Queue.t = Queue.create ()

let c_to_s_mutex = Mutex.create ()

let s_to_c_mutex = Mutex.create ()

let with_mutex mutex f arg =
  Mutex.lock mutex ;
  let result = f arg in
  Mutex.unlock mutex ; result

let put item queue mutex = with_mutex mutex (Queue.add item) queue

let rec get queue mutex =
  Mutex.lock mutex ;
  if Queue.is_empty queue then (
    Mutex.unlock mutex ;
    Unix.sleep 1 ;
    (* Sleep 1 sec *)
    get queue mutex )
  else
    let result = Queue.take queue in
    Mutex.unlock mutex ; result

let put_int x = put (Integer x)

let put_str s = put (String s)

let put_unit () = put Unit

let get_int queue mutex =
  match get queue mutex with Integer i -> i | _ -> failwith "Type mismatch"

let get_str queue mutex =
  match get queue mutex with String s -> s | _ -> failwith "Type mismatch"

let get_unit queue mutex =
  match get queue mutex with Unit -> () | _ -> failwith "Type mismatch"

let commC : CI.comms =
  { send_int= (fun v -> put_int v c_to_s c_to_s_mutex)
  ; send_string= (fun v -> put_str v c_to_s c_to_s_mutex)
  ; send_unit= (fun _ -> put_unit () c_to_s c_to_s_mutex)
  ; recv_int= (fun _ -> get_int s_to_c s_to_c_mutex)
  ; recv_string= (fun _ -> get_str s_to_c s_to_c_mutex)
  ; recv_unit= (fun _ -> get_unit s_to_c s_to_c_mutex) }

let commS : SI.comms =
  { send_int= (fun v -> put_int v s_to_c s_to_c_mutex)
  ; send_string= (fun v -> put_str v s_to_c s_to_c_mutex)
  ; send_unit= (fun _ -> put_unit () s_to_c s_to_c_mutex)
  ; recv_int= (fun _ -> get_int c_to_s c_to_s_mutex)
  ; recv_string= (fun _ -> get_str c_to_s c_to_s_mutex)
  ; recv_unit= (fun _ -> get_unit c_to_s c_to_s_mutex) }

let () =
  let t_c = Thread.create (CI.run (fun _ -> commC)) 10 in
  let t_s = Thread.create (SI.run (fun _ -> commS)) (0, 0) in
  Thread.join t_c ; Thread.join t_s
