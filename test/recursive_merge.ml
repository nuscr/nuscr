open! Base
open Nuscrlib
open Names

(* Compare all reachable states with a hand-written specification, ignoring
   generated state IDs and recursion binder names. This checks infinite
   behavior, including that distinguishing a branch cannot later switch to
   the other branch's continuation. *)
let check ?(sending = false) protocol file specification =
  Pragma.reset () ;
  let path = "cram-tests/core/merge-recursion.t/" ^ file in
  let ast = Stdio.In_channel.with_file path ~f:(parse path) in
  let root, (graph, _) =
    generate_fsm ast
      ~protocol:(ProtocolName.of_string protocol)
      ~role:(RoleName.of_string "B")
  in
  let rec visit seen = function
    | [] -> ()
    | (actual, expected) :: rest ->
        if
          List.mem seen (actual, expected) ~equal:(fun (a, b) (c, d) ->
              Int.equal a c && Int.equal b d )
        then visit seen rest
        else
          let transitions =
            Efsm.G.succ_e graph actual
            |> List.map ~f:(function
              | _, Efsm.RecvA (peer, message, _), target when not sending ->
                  assert (String.equal (RoleName.user peer) "A") ;
                  (LabelName.user message.label, target)
              | _, Efsm.SendA (peer, message, _), target when sending ->
                  assert (String.equal (RoleName.user peer) "A") ;
                  (LabelName.user message.label, target)
              | _ -> failwith "Unexpected transition direction" )
            |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
          in
          let wanted = List.nth_exn specification expected in
          assert (
            List.equal String.equal
              (List.map transitions ~f:fst)
              (List.map wanted ~f:fst) ) ;
          let successors =
            List.map2_exn transitions wanted ~f:(fun (_, a) (_, b) -> (a, b))
          in
          visit ((actual, expected) :: seen) (successors @ rest)
  in
  visit [] [(root, 0)]

let () =
  check "Example2" "Loop2.nuscr" [[("Foo", 1)]; [("Baz", 0); ("Foo", 1)]] ;
  check "SameLabel" "SameLabel.nuscr"
    [ [("Tick", 1)]
    ; [("Left", 2); ("Right", 4)]
    ; [("Tick", 3)]
    ; [("Left", 2)]
    ; [("Tick", 5)]
    ; [("Right", 4)] ] ;
  check ~sending:true "Outputs" "Outputs.nuscr" [[("Ping", 0); ("Pong", 0)]]
