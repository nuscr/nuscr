open! Base
open Names
open Efsm

let gen_code (_start, _g, rec_vars) =
  let module VariableAnalysis = struct
    type vertex = G.E.vertex

    type edge = G.E.t

    type g = G.t

    type data =
      (VariableName.t * Expr.payload_type * (* is_silent *) bool) list

    let direction = Graph.Fixpoint.Forward

    let equal =
      List.equal [%derive.eq: VariableName.t * Expr.payload_type * bool]

    let join vars1 vars2 =
      let rec aux acc vars1 vars2 =
        match (List.hd vars1, List.hd vars2) with
        | Some entry1, Some entry2
          when [%derive.eq: VariableName.t * Expr.payload_type * bool] entry1
                 entry2 ->
            aux (entry1 :: acc) (List.tl_exn vars1) (List.tl_exn vars2)
        | _ -> List.rev acc
      in
      aux [] vars1 vars2

    let analyze (_from, action, to_state) vars =
      match action with
      | SendA (_, m, rannot) | RecvA (_, m, rannot) ->
          let silent_vars =
            List.map ~f:(fun (v, ty) -> (v, ty, true)) rannot.silent_vars
          in
          let find_named_variables =
            List.filter_map ~f:(function
              | Gtype.PValue (Some v, ty) -> Some (v, ty, false)
              | _ -> None)
          in
          let rec_vars =
            Option.value ~default:[] (Map.find rec_vars to_state)
          in
          let rec_vars =
            List.map
              ~f:(fun (is_silent, {Gtype.rv_name; Gtype.rv_ty; _}) ->
                (rv_name, rv_ty, is_silent))
              rec_vars
          in
          let concrete_vars = find_named_variables m.Gtype.payload in
          vars @ silent_vars @ rec_vars @ concrete_vars
      | Epsilon ->
          raise
            (Err.Violation
               "Epsilon transitions should not appear in EFSM outputs")
  end in
  let module Variables = Graph.Fixpoint.Make (G) (VariableAnalysis) in
  assert false
