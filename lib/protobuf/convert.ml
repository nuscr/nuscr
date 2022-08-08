open! Base
open Names
open LiteratureSyntax
open Globaltype
open Localtype

let of_gtype gtype =
  let gtype = LiteratureSyntax.from_gtype gtype in
  match gtype with
  | EndG -> GlobalType.make ~start:(-1) ()
  | gtype ->
      let protobuf_gtype = GlobalType.make () in
      let rec aux tyvars idx pb_gtype = function
        | [] -> (idx, pb_gtype)
        | BranchG {g_br_from= from_role; g_br_to= to_role; g_br_cont= conts}
          :: rest ->
            let from_role = RoleName.user from_role in
            let to_role = RoleName.user to_role in
            let add_cont (idx, pb_gtype) (label, _, cont) =
              let label = LabelName.user label in
              let action_send =
                GlobalAction.make ~idx
                  ~type':GlobalAction.GlobalActionType.SEND ~from_role
                  ~to_role
                  ~continuations:
                    [GlobalAction.Index.make ~next:(idx + 1) ~label ()]
                  ()
              in
              let pb_gtype =
                { pb_gtype with
                  GlobalType.actions=
                    action_send :: pb_gtype.GlobalType.actions }
              in
              let idx = idx + 1 in
              let next_idx =
                match cont with
                | EndG -> -1
                | TVarG tv -> Map.find_exn tyvars tv
                | _ -> idx + 1
              in
              let action_recv =
                GlobalAction.make ~idx
                  ~type':GlobalAction.GlobalActionType.RECV ~from_role
                  ~to_role
                  ~continuations:
                    [GlobalAction.Index.make ~next:next_idx ~label ()]
                  ()
              in
              let idx = idx + 1 in
              let pb_gtype =
                { pb_gtype with
                  GlobalType.actions=
                    action_recv :: pb_gtype.GlobalType.actions }
              in
              match cont with
              | EndG | TVarG _ -> aux tyvars idx pb_gtype rest
              | cont -> aux tyvars idx pb_gtype (cont :: rest)
            in
            List.fold ~f:add_cont ~init:(idx, pb_gtype) conts
        | MuG (tvar, g) :: rest ->
            let tyvars =
              match Map.add tyvars ~key:tvar ~data:idx with
              | `Ok tyvars -> tyvars
              | `Duplicate ->
                  Err.unimpl ~here:[%here] "alpha-renaming for tyvars"
            in
            aux tyvars idx pb_gtype (g :: rest)
        | TVarG _ :: _ ->
            Err.violation ~here:[%here] "TVarG should not appear here"
        | EndG :: _ ->
            Err.violation ~here:[%here] "EndG should not appear here"
      in
      let _, output =
        aux (Map.empty (module TypeVariableName)) 0 protobuf_gtype [gtype]
      in
      output

let of_ltype ltype =
  let ltype = LiteratureSyntax.from_ltype ltype in
  match ltype with
  | EndL -> LocalType.make ~start:(-1) ()
  | ltype ->
      let protobuf_ltype = LocalType.make () in
      let rec aux tyvars idx pb_ltype = function
        | [] -> (idx, pb_ltype)
        | (SendL (partner_role, conts) as ltype) :: rest
         |(RecvL (partner_role, conts) as ltype) :: rest ->
            let partner_role = RoleName.user partner_role in
            let add_cont (idx, pb_ltype) (label, _, cont) =
              let label = LabelName.user label in
              let idx = idx + 1 in
              let action_type =
                match ltype with
                | SendL _ -> LocalAction.LocalActionType.SEND
                | RecvL _ -> LocalAction.LocalActionType.RECV
                | _ -> Err.violation ~here:[%here] "Expect SendL or RecvL"
              in
              let action =
                LocalAction.make ~idx ~type':action_type ~partner_role
                  ~continuations:
                    [LocalAction.Index.make ~next:(idx + 1) ~label ()]
                  ()
              in
              let pb_ltype =
                { pb_ltype with
                  LocalType.actions= action :: pb_ltype.LocalType.actions }
              in
              match cont with
              | EndL | TVarL _ -> aux tyvars idx pb_ltype rest
              | cont -> aux tyvars idx pb_ltype (cont :: rest)
            in
            List.fold ~f:add_cont ~init:(idx, pb_ltype) conts
        | MuL (tvar, g) :: rest ->
            let tyvars =
              match Map.add tyvars ~key:tvar ~data:idx with
              | `Ok tyvars -> tyvars
              | `Duplicate ->
                  Err.unimpl ~here:[%here] "alpha-renaming for tyvars"
            in
            aux tyvars idx pb_ltype (g :: rest)
        | TVarL _ :: _ ->
            Err.violation ~here:[%here] "TVarL should not appear here"
        | EndL :: _ ->
            Err.violation ~here:[%here] "EndL should not appear here"
      in
      let _, output =
        aux (Map.empty (module TypeVariableName)) 0 protobuf_ltype [ltype]
      in
      output
