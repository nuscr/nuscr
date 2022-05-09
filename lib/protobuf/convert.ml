open! Base
open Names
open LiteratureSyntax
open Globaltype

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
                Action.make ~idx ~type':Action.ActionType.SEND ~from_role
                  ~to_role
                  ~continuations:[Action.Index.make ~next:(idx + 1) ~label ()]
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
                Action.make ~idx ~type':Action.ActionType.RECV ~from_role
                  ~to_role
                  ~continuations:[Action.Index.make ~next:next_idx ~label ()]
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
            Err.violationf ~here:[%here] "TVarG should not appear here"
        | EndG :: _ ->
            Err.violationf ~here:[%here] "TVarG should not appear here"
      in
      let _, output =
        aux (Map.empty (module TypeVariableName)) 0 protobuf_gtype [gtype]
      in
      output
