open! Base
open Names
open Gtype
open Globaltype

let of_gtype gtype =
  match gtype with
  | EndG -> GlobalType.make ~start:(-1) ()
  | gtype ->
      let protobuf_gtype = GlobalType.make () in
      let rec aux tyvars idx pb_gtype = function
        | [] -> pb_gtype
        | MessageG ({label; _}, from_role, to_role, cont) :: rest -> (
            let label = LabelName.user label in
            let from_role = RoleName.user from_role in
            let to_role = RoleName.user to_role in
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
              | TVarG (tv, _, _) -> Map.find_exn tyvars tv
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
            | cont -> aux tyvars idx pb_gtype (cont :: rest) )
        | MuG (tvar, _, g) :: rest ->
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
        | CallG _ :: _ -> Err.unimpl ~here:[%here] "CallG for protobuf"
        | ChoiceG _ :: _ -> Err.unimpl ~here:[%here] "ChoiceG for protobuf"
      in
      aux (Map.empty (module TypeVariableName)) 0 protobuf_gtype [gtype]
