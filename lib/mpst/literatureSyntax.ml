open! Base
open Names
open Err

type 'a cont_list = (LabelName.t * PayloadTypeName.t list * 'a) list

type global =
  | BranchG of
      { g_br_from: RoleName.t
      ; g_br_to: RoleName.t
      ; g_br_cont: global cont_list }
  | MuG of TypeVariableName.t * global
  | TVarG of TypeVariableName.t
  | EndG

type local =
  | SendL of RoleName.t * local cont_list
  | RecvL of RoleName.t * local cont_list
  | MuL of TypeVariableName.t * local
  | TVarL of TypeVariableName.t
  | EndL

let rec from_gtype = function
  | Gtype.MessageG (m, r_from, r_to, cont) ->
      BranchG
        { g_br_from= r_from
        ; g_br_to= r_to
        ; g_br_cont=
            [ ( m.Gtype.label
              , List.map ~f:Gtype.typename_of_payload m.Gtype.payload
              , from_gtype cont ) ] }
  | Gtype.MuG (tv, _, cont) -> MuG (tv, from_gtype cont)
  | Gtype.TVarG (tv, _, _) -> TVarG tv
  | Gtype.EndG -> EndG
  | Gtype.ChoiceG (choicer, conts) -> (
    match conts with
    | [] ->
        Err.violation ~here:[%here]
          "Choice should have a non-empty list of continuations"
    | _ :: _ ->
        let aux (receiver_role, acc) gtype =
          let gtype = Gtype.normalise gtype in
          let receiver_role =
            match receiver_role with
            | Some r -> Some r
            | None -> (
              match gtype with
              | Gtype.MessageG (_, _, r_to, _) -> Some r_to
              | _ ->
                  Err.violation ~here:[%here]
                    "Normalise should give a message as prefix" )
          in
          let cont =
            match gtype with
            | Gtype.MessageG (m, _, _, cont) ->
                ( m.Gtype.label
                , List.map ~f:Gtype.typename_of_payload m.Gtype.payload
                , from_gtype cont )
            | _ ->
                Err.violation ~here:[%here]
                  "Normalise should give a message as prefix"
          in
          (receiver_role, cont :: acc)
        in
        let receiver_role, conts = List.fold ~init:(None, []) ~f:aux conts in
        BranchG
          { (* From role should always be the choicer in the standard
               syntax *)
            g_br_from= choicer
          ; g_br_to= Option.value_exn receiver_role
          ; g_br_cont= List.rev conts } )
  | Gtype.CallG _ -> Err.unimpl ~here:[%here] "from_gtype: CallG"

let from_ltype _ = unimpl ~here:[%here] "from_ltype"

let show_gtype_mpstk _ = unimpl ~here:[%here] "show_gtype_mpstk"

let show_ltype_mpstk _ = unimpl ~here:[%here] "show_ltype_mpstk"
