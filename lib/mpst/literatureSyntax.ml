open! Base
open Names

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

let rec from_ltype = function
  | Ltype.RecvL (m, r_from, cont) ->
      RecvL
        ( r_from
        , [ ( m.Gtype.label
            , List.map ~f:Gtype.typename_of_payload m.Gtype.payload
            , from_ltype cont ) ] )
  | Ltype.SendL (m, r_from, cont) ->
      SendL
        ( r_from
        , [ ( m.Gtype.label
            , List.map ~f:Gtype.typename_of_payload m.Gtype.payload
            , from_ltype cont ) ] )
  | Ltype.ChoiceL (_, conts) -> (
    match conts with
    | [] ->
        Err.violation ~here:[%here]
          "Choice should have a non-empty list of continuations"
    | _ :: _ -> (
        let aux (choice_role, acc) ltype =
          let choice_role =
            match choice_role with
            | Some r -> Some r
            | None -> (
              match ltype with
              | Ltype.SendL (_, r_send, _) -> Some (`Send r_send)
              | Ltype.RecvL (_, r_recv, _) -> Some (`Recv r_recv)
              | _ ->
                  Err.violation ~here:[%here]
                    "First action in ChoiceL should be either SendL or RecvL"
              )
          in
          let cont =
            match ltype with
            | Ltype.SendL (m, _, cont) ->
                ( m.Gtype.label
                , List.map ~f:Gtype.typename_of_payload m.Gtype.payload
                , from_ltype cont )
            | Ltype.RecvL (m, _, cont) ->
                ( m.Gtype.label
                , List.map ~f:Gtype.typename_of_payload m.Gtype.payload
                , from_ltype cont )
            | _ ->
                Err.violation ~here:[%here]
                  "First action in ChoiceL should be either SendL or RecvL"
          in
          (choice_role, cont :: acc)
        in
        let choice_role, conts = List.fold ~init:(None, []) ~f:aux conts in
        match choice_role with
        | Some (`Send r) -> SendL (r, conts)
        | Some (`Recv r) -> RecvL (r, conts)
        | None -> Err.violation ~here:[%here] "Choice cannot be empty" ) )
  | Ltype.TVarL (tv, _) -> TVarL tv
  | Ltype.MuL (tv, _, cont) -> MuL (tv, from_ltype cont)
  | Ltype.EndL -> EndL
  | Ltype.InviteCreateL _ ->
      Err.unimpl ~here:[%here] "from_ltype: InviteCreateL"
  | Ltype.AcceptL _ -> Err.unimpl ~here:[%here] "from_ltype: AcceptL"
  | Ltype.SilentL _ -> Err.unimpl ~here:[%here] "from_ltype: SilentL"

let show_cont f (label, payloads, cont) =
  let payloads =
    match payloads with
    | [] -> ""
    | payloads ->
        Printf.sprintf "(%s)"
          (String.concat ~sep:", "
             (List.map ~f:PayloadTypeName.user payloads) )
  in
  Printf.sprintf "%s%s . %s" (LabelName.user label) payloads (f cont)

let show_cont_list f = function
  | [cont] -> show_cont f cont
  | conts ->
      Printf.sprintf "{\n%s\n}"
        (String.concat ~sep:",\n" (List.map ~f:(show_cont f) conts))

let rec show_gtype_mpstk = function
  | BranchG {g_br_from; g_br_to; g_br_cont} ->
      Printf.sprintf "%s→%s:%s" (RoleName.user g_br_from)
        (RoleName.user g_br_to)
        (show_cont_list show_gtype_mpstk g_br_cont)
  | MuG (tv, cont) ->
      Printf.sprintf "μ(%s)(%s)"
        (TypeVariableName.user tv)
        (show_gtype_mpstk cont)
  | TVarG tv -> TypeVariableName.user tv
  | EndG -> "end"

let rec show_ltype_mpstk = function
  | SendL (role, conts) ->
      Printf.sprintf "%s⊕%s" (RoleName.user role)
        (show_cont_list show_ltype_mpstk conts)
  | RecvL (role, conts) ->
      Printf.sprintf "%s&%s" (RoleName.user role)
        (show_cont_list show_ltype_mpstk conts)
  | MuL (tv, cont) ->
      Printf.sprintf "μ(%s)(%s)"
        (TypeVariableName.user tv)
        (show_ltype_mpstk cont)
  | TVarL tv -> TypeVariableName.user tv
  | EndL -> "end"
