open! Base
open Names
open Err

(** Various definitions and operations on Payload *)
type payload =
  | PValue of VariableName.t option * Expr.payload_type
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving sexp_of]

(* Ignoring variable names for now *)
let equal_payload p1 p2 =
  match (p1, p2) with
  | PValue (_, n1), PValue (_, n2) -> Expr.equal_payload_type n1 n2
  | PDelegate (pn1, rn1), PDelegate (pn2, rn2) ->
      ProtocolName.equal pn1 pn2 && RoleName.equal rn1 rn2
  | _, _ -> false

let equal_pvalue_payload p1 p2 =
  let var_name_equal = Option.equal VariableName.equal in
  match (p1, p2) with
  | PValue (v1, n1), PValue (v2, n2) ->
      var_name_equal v1 v2 && Expr.equal_payload_type n1 n2
  | _ -> equal_payload p1 p2

let compare_payload p1 p2 =
  match (p1, p2) with
  | PValue (_, ptn1), PValue (_, ptn2) -> Expr.compare_payload_type ptn1 ptn2
  | PValue _, PDelegate _ -> -1
  | PDelegate _, PValue _ -> 1
  | PDelegate (pn1, rn1), PDelegate (pn2, rn2) ->
      let comp_fst = ProtocolName.compare pn1 pn2 in
      if comp_fst = 0 then RoleName.compare rn1 rn2 else comp_fst

let show_payload = function
  | PValue (var, ty) ->
      let var =
        match var with
        | Some var -> VariableName.user var ^ ": "
        | None -> ""
      in
      Printf.sprintf "%s%s" var (Expr.show_payload_type ty)
  | PDelegate (proto, role) ->
      Printf.sprintf "%s @ %s" (ProtocolName.user proto) (RoleName.user role)

let pp_payload fmt p = Caml.Format.fprintf fmt "%s" (show_payload p)

let of_syntax_payload (payload : Syntax.payloadt) =
  let open Syntax in
  match payload with
  | PayloadName n -> PValue (None, Expr.parse_typename n)
  | PayloadDel (p, r) -> PDelegate (p, r)
  | PayloadBnd (var, n) -> PValue (Some var, Expr.parse_typename n)
  | PayloadRTy (Simple n) -> PValue (None, Expr.parse_typename n)
  | PayloadRTy (Refined (v, t, e)) ->
      if Pragma.refinement_type_enabled () then
        PValue (Some v, Expr.PTRefined (v, Expr.parse_typename t, e))
      else
        uerr
          (PragmaNotSet
             ( Pragma.show Pragma.RefinementTypes
             , "Refinement Types require RefinementTypes pragma to be set."
             ) )

let typename_of_payload = function
  | PValue (_, ty) -> Expr.payload_typename_of_payload_type ty
  | PDelegate _ -> Err.unimpl ~here:[%here] "delegation for code generation"

type message = {label: LabelName.t; payload: payload list}
[@@deriving eq, sexp_of, ord]

let show_message {label; payload} =
  Printf.sprintf "%s(%s)" (LabelName.user label)
    (String.concat ~sep:", " (List.map ~f:show_payload payload))

let pp_message fmt m = Caml.Format.fprintf fmt "%s" (show_message m)

let of_syntax_message (message : Syntax.message) =
  let open Syntax in
  match message with
  | Message {name; payload} ->
      {label= name; payload= List.map ~f:of_syntax_payload payload}
  | MessageName name -> {label= name; payload= []}
