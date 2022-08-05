open! Base
open Printf
open Loc
open Names

module type EXPRS_SIG = sig
  type expr =
    | Var of VariableName.t
    | Int of int
    | Bool of bool
    | String of string
    | Binop of binop * expr * expr
    | Unop of unop * expr

  and binop = Add | Minus | Eq | Neq | Lt | Gt | Leq | Geq | And | Or

  and unop = Neg | Not | StrLen [@@deriving eq, ord, show, sexp_of]
end

module Exprs : EXPRS_SIG = struct
  type expr =
    | Var of VariableName.t
    | Int of int
    | Bool of bool
    | String of string
    | Binop of binop * expr * expr
    | Unop of unop * expr

  and binop = Add | Minus | Eq | Neq | Lt | Gt | Leq | Geq | And | Or

  and unop = Neg | Not | StrLen [@@deriving eq, ord, show, sexp_of]

  let show_binop = function
    | Add -> "+"
    | Minus -> "-"
    | Eq -> "="
    | Neq -> "<>"
    | Lt -> "<"
    | Gt -> ">"
    | Leq -> "<="
    | Geq -> ">="
    | And -> "&&"
    | Or -> "||"

  let show_unop = function Neg -> "-" | Not -> "not " | StrLen -> "len"
end

include Exprs

type ty =
  | Simple of PayloadTypeName.t
  | Refined of VariableName.t * PayloadTypeName.t * expr
[@@deriving eq, ord, show, sexp_of]

type annotation = string [@@deriving show {with_path= false}, sexp_of]

type payloadt =
  | PayloadName of PayloadTypeName.t
  | PayloadDel of ProtocolName.t * RoleName.t (* protocol @ role *)
  | PayloadBnd of VariableName.t * PayloadTypeName.t
  | PayloadRTy of ty
[@@deriving eq, ord, sexp_of]

(* var : type *)

let show_payloadt = function
  | PayloadName n -> PayloadTypeName.user n
  | PayloadDel (p, r) ->
      sprintf "%s @ %s" (ProtocolName.user p) (RoleName.user r)
  | PayloadBnd (n, n') ->
      sprintf "%s: %s" (VariableName.user n) (PayloadTypeName.user n')
  | PayloadRTy ty -> show_ty ty

let pp_payloadt fmt p = Caml.Format.fprintf fmt "%s" (show_payloadt p)

type message =
  | Message of {name: LabelName.t; payload: payloadt list}
  | MessageName of LabelName.t
[@@deriving eq, ord]

let show_message = function
  | Message {name; payload} ->
      sprintf "%s(%s)" (LabelName.user name)
        (String.concat ~sep:", " (List.map ~f:show_payloadt payload))
  | MessageName n -> LabelName.user n

let pp_message fmt m = Caml.Format.fprintf fmt "%s" (show_message m)

let sexp_of_message m = Sexp.Atom (show_message m)

let message_label = function
  | Message {name; _} -> LabelName.user name
  | MessageName name -> LabelName.user name

let message_payload_ty =
  let payload_type_of_payload_t = function
    | PayloadName n -> PayloadTypeName.user n
    | PayloadDel (_p, _r) -> failwith "Delegation is not supported"
    | PayloadBnd (_n, n) -> PayloadTypeName.user n
    | PayloadRTy _ -> assert false
  in
  function
  | Message {payload; _} -> List.map ~f:payload_type_of_payload_t payload
  | _ -> []

type rec_var =
  {var: VariableName.t; roles: RoleName.t list; ty: payloadt; init: expr}
[@@deriving show {with_path= false}, sexp_of]

type global_interaction = raw_global_interaction located
[@@deriving show {with_path= false}, sexp_of]

and raw_global_interaction =
  | MessageTransfer of
      { message: message
      ; from_role: RoleName.t
      ; to_roles: RoleName.t list
      ; ann: annotation option }
  (* recursion variable, protocol *)
  | Recursion of TypeVariableName.t * rec_var list * global_interaction list
  | Continue of TypeVariableName.t * expr list
  (* role, protocol options *)
  | Choice of RoleName.t * global_interaction list list
  (* protocol * roles *)
  | Do of ProtocolName.t * RoleName.t list * annotation option
  (* caller * protocol * roles *)
  | Calls of
      RoleName.t * ProtocolName.t * RoleName.t list * annotation option
[@@deriving show {with_path= false}]

type global_protocol = raw_global_protocol located
[@@deriving show {with_path= false}, sexp_of]

and raw_global_protocol =
  { name: ProtocolName.t
  ; roles: RoleName.t list
  ; split_roles: RoleName.t list * RoleName.t list
  ; nested_protocols: global_protocol list
  ; interactions: global_interaction list
  ; ann: annotation option }
[@@deriving show {with_path= false}, sexp_of]

type scr_module =
  { pragmas: Pragma.pragmas
  ; nested_protocols: global_protocol list
  ; protocols: global_protocol list }
[@@deriving show {with_path= false}]
