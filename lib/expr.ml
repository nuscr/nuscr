open! Base
open Printf
open Names

type t =
  | Var of VariableName.t
  | Int of int
  | Bool of bool
  | String of string
  | Binop of Syntax.binop * t * t
  | Unop of Syntax.unop * t
[@@deriving sexp_of, eq, ord]

let rec show = function
  | Var v -> VariableName.user v
  | Int i -> Int.to_string i
  | Bool b -> Bool.to_string b
  | String s -> "\"" ^ s ^ "\""
  | Binop (b, e1, e2) ->
      sprintf "(%s)%s(%s)" (show e1) (Syntax.show_binop b) (show e2)
  | Unop (u, e) -> sprintf "%s(%s)" (Syntax.show_unop u) (show e)

let pp_expr fmt e = Caml.Format.fprintf fmt "%s" (show e)

let rec of_syntax_expr = function
  | Syntax.Var n -> Var (VariableName.of_name n)
  | Syntax.Int n -> Int n
  | Syntax.Bool n -> Bool n
  | Syntax.String n -> String n
  | Syntax.Binop (b, e1, e2) ->
      Binop (b, of_syntax_expr e1, of_syntax_expr e2)
  | Syntax.Unop (u, e) -> Unop (u, of_syntax_expr e)

type payload_type =
  | PTInt
  | PTBool
  | PTString
  | PTAbstract of PayloadTypeName.t
  | PTRefined of VariableName.t * payload_type * t
[@@deriving sexp_of, eq, ord]

let rec show_payload_type = function
  | PTAbstract n -> PayloadTypeName.user n
  | PTRefined (v, t, e) ->
      sprintf "%s:%s{%s}" (VariableName.user v) (show_payload_type t)
        (show e)
  | PTInt -> "int"
  | PTBool -> "bool"
  | PTString -> "string"

let rec payload_typename_of_payload_type = function
  | PTInt -> PayloadTypeName.of_string "int"
  | PTBool -> PayloadTypeName.of_string "bool"
  | PTString -> PayloadTypeName.of_string "string"
  | PTAbstract n -> n
  | PTRefined (_, t, _) -> payload_typename_of_payload_type t

type typing_env = payload_type Map.M(VariableName).t

let new_typing_env = Map.empty (module VariableName)

let env_append env var ty =
  match Map.add env ~key:var ~data:ty with
  | `Ok env -> env
  | `Duplicate -> Err.unimpl "alpha-converting variables"

let typecheck_basic env expr ty =
  match expr with
  | Int _ -> equal_payload_type ty PTInt
  | Bool _ -> equal_payload_type ty PTBool
  | Var v -> (
    match Map.find env v with
    | Some ty_ -> equal_payload_type ty ty_
    | None -> false )
  | e -> raise @@ Err.Violation ("Don't know how to deal with " ^ show e)

let is_well_formed_type env = function
  | PTAbstract _ -> true
  | PTInt -> true
  | PTBool -> true
  | PTString -> true
  | PTRefined (v, t, e) ->
      let env = env_append env v t in
      typecheck_basic env e PTBool
