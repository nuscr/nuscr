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

let rec equal_payload_type_basic t1 t2 =
  match (t1, t2) with
  | PTInt, PTInt -> true
  | PTBool, PTBool -> true
  | PTString, PTString -> true
  | PTAbstract n1, PTAbstract n2 -> PayloadTypeName.equal n1 n2
  | PTRefined (_, t1, _), t2 -> equal_payload_type_basic t1 t2
  | t1, PTRefined (_, t2, _) -> equal_payload_type_basic t1 t2
  | _, _ -> false

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

(* let env_print env =
 *   Map.iteri
 *     ~f:(fun ~key ~data ->
 *       Stdio.print_endline
 *         (Printf.sprintf "%s: %s" (VariableName.user key)
 *            (show_payload_type data)))
 *     env
 *)

let rec typecheck_basic env expr ty =
  match expr with
  | Int _ -> equal_payload_type_basic ty PTInt
  | Bool _ -> equal_payload_type_basic ty PTBool
  | String _ -> equal_payload_type_basic ty PTString
  | Var v -> (
    match Map.find env v with
    | Some ty_ -> equal_payload_type_basic ty ty_
    | None -> false )
  | Unop (unop, e) ->
      let typecheck_unop env e = function
        | Syntax.Neg ->
            typecheck_basic env e PTInt && equal_payload_type_basic ty PTInt
        | Syntax.Not ->
            typecheck_basic env e PTBool
            && equal_payload_type_basic ty PTBool
      in
      typecheck_unop env e unop
  | Binop (binop, e1, e2) ->
      let typecheck_binop env e1 e2 = function
        | Syntax.Add | Syntax.Minus ->
            typecheck_basic env e1 PTInt
            && typecheck_basic env e2 PTInt
            && equal_payload_type_basic ty PTInt
        | Syntax.And | Syntax.Or ->
            typecheck_basic env e1 PTBool
            && typecheck_basic env e2 PTBool
            && equal_payload_type_basic ty PTBool
        | Syntax.Lt | Syntax.Gt | Syntax.Geq | Syntax.Leq ->
            typecheck_basic env e1 PTInt
            && typecheck_basic env e2 PTInt
            && equal_payload_type_basic ty PTBool
        | Syntax.Eq | Syntax.Neq ->
            equal_payload_type_basic ty PTBool
            && List.exists
                 ~f:(fun t ->
                   typecheck_basic env e1 t && typecheck_basic env e2 t)
                 [PTInt; PTBool; PTString]
      in
      typecheck_binop env e1 e2 binop

let is_well_formed_type env = function
  | PTAbstract _ -> true
  | PTInt -> true
  | PTBool -> true
  | PTString -> true
  | PTRefined (v, t, e) ->
      let env = env_append env v t in
      typecheck_basic env e PTBool

let sexp_of_binop = function
  | Syntax.Add -> Sexp.Atom "+"
  | Syntax.Minus -> Sexp.Atom "-"
  | Syntax.Eq -> Sexp.Atom "="
  | Syntax.Neq -> Sexp.Atom "distinct"
  | Syntax.Lt -> Sexp.Atom "<"
  | Syntax.Gt -> Sexp.Atom ">"
  | Syntax.Leq -> Sexp.Atom "<="
  | Syntax.Geq -> Sexp.Atom ">="
  | Syntax.And -> Sexp.Atom "and"
  | Syntax.Or -> Sexp.Atom "or"

let sexp_of_unop = function
  | Syntax.Neg -> Sexp.Atom "-"
  | Syntax.Not -> Sexp.Atom "not"

let rec sexp_of_expr = function
  | Var v -> Sexp.Atom (VariableName.user v)
  | Int i -> Int.sexp_of_t i
  | Bool b -> Bool.sexp_of_t b
  | String _s -> failwith "Encode string for SMT"
  | Binop (binop, e1, e2) ->
      let binop = sexp_of_binop binop in
      let e1 = sexp_of_expr e1 in
      let e2 = sexp_of_expr e2 in
      Sexp.List [binop; e1; e2]
  | Unop (unop, e) ->
      let unop = sexp_of_unop unop in
      let e = sexp_of_expr e in
      Sexp.List [unop; e]
