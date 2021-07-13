open! Base
open Printf
open Names
open Syntaxtree

(* Workaround for https://github.com/janestreet/sexplib/issues/34 *)
module Sexp = struct
  (* The Jane Street sexplib library does not distinguish between quoted
     strings and unquoted strings, which have semantic differences in
     SMT-LIB. Quoted strings are literals and unquoted strings are variables.*)

  type t = Literal of string | Atom of string | List of t list

  let to_string_buf buf =
    let rec to_string = function
      | Literal s ->
          Buffer.add_char buf '\"' ;
          Buffer.add_string buf (String.escaped s) ;
          Buffer.add_char buf '\"'
      | Atom s -> Buffer.add_string buf s
      | List l ->
          Buffer.add_char buf '(' ;
          List.iteri
            ~f:(fun idx s ->
              if idx <> 0 then Buffer.add_char buf ' ' ;
              to_string s )
            l ;
          Buffer.add_char buf ')'
    in
    to_string

  let to_string s =
    let buffer = Buffer.create 16 in
    to_string_buf buffer s ; Buffer.contents buffer
end

(** An expression, used in RefinementType extension *)
type t =
  | Var of VariableName.t  (** A variable *)
  | Int of int  (** An integer constant *)
  | Bool of bool  (** An boolean constant *)
  | String of string  (** A string literal *)
  | Binop of Syntax.binop * t * t  (** A binary operator *)
  | Unop of Syntax.unop * t  (** An unary operator *)
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

(** Types for expressions. Integers, booleans and strings are are modelled,
    and can be thus refined with RefinementTypes extension *)
type payload_type =
  | PTInt  (** A type for integers *)
  | PTBool  (** A type for booleans *)
  | PTString  (** A type for strings *)
  | PTUnit  (** A type for units *)
  | PTAbstract of PayloadTypeName.t
      (** A type for other un-modelled payloads, e.g. custom types *)
  | PTRefined of VariableName.t * payload_type * t
      (** A refined types, [PTRefined (x, ty, e)] stands for the refined type
          'x:ty\{e\}' where [e] is a predicate on [x]. *)
[@@deriving sexp_of, eq, ord]

let rec equal_payload_type_basic t1 t2 =
  match (t1, t2) with
  | PTInt, PTInt -> true
  | PTBool, PTBool -> true
  | PTString, PTString -> true
  | PTUnit, PTUnit -> true
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
  | PTUnit -> "unit"

(** Obtain [PayloadTypeName.t] from a [payload_type], useful for code
    generation purposes *)
let rec payload_typename_of_payload_type = function
  | PTInt -> PayloadTypeName.of_string "int"
  | PTBool -> PayloadTypeName.of_string "bool"
  | PTString -> PayloadTypeName.of_string "string"
  | PTUnit -> PayloadTypeName.of_string "unit"
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
                   typecheck_basic env e1 t && typecheck_basic env e2 t )
                 [PTInt; PTBool; PTString]
      in
      typecheck_binop env e1 e2 binop

let is_well_formed_type env = function
  | PTAbstract _ -> true
  | PTInt -> true
  | PTBool -> true
  | PTString -> true
  | PTUnit -> true
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
  | Int i -> Sexp.Atom (Int.to_string i)
  | Bool b -> Sexp.Atom (Bool.to_string b)
  | String s -> Sexp.Literal s
  | Binop (binop, e1, e2) ->
      let binop = sexp_of_binop binop in
      let e1 = sexp_of_expr e1 in
      let e2 = sexp_of_expr e2 in
      Sexp.List [binop; e1; e2]
  | Unop (unop, e) ->
      let unop = sexp_of_unop unop in
      let e = sexp_of_expr e in
      Sexp.List [unop; e]

type smt_script =
  {declare_consts: string Map.M(String).t; asserts: Sexp.t list}

let empty_smt_script =
  {declare_consts= Map.empty (module String); asserts= []}

let rec smt_sort_of_type = function
  | PTInt -> "Int"
  | PTBool -> "Bool"
  | PTString -> "String"
  | PTUnit -> "Int" (* SMT does not have a separate sort for units *)
  | PTAbstract n ->
      Err.unimpl
        (Printf.sprintf "Type %s is currently not supported for SMT encoding"
           (PayloadTypeName.user n) )
  | PTRefined (_, t, _) -> smt_sort_of_type t

let add_const var ty env =
  let {declare_consts; _} = env in
  let key = VariableName.user var in
  let data = smt_sort_of_type ty in
  let declare_consts =
    match Map.find declare_consts key with
    | Some data_ ->
        if String.equal data data_ then declare_consts
        else Err.unimpl "Handling multiply defined variables"
    | None -> Map.add_exn declare_consts ~key ~data
  in
  {env with declare_consts}

let add_assert_expr assert_ env =
  {env with asserts= sexp_of_expr assert_ :: env.asserts}

let add_assert_s_expr assert_ env = {env with asserts= assert_ :: env.asserts}

let encode_env env =
  let init = empty_smt_script in
  Map.fold ~init
    ~f:(fun ~key ~data env ->
      let env = add_const key data env in
      let env =
        match data with
        | PTRefined (v, _, e) ->
            let env = add_assert_expr e env in
            let env =
              if VariableName.equal v key then env
              else add_assert_expr (Binop (Syntax.Eq, Var v, Var key)) env
            in
            env
        | _ -> env
      in
      env )
    env

let check_sat {declare_consts; asserts} =
  let buffer = Buffer.create 4096 in
  Map.iteri
    ~f:(fun ~key:var ~data:sort ->
      Buffer.add_string buffer
        (Printf.sprintf "(declare-const %s %s)\n" var sort) )
    declare_consts ;
  List.iter
    ~f:(fun sexp ->
      Buffer.add_string buffer
        (Printf.sprintf "(assert %s)\n" (Sexp.to_string sexp)) )
    asserts ;
  Buffer.add_string buffer "(check-sat)" ;
  let script = Buffer.contents buffer in
  let result = Solver.run_solver script in
  match String.strip result with
  | "sat" -> `Sat
  | "unsat" -> `Unsat
  | "unknown" -> `Unknown
  | result -> failwith @@ "Unexpected response from solver:" ^ result

let is_unsat script =
  match check_sat script with `Unsat -> true | _ -> false

let subtype env t1 t2 =
  match (t1, t2) with
  (* Types with different base types are never subtypes *)
  | t1, t2 when not (equal_payload_type_basic t1 t2) -> false
  (* Subtyping is reflexive *)
  | t1, t2 when equal_payload_type t1 t2 -> true
  (* Shortcut for base types: Base types are always supertypes of refined
     types *)
  | PTRefined (_, PTInt, _), PTInt
   |PTRefined (_, PTBool, _), PTBool
   |PTRefined (_, PTString, _), PTString ->
      true
  | PTRefined (v1, t, e1), PTRefined (v2, _, e2) ->
      let env = encode_env env in
      let env = add_const v1 t env in
      let env = add_const v2 t env in
      let env =
        if not (VariableName.equal v1 v2) then
          add_assert_expr (Binop (Syntax.Eq, Var v1, Var v2)) env
        else env
      in
      let env = add_assert_expr e1 env in
      let env = add_assert_expr (Unop (Syntax.Not, e2)) env in
      is_unsat env
  | _, _ -> assert false

let count = ref 0

let fresh_var () =
  let var = Printf.sprintf "freshvar$%d" !count in
  let var = VariableName.of_string var in
  count := !count + 1 ;
  var

let infer_type env = function
  | Var v -> Map.find env v
  | (Int _ | Bool _ | String _) as e ->
      let var = fresh_var () in
      let t =
        match e with
        | Int _ -> PTInt
        | Bool _ -> PTBool
        | String _ -> PTString
        | _ -> assert false
      in
      Some (PTRefined (var, t, Binop (Syntax.Eq, Var var, e)))
  | Binop (b, e1, e2) as e -> (
      let var = fresh_var () in
      match b with
      | Syntax.Add | Syntax.Minus ->
          if typecheck_basic env e1 PTInt && typecheck_basic env e2 PTInt
          then Some (PTRefined (var, PTInt, Binop (Syntax.Eq, Var var, e)))
          else None
      | Syntax.And | Syntax.Or ->
          if typecheck_basic env e1 PTBool && typecheck_basic env e2 PTBool
          then Some (PTRefined (var, PTBool, Binop (Syntax.Eq, Var var, e)))
          else None
      | Syntax.Lt | Syntax.Gt | Syntax.Geq | Syntax.Leq ->
          if typecheck_basic env e1 PTInt && typecheck_basic env e2 PTInt
          then Some (PTRefined (var, PTBool, Binop (Syntax.Eq, Var var, e)))
          else None
      | Syntax.Eq | Syntax.Neq ->
          if
            List.exists
              ~f:(fun t ->
                typecheck_basic env e1 t && typecheck_basic env e2 t )
              [PTInt; PTBool; PTString]
          then Some (PTRefined (var, PTBool, Binop (Syntax.Eq, Var var, e)))
          else None )
  | Unop (u, e_) as e -> (
      let var = fresh_var () in
      match u with
      | Syntax.Neg ->
          if typecheck_basic env e_ PTInt then
            Some (PTRefined (var, PTInt, Binop (Syntax.Eq, Var var, e)))
          else None
      | Syntax.Not ->
          if typecheck_basic env e_ PTBool then
            Some (PTRefined (var, PTBool, Binop (Syntax.Eq, Var var, e)))
          else None )

let check_type env expr ty =
  match infer_type env expr with
  | Some inferred -> subtype env inferred ty
  | None -> false

let rec free_var = function
  | Var v -> Set.singleton (module VariableName) v
  | Int _ | Bool _ | String _ -> Set.empty (module VariableName)
  | Binop (_, e1, e2) -> Set.union (free_var e1) (free_var e2)
  | Unop (_, e) -> free_var e

let rec substitute ~from ~replace = function
  | Var v when VariableName.equal from v -> replace
  | (Var _ | Int _ | Bool _ | String _) as e -> e
  | Binop (b, e1, e2) ->
      Binop (b, substitute ~from ~replace e1, substitute ~from ~replace e2)
  | Unop (u, e) -> Unop (u, substitute ~from ~replace e)

let rec default_value = function
  | PTInt -> Int 0
  | PTBool -> Bool true
  | PTString -> String ""
  | PTUnit -> Err.unimpl "unit as an expression"
  | PTAbstract typename ->
      Err.violationf "No default value available for %s"
        (PayloadTypeName.user typename)
  | PTRefined (_, ty, _) -> default_value ty

let ensure_satisfiable env =
  let encoded = encode_env env in
  match check_sat encoded with
  | `Sat -> ()
  | `Unsat -> Err.uerr Err.UnsatisfiableRefinement
  | `Unknown -> Err.violation "Solver returned unknown result"
