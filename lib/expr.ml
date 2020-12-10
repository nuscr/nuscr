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
  | PTSimple of PayloadTypeName.t
  | PTRefined of VariableName.t * PayloadTypeName.t * t
[@@deriving sexp_of, eq, ord]

let show_payload_type = function
  | PTSimple n -> PayloadTypeName.user n
  | PTRefined (v, t, e) ->
      sprintf "%s:%s{%s}" (VariableName.user v) (PayloadTypeName.user t)
        (show e)

let payload_typename_of_payload_type = function
  | PTSimple n | PTRefined (_, n, _) -> n

type typing_env = payload_type Map.M(VariableName).t

let new_typing_env = Map.empty (module VariableName)
