(** Expressions, used in [RefinementTypes] pragma *)

open! Base
open Names
open Syntax.RawExpr

(** {1 Expressions} *)

(** An expression, used in RefinementType extension

    {v
 type t =
   | Var of VariableName.t  (** A variable *)
   | Int of int  (** An integer constant *)
   | Bool of bool  (** An boolean constant *)
   | String of string  (** A string literal *)
   | Binop of binop * t * t  (** A binary operator *)
   | Unop of unop * t  (** An unary operator *)
    v} *)
type t = expr [@@deriving sexp_of, eq, ord]

val show : t -> string

val free_var : t -> Set.M(VariableName).t
(** Get free variables in an expression *)

val substitute : from:VariableName.t -> replace:t -> t -> t
(** Perform substitutions on an expression *)

(* Workaround for https://github.com/janestreet/sexplib/issues/34 *)
(** An modified S-expression library that distinguishes literal strings and *
    atoms *)
module Sexp : sig
  (* The Jane Street sexplib library does not distinguish between quoted
     strings and unquoted strings, which have semantic differences in
     SMT-LIB. Quoted strings are literals and unquoted strings are
     variables.*)

  type t = Literal of string | Atom of string | List of t list

  val to_string : t -> string
end

val sexp_of_expr : t -> Sexp.t
(** Convert an expression to S-expression *)

(** {1 Types} *)

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

val show_payload_type : payload_type -> string

val payload_typename_of_payload_type : payload_type -> PayloadTypeName.t
(** Extract [PayloadTypeName] from a [payload_type] *)

val smt_sort_of_type : payload_type -> string
(** Get the SMT sort of a [payload_type] *)

val default_value : payload_type -> t
(** Get the default value of a payload type, which may not exist. *)

val parse_typename : PayloadTypeName.t -> payload_type
(** Convert a PayloadTypeName to a [payload_type] *)

(** {1 Typing} *)

type typing_env

val new_typing_env : typing_env

val is_well_formed_type : typing_env -> payload_type -> bool
(** Check whether a payload type is well-formed under the typing context *)

val ensure_satisfiable : typing_env -> unit
(** Validate whether the typing context is satisfiable, i.e. it does not
    contain in consistencies *)

val env_append : typing_env -> VariableName.t -> payload_type -> typing_env
(** Append an new entry into typing context *)

val check_type : typing_env -> t -> payload_type -> bool
(** Check whether an expression can be assigned a provided type under a
    typing context *)

(** {1 SMT Interfacing} *)

type smt_script

val add_assert_s_expr : Sexp.t -> smt_script -> smt_script
(** Add an assertion into a SMT script *)

val encode_env : typing_env -> smt_script
(** Encode a typing context into a SMT script *)

val check_sat : smt_script -> [`Sat | `Unsat | `Unknown]
(** Invoke SMT solver on an SMT script *)
