(** A Multiparty Session Type representation that is similar to those used in
    the literature *)

open! Base
open Names

type 'a cont_list = (LabelName.t * PayloadTypeName.t list * 'a) list

(** Global Types with basic features, in a form that is similar to those seen
    in standard literature. (No choice constructor, all choices are directed) *)
type global =
  | BranchG of
      { g_br_from: RoleName.t
      ; g_br_to: RoleName.t
      ; g_br_cont: global cont_list }
      (** p -> q : \{ l_i(S_i) . G_i \}_\{i \in I\} *)
  | MuG of TypeVariableName.t * global  (** mu t. G *)
  | TVarG of TypeVariableName.t  (** t *)
  | EndG  (** end *)

type local =
  | SendL of RoleName.t * local cont_list
      (** !p \{ l_i(S_i) . L_i \}_\{i \in I\} *)
  | RecvL of RoleName.t * local cont_list
      (** ?p \{ l_i(S_i) . L_i \}_\{i \in I\} *)
  | MuL of TypeVariableName.t * local  (** mu t. L *)
  | TVarL of TypeVariableName.t  (** t *)
  | EndL  (** end *)

val from_gtype : Gtype.t -> global
(** Convert from a {!Gtype.t} *)

val from_ltype : Ltype.t -> local
(** Convert from a {!Ltype.t} *)

val show_gtype_mpstk : global -> string
(** Output a global type in a form recognised by MPSTK.
    https://github.com/alcestes/mpstk *)

val show_ltype_mpstk : local -> string
(** Output a local type in a form recognised by MPSTK.
    https://github.com/alcestes/mpstk *)

val show_gtype_tex : global -> string
(** Output a global type in a tex format using the package mpstmacros.
    https://github.com/fangyi-zhou/mpstmacros

    WARNING: identifiers are provided "as is", manual escaping may be
    required *)

val show_ltype_tex : local -> string
(** Output a local type in a tex format using the package mpstmacros.
    https://github.com/fangyi-zhou/mpstmacros

    WARNING: identifiers are provided "as is", manual escaping may be
    required *)
