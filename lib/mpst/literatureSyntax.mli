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

val from_gtype : Gtype.t -> global

val from_ltype : Ltype.t -> local

val show_gtype_mpstk : global -> string

val show_ltype_mpstk : local -> string
