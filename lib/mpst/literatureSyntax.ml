open! Base
open Names
open Err

type 'a cont_list = (LabelName.t * PayloadTypeName.t * 'a) list

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

let from_gtype _ = unimpl ~here:[%here] "from_gtype"

let from_ltype _ = unimpl ~here:[%here] "from_ltype"

let show_gtype_mpstk _ = unimpl ~here:[%here] "show_gtype_mpstk"

let show_ltype_mpstk _ = unimpl ~here:[%here] "show_ltype_mpstk"
