open! Core_kernel
open Syntax
open Gtype
open Err

type local_type =
  | RecvL of message * name * local_type
  | SendL of message * name * local_type
  | ChoiceL of name * local_type list
  | TVarL of name
  | MuL of name * local_type
  | EndL

let rec project (gType : global_type) (roles : name list)
    (projected_role : name) =
  assert (List.mem roles projected_role ~equal:String.equal) ;
  match gType with
  | EndG -> EndL
  | TVarG name -> TVarL name
  | MuG (name, gType) -> MuL (name, project gType roles projected_role)
  | MessageG (m, send_r, recv_r, gType) -> (
    match projected_role with
    | _ when String.equal projected_role send_r ->
        SendL (m, recv_r, project gType roles projected_role)
    | _ when String.equal projected_role recv_r ->
        RecvL (m, send_r, project gType roles projected_role)
    | _ -> project gType roles projected_role )
  | ChoiceG (_choice_r, _gTypes) -> unimpl "Projection on Choices"
