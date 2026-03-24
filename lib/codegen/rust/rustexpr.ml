open! Base
open Names
open Syntax.Exprs
open Message

let rust_show_binop = function
  | Add -> "+"
  | Minus -> "-"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let rec rust_show_expr = function
  | Var v -> VariableName.user v
  | Int i -> Int.to_string i
  | Bool b -> if b then "true" else "false"
  | String s -> "\"" ^ s ^ "\".to_string()"
  | Binop (op, l, r) ->
      Printf.sprintf "(%s) %s (%s)" (rust_show_expr l) (rust_show_binop op)
        (rust_show_expr r)
  | Unop (Neg, e) -> Printf.sprintf "-(%s)" (rust_show_expr e)
  | Unop (Not, e) -> Printf.sprintf "!(%s)" (rust_show_expr e)
  | Unop (StrLen, e) -> Printf.sprintf "(%s).len() as i64" (rust_show_expr e)

let rec rust_type_of_payload_type = function
  | Expr.PTInt -> "i64"
  | Expr.PTBool -> "bool"
  | Expr.PTString -> "String"
  | Expr.PTUnit -> "()"
  | Expr.PTRefined (_, t, _) -> rust_type_of_payload_type t
  | Expr.PTAbstract n ->
      Err.unimpl ~here:[%here]
        (Printf.sprintf
           "in Rust codegen use bool, int, string or unit, abstract payload \
            type '%s'"
           (PayloadTypeName.user n) )

let rust_keywords =
  Set.of_list
    (module String)
    [ "as"
    ; "break"
    ; "const"
    ; "continue"
    ; "crate"
    ; "else"
    ; "enum"
    ; "extern"
    ; "false"
    ; "fn"
    ; "for"
    ; "if"
    ; "impl"
    ; "in"
    ; "let"
    ; "loop"
    ; "match"
    ; "mod"
    ; "move"
    ; "mut"
    ; "pub"
    ; "ref"
    ; "return"
    ; "self"
    ; "Self"
    ; "static"
    ; "struct"
    ; "super"
    ; "trait"
    ; "true"
    ; "type"
    ; "unsafe"
    ; "use"
    ; "where"
    ; "while"
    ; "state"
    ; "action"
    ; "label"
    ; "dir" ]

let rust_validate_identifier v =
  let name = VariableName.user v in
  if Set.mem rust_keywords name || String.is_prefix name ~prefix:"new_" then
    Err.uerr (Err.RustKeywordConflict v)

let rec rust_value_pattern_of_payload name_opt = function
  | Expr.PTInt ->
      let b = Option.value_map name_opt ~default:"_" ~f:VariableName.user in
      Printf.sprintf "Value::Int(%s)" b
  | Expr.PTBool ->
      let b = Option.value_map name_opt ~default:"_" ~f:VariableName.user in
      Printf.sprintf "Value::Bool(%s)" b
  | Expr.PTString ->
      let b = Option.value_map name_opt ~default:"_" ~f:VariableName.user in
      Printf.sprintf "Value::String(%s)" b
  | Expr.PTUnit -> "Value::Unit"
  | Expr.PTRefined (_, t, _) -> rust_value_pattern_of_payload name_opt t
  | Expr.PTAbstract n ->
      Err.unimpl ~here:[%here]
        (Printf.sprintf
           "in Rust codegen use bool, int, string or unit, abstract payload \
            type '%s'"
           (PayloadTypeName.user n) )

let rust_action_pattern dir label payload =
  let field_bindings =
    List.filter_map payload ~f:(function
      | PValue (Some v, _) -> rust_validate_identifier v ; Some (VariableName.user v)
      | PValue (None, _) -> None
      | PDelegate (protocol, role) ->
          Err.unimpl ~here:[%here]
            (Printf.sprintf
               "in Rust codegen, delegation to another protocol '%s'@'%s'"
               (ProtocolName.user protocol)
               (RoleName.user role) ) )
  in
  let fields =
    Printf.sprintf "dir: Direction::%s, %s" dir
      ( match field_bindings with
      | [] -> ".."
      | _ -> String.concat ~sep:", " field_bindings ^ ", .." )
  in
  Printf.sprintf "Action::%s { %s }" label fields

let rust_payload_constraints payloads =
  let preds =
    List.filter_map payloads ~f:(function
      | PValue (Some _, Expr.PTRefined (_, _, pred)) ->
          Some (rust_show_expr pred)
      | _ -> None )
  in
  match preds with [] -> None | _ -> Some (String.concat ~sep:" && " preds)
