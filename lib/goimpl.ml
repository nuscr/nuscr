open! Base
open Printf
open Names
open Gonames

(* FIXME: Use an AST *)

(* IMPORTS *)
let messages_import_path root pkg =
  sprintf "\"%s/%s/%s\"" (RootDirName.user root)
    (PackageName.user pkg_messages)
    (PackageName.user pkg)

let channels_import_path root pkg =
  sprintf "\"%s/%s/%s\"" (RootDirName.user root)
    (PackageName.user pkg_channels)
    (PackageName.user pkg)

let roles_import_path root pkg =
  sprintf "\"%s/%s\"" (RootDirName.user root) (PackageName.user pkg)

let invitations_import_path root pkg =
  sprintf "\"%s/%s\"" (RootDirName.user root) (PackageName.user pkg)

let callbacks_import_path root pkg =
  sprintf "\"%s/%s\"" (RootDirName.user root) (PackageName.user pkg)

let results_import_path root pkg =
  sprintf "\"%s/%s/%s\"" (RootDirName.user root)
    (PackageName.user pkg_results)
    (PackageName.user pkg)

let sync_import_path _ pkg = sprintf "\"%s\"" (PackageName.user pkg)

let gen_import import_path = sprintf "import %s" import_path

let gen_aliased_import import_path alias =
  sprintf "import %s %s" alias import_path

(* GO CODE *)
let incr_indent curr_indent = sprintf "\t%s" curr_indent

let indent_line indent line = sprintf "%s%s" indent line

let join_non_empty_lines ?(sep = "\n") lines =
  let lines = List.filter ~f:(fun line -> String.length line > 0) lines in
  String.concat ~sep lines

let join_params params = String.concat ~sep:", " params

let package_stmt pkg_name = sprintf "package %s" (PackageName.user pkg_name)

let pointer_type type_str = sprintf "*%s" type_str

let reference_var var =
  let ref_var = sprintf "&%s" (VariableName.user var) in
  VariableName.of_string ref_var

let chan_type type_str = sprintf "chan %s" type_str

let new_variable name_gen var_name =
  let name_gen, var_name = Namegen.unique_name name_gen var_name in
  let variable = VariableName.of_string var_name in
  (name_gen, variable)

let var_type_decl var type_name = sprintf "%s %s" var type_name

let new_var_decl var_name var_type =
  sprintf "var %s" (var_type_decl (VariableName.user var_name) var_type)

let new_var_assignment var_name rhs =
  sprintf "%s := %s" (VariableName.user var_name) rhs

let panic_with_msg msg = sprintf "panic(\"%s\")" msg

(* STRUCT DECLARATION *)
let struct_decl struct_name field_decls =
  let field_decls = List.sort field_decls ~compare:String.compare in
  let field_decls_str = join_non_empty_lines field_decls in
  sprintf "type %s struct {\n%s\n}" struct_name field_decls_str

let struct_literal_field_decl (field_name, field_value) =
  sprintf "%s: %s," field_name field_value

let struct_literal struct_name struct_fields field_values indent =
  let struct_literal_fields = List.zip_exn struct_fields field_values in
  let field_decls =
    List.map ~f:struct_literal_field_decl struct_literal_fields
  in
  let field_indent = incr_indent indent in
  let field_decls =
    List.map
      ~f:(fun field_decl -> indent_line field_indent field_decl)
      field_decls
  in
  let field_decls_str = join_non_empty_lines field_decls in
  sprintf "%s{\n%s\n%s}" struct_name field_decls_str indent

let struct_field_decl field_name field_type =
  sprintf "\t%s" (var_type_decl field_name field_type)

(* PKG/FIELD ACCESSES *)
let invitation_pkg_access invite_pkg struct_name =
  sprintf "%s.%s"
    (PackageName.user invite_pkg)
    (InviteChannelStructName.user struct_name)

let callbacks_pkg_enum callbacks_pkg enum_name =
  sprintf "%s.%s" (PackageName.user callbacks_pkg) (EnumName.user enum_name)

let callbacks_pkg_env callbacks_pkg env_name =
  sprintf "%s.%s"
    (PackageName.user callbacks_pkg)
    (CallbacksEnvName.user env_name)

let pkg_function pkg function_name =
  sprintf "%s.%s" (PackageName.user pkg) (FunctionName.user function_name)

let protocol_result_access pkg result =
  sprintf "%s.%s" (PackageName.user pkg) (ResultName.user result)

let protocol_msg_access pkg msg =
  sprintf "%s.%s" (PackageName.user pkg) (MessageStructName.user msg)

let protocol_channel_access pkg channel =
  sprintf "%s.%s" (PackageName.user pkg) (ChannelStructName.user channel)

let protocol_invite_channel_access pkg invite_channel =
  sprintf "%s.%s" (PackageName.user pkg)
    (InviteChannelStructName.user invite_channel)

let pkg_var_access pkg var_name =
  sprintf "%s.%s" (PackageName.user pkg) (VariableName.user var_name)

let channel_struct_field_access chan_var chan_name =
  sprintf "%s.%s" (VariableName.user chan_var) (ChannelName.user chan_name)

let invite_channel_struct_field_access chan_var chan_name =
  sprintf "%s.%s"
    (VariableName.user chan_var)
    (InviteChannelName.user chan_name)

(* INTERFACE DECLARATION *)
let interface_method_decl indent (function_name, params, return_type_str) =
  let params_str = join_params params in
  let method_decl =
    sprintf "%s(%s) %s"
      (FunctionName.user function_name)
      params_str return_type_str
  in
  indent_line indent method_decl

let gen_interface interface_name interface_method_decls =
  let indent = incr_indent "" in
  let interface_methods =
    List.map interface_method_decls ~f:(interface_method_decl indent)
  in
  let interface_methods_str = join_non_empty_lines interface_methods in
  sprintf "type %s interface {\n%s\n}"
    (InterfaceName.user interface_name)
    interface_methods_str

(* CALLBACKS INTERFACE *)
let callbacks_env_interface env_name callbacks =
  let gen_callback_decl (callback_name, param, return_val) =
    let params =
      match param with
      | None -> []
      | Some (param_name, `Result (pkg, result)) ->
          [ var_type_decl
              (ParameterName.user param_name)
              (protocol_result_access pkg result) ]
      | Some (param_name, `Msg (pkg, msg)) ->
          [ var_type_decl
              (ParameterName.user param_name)
              (protocol_msg_access pkg msg) ]
    in
    let return_type =
      match return_val with
      | None -> ""
      | Some (`Msg (pkg, msg)) -> protocol_msg_access pkg msg
      | Some (`Result (pkg, result)) -> protocol_result_access pkg result
      | Some (`Env env) -> CallbacksEnvName.user env
      | Some (`Enum enum) -> EnumTypeName.user enum
    in
    let callback_function =
      FunctionName.of_string (CallbackName.user callback_name)
    in
    (callback_function, params, return_type)
  in
  let callback_decls = List.map ~f:gen_callback_decl callbacks in
  let callbacks_interface_name =
    InterfaceName.of_string (CallbacksEnvName.user env_name)
  in
  gen_interface callbacks_interface_name callback_decls

(* ENUM DECLARATIONS *)
let enum_type_decl enum_type =
  sprintf "type %s %s" (EnumTypeName.user enum_type) int_type

let enum_value_decl enum_value enum_type =
  sprintf "%s %s = iota"
    (EnumName.user enum_value)
    (EnumTypeName.user enum_type)

let enum_decl enum_type enum_values =
  let enum_value_decls =
    match enum_values with
    | fst_value :: values ->
        let fst_decl = enum_value_decl fst_value enum_type in
        let decls = List.map ~f:EnumName.user values in
        String.concat ~sep:"\n\t" (fst_decl :: decls)
    | _ -> failwith "Enum decl should have at least one value"
  in
  sprintf "const (\n\t%s\n)" enum_value_decls

(* MESSAGE STRUCT *)
let msg_field_decl (field_name, field_type) =
  struct_field_decl
    (VariableName.user field_name)
    (PayloadTypeName.user field_type)

let gen_msg_struct (msg_struct_name, fields) =
  let struct_name = MessageStructName.user msg_struct_name in
  let field_decls = List.map ~f:msg_field_decl fields in
  struct_decl struct_name field_decls

(* ROLE CHANNEL STRUCT *)
let role_chan_field_decl (chan_name, (pkg, role_chan)) =
  let type_of_channel = protocol_channel_access pkg role_chan in
  let channel_type = chan_type type_of_channel in
  struct_field_decl (InviteChannelName.user chan_name) channel_type

(* INVITE CHANNEL STRUCT *)
let invite_chan_field_decl (chan_name, invite_chan_struct) =
  let channel_type =
    chan_type (InviteChannelStructName.user invite_chan_struct)
  in
  struct_field_decl (InviteChannelName.user chan_name) channel_type

(* CHANNEL OPERATIONS *)
let msg_from_channel chan_str = sprintf "<-%s" chan_str

let send_msg_over_channel chan_struct chan_field msg_var =
  let chan = channel_struct_field_access chan_struct chan_field in
  sprintf "%s <- %s" chan (VariableName.user msg_var)

let send_invite_over_channel invite_chan_struct (chan_field, chan_var) =
  let chan =
    invite_channel_struct_field_access invite_chan_struct chan_field
  in
  sprintf "%s <- %s" chan (VariableName.user chan_var)

let recv_from_invite_chan var_name invite_chan_struct chan_field =
  let chan =
    invite_channel_struct_field_access invite_chan_struct chan_field
  in
  let recv_from_chan = msg_from_channel chan in
  new_var_assignment var_name recv_from_chan

let recv_from_msg_chan var_name chan_struct chan_field =
  let chan = channel_struct_field_access chan_struct chan_field in
  let recv_from_chan = msg_from_channel chan in
  new_var_assignment var_name recv_from_chan

let make_async_chan chan_type =
  (* chan_type = "chan <type>" *)
  sprintf "make(%s, 1)" chan_type

let new_chan_var_assignment (chan_var, chan_type) =
  (* assume chan_type is of the form "chan <type>" *)
  new_var_assignment chan_var (make_async_chan chan_type)

let gen_case_stmt case_str = sprintf "case %s:" case_str

(* SWITCH STATEMENT *)
let gen_switch_stmt switch_var callbacks_pkg enum_values cases_impl indent
    default_impl =
  let gen_switch_case (case_stmt, impl) =
    sprintf "%s%s\n%s" indent case_stmt impl
  in
  let enums = List.map ~f:(callbacks_pkg_enum callbacks_pkg) enum_values in
  let case_stmts = List.map ~f:gen_case_stmt enums in
  let cases_and_impl = List.zip_exn case_stmts cases_impl in
  let switch_cases = List.map ~f:gen_switch_case cases_and_impl in
  let switch_cases =
    match default_impl with
    | None -> switch_cases
    | Some default_impl ->
        let default_case_impl =
          gen_switch_case (default_case, default_impl)
        in
        switch_cases @ [default_case_impl]
  in
  let switch_cases_str = join_non_empty_lines switch_cases in
  sprintf "%sswitch %s {\n%s\n%s}" indent
    (VariableName.user switch_var)
    switch_cases_str indent

(* SELECT STATEMENT *)
let gen_select_case impl case_indent =
  match impl with
  | recv_stmt :: impl' ->
      let select_case = gen_case_stmt recv_stmt in
      let select_case = indent_line case_indent select_case in
      let impl_indent = incr_indent case_indent in
      let impl' = List.map ~f:(indent_line impl_indent) impl' in
      (select_case :: impl', impl_indent)
  | [] -> failwith "Cannot have empty choice branch"

let gen_select_stmt select_cases indent =
  (* Assume each of the implementations is of the form: case <recv>:\n <impl> *)
  let select_cases_str = join_non_empty_lines select_cases in
  sprintf "%sselect {\n%s\n%s}" indent select_cases_str indent

(* FUNCTION DECLARATIONS *)
let function_decl function_name params return_type impl =
  let gen_param_decl (param_name, param_type) =
    sprintf "%s %s" (VariableName.user param_name) param_type
  in
  let return_type_str =
    match return_type with None -> "" | Some ret_type -> ret_type
  in
  let param_decls = List.map ~f:gen_param_decl params in
  let param_decls_str = join_params param_decls in
  sprintf "func %s(%s) %s {\n%s\n} "
    (FunctionName.user function_name)
    param_decls_str return_type_str impl

(* CONTROL FLOW *)
let return_stmt return_val = sprintf "return %s" return_val

let call_function function_name params =
  let str_params = List.map ~f:VariableName.user params in
  let params_str = join_params str_params in
  sprintf "%s(%s)" function_name params_str

let call_method obj_var method_name params =
  let params_str = join_params params in
  sprintf "%s.%s(%s)"
    (VariableName.user obj_var)
    (FunctionName.user method_name)
    params_str

let goroutine_call function_call = sprintf "go %s" function_call

let defer_call function_call = sprintf "defer %s" function_call
