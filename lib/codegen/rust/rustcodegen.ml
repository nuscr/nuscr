open! Base
open Names
open Gtype
open Efsm
open Message
open Syntax
open Rustefsm

let generate_big_derive buffer =
  Buffer.add_string buffer "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n"

let generate_small_derive buffer =
  Buffer.add_string buffer "#[derive(Debug, Clone, PartialEq, Eq)]\n"

let generate_state_enum buffer var_map g =
  generate_small_derive buffer ;
  Buffer.add_string buffer "enum State {\n" ;
  G.iter_vertex
    (fun state ->
      let vars = Map.find_exn var_map state in
      match vars with
      | [] -> Buffer.add_string buffer (Printf.sprintf "    S%d,\n" state)
      | _ ->
          let fields =
            List.map vars ~f:(fun (v, ty) ->
                Printf.sprintf "%s: %s" (VariableName.user v)
                  (Rustexpr.rust_type_of_payload_type ty) )
          in
          Buffer.add_string buffer
            (Printf.sprintf "    S%d { %s },\n" state
               (String.concat ~sep:", " fields) ) )
    g ;
  Buffer.add_string buffer "}\n"

let generate_labels buffer g =
  let labels = collect_labels g in
  generate_big_derive buffer ;
  Buffer.add_string buffer "pub enum Label {\n" ;
  Set.iter labels ~f:(fun label ->
      Buffer.add_string buffer ("    " ^ upper_camel_case label ^ ",\n") ) ;
  Buffer.add_string buffer "}\n"

let generate_support_types buffer =
  generate_big_derive buffer ;
  Buffer.add_string buffer
    "pub enum Direction {\n    Send,\n    Recv,\n}\n\n" ;
  generate_small_derive buffer ;
  Buffer.add_string buffer
    "pub enum Value {\n\
    \    Int(i64),\n\
    \    Bool(bool),\n\
    \    String(String),\n\
    \    Unit,\n\
     }\n\n" ;
  generate_small_derive buffer ;
  Buffer.add_string buffer
    "pub struct Action {\n\
    \    dir: Direction,\n\
    \    label: Label,\n\
    \    payloads: Vec<Value>,\n\
     }\n"

let generate_monitor_struct buffer protocol_name =
  generate_small_derive buffer ;
  Buffer.add_string buffer
    (Printf.sprintf "pub struct %sMonitor {\n    state: State,\n}\n"
       protocol_name )

let fmt_state_variant state fields =
  match fields with
  | [] -> Printf.sprintf "S%d" state
  | _ -> Printf.sprintf "S%d { %s }" state (String.concat ~sep:", " fields)

let generate_constructor buffer start var_map rec_var_info =
  let start_vars = Map.find_exn var_map start in
  let start_rv_info =
    Option.value ~default:[] (Map.find rec_var_info start)
  in
  List.iter start_vars ~f:(fun (v, _) ->
      let is_rec_var =
        List.exists start_rv_info ~f:(fun rv ->
            VariableName.equal v rv.rv_name )
      in
      if not is_rec_var then
        Err.violationf ~here:[%here]
          "Payload variable '%s' at start state has no initial value"
          (VariableName.user v) ) ;
  let inits =
    List.map start_vars ~f:(fun (v, _) ->
        let rv =
          List.find_exn start_rv_info ~f:(fun rv ->
              VariableName.equal v rv.rv_name )
        in
        Printf.sprintf "%s: %s" (VariableName.user v)
          (Rustexpr.rust_show_expr rv.rv_init_expr) )
  in
  Buffer.add_string buffer
    (Printf.sprintf
       "    pub fn new() -> Self {\n\
       \        Self { state: State::%s }\n\
       \    }\n"
       (fmt_state_variant start inits) )

(* Precondition: silent vars have been stripped by [rm_silent_var]. *)
let compute_rec_var_updates rannot dst_rv_info =
  let n_updates = List.length rannot.rec_expr_updates in
  let n_rec_vars = List.length dst_rv_info in
  if n_updates > n_rec_vars then
    Err.violationf ~here:[%here]
      "More rec_expr_updates (%d) than destination rec vars (%d)" n_updates
      n_rec_vars ;
  let paired =
    List.zip_exn rannot.rec_expr_updates (List.take dst_rv_info n_updates)
  in
  List.map paired ~f:(fun (e, rv) ->
      let binding = "new_" ^ VariableName.user rv.rv_name in
      (rv.rv_name, binding, e, rv.rv_ty) )

let find_new_rec_vars src_vars dst_rv_info rec_var_updates =
  List.filter_map dst_rv_info ~f:(fun rv ->
      let in_src =
        List.exists src_vars ~f:(fun (v, _) ->
            VariableName.equal v rv.rv_name )
      in
      let has_update =
        List.exists rec_var_updates ~f:(fun (name, _, _, _) ->
            VariableName.equal name rv.rv_name )
      in
      if (not in_src) && not has_update then
        Some (rv.rv_name, Rustexpr.rust_show_expr rv.rv_init_expr)
      else None )

let build_dst_field_inits dst_vars rec_var_updates new_rec_vars =
  List.map dst_vars ~f:(fun (v, _) ->
      let name = VariableName.user v in
      let updated_binding =
        List.find_map rec_var_updates ~f:(fun (rv, binding, _, _) ->
            Option.some_if (VariableName.equal v rv) binding )
      in
      let init_expr =
        List.find_map new_rec_vars ~f:(fun (rv, expr) ->
            Option.some_if (VariableName.equal v rv) expr )
      in
      match (updated_binding, init_expr) with
      | Some binding, _ -> Printf.sprintf "%s: %s" name binding
      | None, Some expr -> Printf.sprintf "%s: %s" name expr
      | None, None -> name )

let generate_step_fn buffer g var_map rec_var_info =
  Buffer.add_string buffer
    "\n\
    \    pub fn step(&mut self, action: &Action) -> bool {\n\
    \        match (self.state.clone(), &action.dir, &action.label) {\n" ;
  G.iter_edges_e
    (fun (src, a, dst) ->
      match a with
      | SendA (_, m, rannot) | RecvA (_, m, rannot) ->
          let dir =
            match a with
            | SendA _ -> "Send"
            | RecvA _ -> "Recv"
            | Epsilon -> assert false
          in
          let src_vars = Map.find_exn var_map src in
          let dst_vars = Map.find_exn var_map dst in
          let dst_rv_info =
            Option.value ~default:[] (Map.find rec_var_info dst)
          in
          let rec_var_updates = compute_rec_var_updates rannot dst_rv_info in
          let new_rec_vars =
            find_new_rec_vars src_vars dst_rv_info rec_var_updates
          in
          let dst_field_inits =
            build_dst_field_inits dst_vars rec_var_updates new_rec_vars
          in
          let src_fields =
            List.map src_vars ~f:(fun (v, _) -> VariableName.user v)
          in
          (* State + direction + label *)
          Buffer.add_string buffer
            (Printf.sprintf
               "            (State::%s, Direction::%s, Label::%s) =>\n"
               (fmt_state_variant src src_fields)
               dir
               (upper_camel_case (LabelName.user m.label)) ) ;
          (* Payload match *)
          Buffer.add_string buffer
            (Printf.sprintf
               "                match action.payloads.as_slice() {\n\
               \                    %s => {\n"
               (Rustexpr.rust_payload_slice_pattern m.payload) ) ;
          (* Clone payload bindings from slice refs to owned values *)
          let payload_vars = find_payload_vars m in
          List.iter payload_vars ~f:(fun (v, _) ->
              let name = VariableName.user v in
              Buffer.add_string buffer
                (Printf.sprintf
                   "                        let %s = %s.clone();\n" name name ) ) ;
          (* Payload constraints *)
          Option.iter (Rustexpr.rust_payload_constraints m.payload)
            ~f:(fun c ->
              Buffer.add_string buffer
                (Printf.sprintf
                   "                        if !(%s) { return false; }\n" c ) ) ;
          (* Rec var update let-bindings *)
          List.iter rec_var_updates ~f:(fun (_, binding, expr, _) ->
              Buffer.add_string buffer
                (Printf.sprintf "                        let %s = %s;\n"
                   binding
                   (Rustexpr.rust_show_expr expr) ) ) ;
          (* Rec var type constraints (on updated values only) *)
          List.iter rec_var_updates ~f:(fun (_, binding, _, rv_ty) ->
              match rv_ty with
              | Expr.PTRefined (binder, _, pred) ->
                  let pred =
                    Expr.substitute ~from:binder
                      ~replace:(Var (VariableName.of_string binding))
                      pred
                  in
                  Buffer.add_string buffer
                    (Printf.sprintf
                       "                        if !(%s) { return false; }\n"
                       (Rustexpr.rust_show_expr pred) )
              | _ -> () ) ;
          (* Transition to next state *)
          Buffer.add_string buffer
            (Printf.sprintf
               "                        self.state = State::%s;\n\
               \                        true\n\
               \                    }\n\
               \                    _ => false\n\
               \                },\n"
               (fmt_state_variant dst dst_field_inits) )
      | Epsilon -> () )
    g ;
  Buffer.add_string buffer "            _ => false\n        }\n    }\n"

let generate_impl buffer start g protocol_name var_map rec_var_info =
  Buffer.add_string buffer
    (Printf.sprintf "impl %sMonitor {\n" protocol_name) ;
  generate_constructor buffer start var_map rec_var_info ;
  generate_step_fn buffer g var_map rec_var_info ;
  Buffer.add_string buffer "}\n"

let gen_code (start, (g, rec_var_info)) ~protocol =
  let rec_var_info = rm_silent_var rec_var_info in
  let var_map = compute_var_map start g rec_var_info in
  let protocol_name = upper_camel_case @@ ProtocolName.user protocol in
  let buffer = Buffer.create 4096 in
  generate_state_enum buffer var_map g ;
  Buffer.add_string buffer "\n" ;
  generate_labels buffer g ;
  Buffer.add_string buffer "\n" ;
  generate_support_types buffer ;
  Buffer.add_string buffer "\n" ;
  generate_monitor_struct buffer protocol_name ;
  Buffer.add_string buffer "\n" ;
  generate_impl buffer start g protocol_name var_map rec_var_info ;
  Buffer.contents buffer
