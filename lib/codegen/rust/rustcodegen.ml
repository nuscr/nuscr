open! Base
open Names
open Gtype
open Efsm
open Message
open Syntax
open Rustefsm
open Rustexpr

let generate_derive buffer ~copy =
  let copy_str = if copy then ", Copy" else "" in
  Buffer.add_string buffer
    (Printf.sprintf "#[derive(Debug, Clone%s, PartialEq, Eq)]\n" copy_str)

let generate_state_enum buffer var_map g =
  generate_derive ~copy:false buffer ;
  Buffer.add_string buffer "#[allow(dead_code)]\n" ;
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
                  (rust_type_of_payload_type ty) )
          in
          Buffer.add_string buffer
            (Printf.sprintf "    S%d { %s },\n" state
               (String.concat ~sep:", " fields) ) )
    g ;
  Buffer.add_string buffer "    Error,\n" ;
  Buffer.add_string buffer "}\n"

let generate_monitor_struct buffer protocol_name =
  generate_derive ~copy:false buffer ;
  Buffer.add_string buffer
    (Printf.sprintf "pub struct %sMonitor { state: State }\n" protocol_name)

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
          (rust_show_expr rv.rv_init_expr) )
  in
  Buffer.add_string buffer
    (Printf.sprintf
       "    pub fn new() -> Self {\n\
       \        Self { state: State::%s }\n\
       \    }\n"
       (fmt_state_variant start inits) )

(* Precondition: silent vars have been stripped by [rm_silent_var]. Also:
   rec_expr_updates and dst_rv_info are paired positionally *)
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
        Some (rv.rv_name, rust_show_expr rv.rv_init_expr)
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
    \        match (&self.state, action) {\n\
    \            (State::Error, _) => true,\n" ;
  G.iter_edges_e
    (fun (src, a, dst) ->
      match a with
      | SendA (_, m, rannot) | RecvA (_, m, rannot) ->
          let dir =
            match a with
            | SendA _ -> "Send"
            | RecvA _ -> "Recv"
            | Epsilon ->
                Err.violationf ~here:[%here]
                  "Found Epsilon transition in EFSM, not supported in Rust \
                   codegen"
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
          let label = upper_camel_case (LabelName.user m.label) in
          (* State + Action variant with direction and field bindings *)
          Buffer.add_string buffer
            (Printf.sprintf "            (State::%s, %s) => {\n"
               (fmt_state_variant src src_fields)
               (rust_action_pattern dir label m.payload) ) ;
          (* Clone all bound variables (references from &self.state and
             &Action) *)
          let payload_vars = find_payload_vars m in
          let all_bindings = src_vars @ payload_vars in
          List.iter all_bindings ~f:(fun (v, _) ->
              let name = VariableName.user v in
              Buffer.add_string buffer
                (Printf.sprintf "                let %s = %s.clone();\n" name
                   name ) ) ;
          (* Payload constraints *)
          Option.iter (rust_payload_constraints m.payload) ~f:(fun c ->
              Buffer.add_string buffer
                (Printf.sprintf
                   "                if !(%s) { self.state = State::Error; \
                    return false; }\n"
                   c ) ) ;
          (* Rec var update let-bindings *)
          List.iter rec_var_updates ~f:(fun (_, binding, expr, _) ->
              Buffer.add_string buffer
                (Printf.sprintf "                let %s = %s;\n" binding
                   (rust_show_expr expr) ) ) ;
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
                       "                if !(%s) { self.state = \
                        State::Error; return false; }\n"
                       (rust_show_expr pred) )
              | _ -> () ) ;
          (* Transition to next state *)
          Buffer.add_string buffer
            (Printf.sprintf
               "                self.state = State::%s;\n\
               \                true\n\
               \            }\n"
               (fmt_state_variant dst dst_field_inits) )
      | Epsilon -> () )
    g ;
  Buffer.add_string buffer
    "            _ => { self.state = State::Error; false }\n\
    \        }\n\
    \    }\n"

let generate_accepts_fn buffer g =
  let arms = collect_accepts_arms g in
  Buffer.add_string buffer
    "\n\
    \    pub fn accepts(&self, action: &Action) -> bool {\n\
    \        match action {\n" ;
  Map.iteri arms ~f:(fun ~key ~data:(payload, guards) ->
      let dir = String.prefix key (String.index_exn key ':') in
      let label = String.drop_prefix key (String.index_exn key ':' + 1) in
      let payload_list =
        List.map payload ~f:(fun (v, ty) -> PValue (Some v, ty))
      in
      let pattern = rust_action_pattern dir label payload_list in
      match guards with
      | [] ->
          Buffer.add_string buffer
            (Printf.sprintf "            %s => true,\n" pattern)
      | _ ->
          Buffer.add_string buffer
            (Printf.sprintf "            %s => {\n" pattern) ;
          let payload_names =
            Set.of_list (module VariableName) (List.map payload ~f:fst)
          in
          List.iter payload ~f:(fun (v, _) ->
              let name = VariableName.user v in
              Buffer.add_string buffer
                (Printf.sprintf "                let %s = %s.clone();\n" name
                   name ) ) ;
          let guard_fvs =
            List.fold guards
              ~init:(Set.empty (module VariableName))
              ~f:(fun acc e -> Set.union acc (Expr.free_var e))
          in
          Set.iter guard_fvs ~f:(fun v ->
              if not (Set.mem payload_names v) then
                let name = VariableName.user v in
                let stripped = strip_trailing_underscores name in
                Buffer.add_string buffer
                  (Printf.sprintf "                let %s = %s.clone();\n"
                     name stripped ) ) ;
          let disjoined =
            List.map guards ~f:rust_show_expr |> String.concat ~sep:" || "
          in
          Buffer.add_string buffer
            (Printf.sprintf "                %s\n            }\n" disjoined) ) ;
  Buffer.add_string buffer "            _ => false,\n        }\n    }\n"

let generate_impl buffer start g protocol_name var_map rec_var_info =
  Buffer.add_string buffer
    (Printf.sprintf "#[allow(unused_variables)]\nimpl %sMonitor {\n"
       protocol_name ) ;
  generate_constructor buffer start var_map rec_var_info ;
  generate_accepts_fn buffer g ;
  generate_step_fn buffer g var_map rec_var_info ;
  Buffer.add_string buffer "}\n"

let gen_code (start, (g, rec_var_info)) ~protocol =
  let rec_var_info = rm_silent_var rec_var_info in
  let var_map = compute_var_map start g rec_var_info in
  let protocol_name = upper_camel_case @@ ProtocolName.user protocol in
  let buffer = Buffer.create 4096 in
  generate_state_enum buffer var_map g ;
  Buffer.add_string buffer "\n" ;
  generate_monitor_struct buffer protocol_name ;
  Buffer.add_string buffer "\n" ;
  generate_impl buffer start g protocol_name var_map rec_var_info ;
  Buffer.contents buffer

let generate_direction buffer =
  Buffer.add_string buffer "pub enum Direction {\n    Recv,\n    Send,\n" ;
  Buffer.add_string buffer "}\n"

let generate_action buffer g =
  let label_fields = collect_labels_with_fields g in
  Buffer.add_string buffer "#[allow(dead_code)]\n" ;
  Buffer.add_string buffer "pub enum Action {\n" ;
  Map.iteri label_fields ~f:(fun ~key:label ~data:fields ->
      let field_decls =
        List.map fields ~f:(fun (v, ty) ->
            Printf.sprintf "%s: %s" (VariableName.user v)
              (rust_type_of_payload_type ty) )
      in
      let all_fields = "dir: Direction" :: field_decls in
      Buffer.add_string buffer
        (Printf.sprintf "    %s { %s },\n" label
           (String.concat ~sep:", " all_fields) ) ) ;
  Buffer.add_string buffer "}\n"

let gen_test_code (start, (g, rec_var_info)) ~protocol =
  let rec_var_info = rm_silent_var rec_var_info in
  let var_map = compute_var_map start g rec_var_info in
  let protocol_name = upper_camel_case @@ ProtocolName.user protocol in
  let buffer = Buffer.create 4096 in
  generate_direction buffer ;
  Buffer.add_string buffer "\n" ;
  generate_action buffer g ;
  Buffer.add_string buffer "\n" ;
  generate_state_enum buffer var_map g ;
  Buffer.add_string buffer "\n" ;
  generate_monitor_struct buffer protocol_name ;
  Buffer.add_string buffer "\n" ;
  generate_impl buffer start g protocol_name var_map rec_var_info ;
  Buffer.contents buffer
