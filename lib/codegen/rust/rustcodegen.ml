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

let generate_state_enum buffer var_map g protocol_name =
  generate_derive ~copy:true buffer ;
  Buffer.add_string buffer "#[allow(dead_code)]\n" ;
  Buffer.add_string buffer (Printf.sprintf "enum %sState {\n" protocol_name) ;
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
    (Printf.sprintf "pub struct %sMonitor { state: %sState }\n" protocol_name
       protocol_name )

let fmt_state_variant state fields =
  match fields with
  | [] -> Printf.sprintf "S%d" state
  | _ -> Printf.sprintf "S%d { %s }" state (String.concat ~sep:", " fields)

let generate_constructor buffer start var_map rec_var_info protocol_name =
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
       \        Self { state: %sState::%s }\n\
       \    }\n"
       protocol_name
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

let emit_branch_body buffer indent branch src_vars var_map rec_var_info
    protocol_name =
  let dst_vars = Map.find_exn var_map branch.sb_dst in
  let dst_rv_info =
    Option.value ~default:[] (Map.find rec_var_info branch.sb_dst)
  in
  let rec_var_updates =
    compute_rec_var_updates branch.sb_rannot dst_rv_info
  in
  let new_rec_vars =
    find_new_rec_vars src_vars dst_rv_info rec_var_updates
  in
  let dst_field_inits =
    build_dst_field_inits dst_vars rec_var_updates new_rec_vars
  in
  List.iter rec_var_updates ~f:(fun (_, binding, expr, _) ->
      Buffer.add_string buffer
        (Printf.sprintf "%slet %s = %s;\n" indent binding
           (rust_show_expr expr) ) ) ;
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
               "%sif !(%s) { self.state = %sState::Error; return false; }\n"
               indent (rust_show_expr pred) protocol_name )
      | _ -> () ) ;
  Buffer.add_string buffer
    (Printf.sprintf "%sself.state = %sState::%s;\n%strue\n" indent
       protocol_name
       (fmt_state_variant branch.sb_dst dst_field_inits)
       indent )

let generate_step_fn buffer g var_map rec_var_info protocol_name =
  Buffer.add_string buffer
    (Printf.sprintf
       "\n\
       \    pub fn step(&mut self, action: &Action) -> bool {\n\
       \        match (&self.state, action) {\n\
       \            (%sState::Error, _) => true,\n"
       protocol_name ) ;
  Map.iter (group_step_arms g)
    ~f:(fun (src, dir, label, merged_payload, branches) ->
      let src_vars = Map.find_exn var_map src in
      let src_fields =
        List.map src_vars ~f:(fun (v, _) -> VariableName.user v)
      in
      let merged_pvalues =
        List.map merged_payload ~f:(fun (v, ty) -> PValue (Some v, ty))
      in
      Buffer.add_string buffer
        (Printf.sprintf "            (%sState::%s, %s) => {\n" protocol_name
           (fmt_state_variant src src_fields)
           (rust_action_pattern dir label merged_pvalues) ) ;
      let all_bindings = src_vars @ merged_payload in
      List.iter all_bindings ~f:(fun (v, _) ->
          let name = VariableName.user v in
          Buffer.add_string buffer
            (Printf.sprintf "                let %s = *%s;\n" name name) ) ;
      ( match branches with
      | [branch] ->
          Option.iter (rust_payload_constraints branch.sb_m.payload)
            ~f:(fun c ->
              Buffer.add_string buffer
                (Printf.sprintf
                   "                if !(%s) { self.state = %sState::Error; \
                    return false; }\n"
                   c protocol_name ) ) ;
          emit_branch_body buffer "                " branch src_vars var_map
            rec_var_info protocol_name
      | _ ->
          (* GuardedUniqueness guarantees the payload guards are mutually
             exclusive, so branching on payload guards alone is sound. *)
          let rec go first = function
            | [] ->
                Buffer.add_string buffer
                  (Printf.sprintf
                     "                } else {\n\
                     \                    self.state = %sState::Error;\n\
                     \                    false\n\
                     \                }\n"
                     protocol_name )
            | branch :: rest -> (
                let guard = rust_payload_constraints branch.sb_m.payload in
                ( match (first, guard) with
                | true, Some g ->
                    Buffer.add_string buffer
                      (Printf.sprintf "                if %s {\n" g)
                | false, Some g ->
                    Buffer.add_string buffer
                      (Printf.sprintf "                } else if %s {\n" g)
                | _, None ->
                    Buffer.add_string buffer
                      ( if first then "                {\n"
                        else "                } else {\n" ) ) ;
                emit_branch_body buffer "                    " branch
                  src_vars var_map rec_var_info protocol_name ;
                match guard with
                | None -> Buffer.add_string buffer "                }\n"
                | Some _ -> go false rest )
          in
          go true branches ) ;
      Buffer.add_string buffer "            }\n" ) ;
  Buffer.add_string buffer
    (Printf.sprintf
       "            _ => { self.state = %sState::Error; false }\n\
       \        }\n\
       \    }\n"
       protocol_name )

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
                (Printf.sprintf "                let %s = *%s;\n" name name) ) ;
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
                  (Printf.sprintf "                let %s = %s;\n" name
                     stripped ) ) ;
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
  generate_constructor buffer start var_map rec_var_info protocol_name ;
  generate_accepts_fn buffer g ;
  generate_step_fn buffer g var_map rec_var_info protocol_name ;
  Buffer.add_string buffer "}\n"

let gen_code (start, (g, rec_var_info)) ~protocol =
  let rec_var_info = rm_silent_var rec_var_info in
  let var_map = compute_var_map start g rec_var_info in
  let protocol_name = upper_camel_case @@ ProtocolName.user protocol in
  let buffer = Buffer.create 4096 in
  generate_state_enum buffer var_map g protocol_name ;
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
  generate_state_enum buffer var_map g protocol_name ;
  Buffer.add_string buffer "\n" ;
  generate_monitor_struct buffer protocol_name ;
  Buffer.add_string buffer "\n" ;
  generate_impl buffer start g protocol_name var_map rec_var_info ;
  Buffer.contents buffer
