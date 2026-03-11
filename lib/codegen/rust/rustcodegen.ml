open! Base
open Names
open Gtype
open Efsm
open Message

(* Helpers *)
let upper_camel_case s:string =
  Stdlib.String.capitalize_ascii @@ Stdlib.String.lowercase_ascii s

let int_to_name i = upper_camel_case @@ Int.to_string i

let find_payload_vars m =
  List.filter_map m.payload ~f:(function
    | PValue (Some v, ty) -> Some (v, ty)
    | _ -> None)

let append_var lst ((v, _) as entry) =
  if List.exists lst ~f:(fun (v_, _) -> VariableName.equal v v_) then lst
  else lst @ [entry]

(* Walk the EFSM graph from [start], accumulating rec vars and named payload
   vars along each path. Each state is visited at most once: when choice
   branches merge into a single state, session-type merging guarantees
   identical variable scopes on every path, so the first visit suffices. *)
let compute_var_map start g rec_var_info =
  let rec aux acc (curr_st, vars) =
    match Map.find acc curr_st with
    | Some _ -> acc
    | None ->
        let rec_vars =
          Option.value ~default:[] (Map.find rec_var_info curr_st)
          |> List.map ~f:(fun (_, rv) -> (rv.rv_name, rv.rv_ty))
        in
        let vars = List.fold ~f:append_var ~init:vars rec_vars in
        let acc = Map.set acc ~key:curr_st ~data:vars in
        G.fold_succ_e (fun (_, action, next_st) acc ->
          match action with
          | SendA (_, m, _) | RecvA (_, m, _) ->
              let payload_vars = find_payload_vars m in
              let vars = List.fold ~f:append_var ~init:vars payload_vars in
              aux acc (next_st, vars)
          | Epsilon ->
              Err.violation ~here:[%here]
                "Epsilon transitions should not appear in EFSM outputs"
        ) g curr_st acc
  in
  aux (Map.empty (module Int)) (start, [])

let collect_labels g =
  let f (_, a, _) acc =
    match a with
    | SendA (_, m, _) | RecvA (_, m, _) ->
        Set.add acc (LabelName.user m.label)
    | Epsilon -> acc
  in
  G.fold_edges_e f g (Set.empty (module String))

(* Generators *)
let generate_big_derive buffer =
  Buffer.add_string buffer "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n"

let generate_small_derive buffer =
  Buffer.add_string buffer "#[derive(Debug, Clone, PartialEq, Eq)]\n"

let generate_state_enum buffer var_map g =
  generate_small_derive buffer;
  Buffer.add_string buffer "enum State {\n";
  G.iter_vertex (fun state ->
    let vars = Map.find_exn var_map state in
    match vars with
    | [] ->
        Buffer.add_string buffer
          (Printf.sprintf "    S%d,\n" state)
    | _ ->
        let fields =
          List.map vars ~f:(fun (v, ty) ->
            Printf.sprintf "%s: %s"
              (VariableName.user v)
              (Rustexpr.rust_type_of_payload_type ty))
        in
        Buffer.add_string buffer
          (Printf.sprintf "    S%d { %s },\n" state
             (String.concat ~sep:", " fields))
  ) g;
  Buffer.add_string buffer "}\n"

let generate_labels buffer g =
  let labels = collect_labels g in
  generate_big_derive buffer;
  Buffer.add_string buffer "pub enum Label {\n";
  Set.iter labels ~f:(fun label ->
    Buffer.add_string buffer
    ("\    " ^ (upper_camel_case label) ^ ",\n")
  );
  Buffer.add_string buffer "}\n"

let generate_support_types buffer =
  generate_big_derive buffer;
  Buffer.add_string buffer
  "pub enum Direction {\n\
  \    Send,\n\
  \    Recv,\n\
   }\n\
  \n";
  generate_small_derive buffer;
  Buffer.add_string buffer
  "pub enum Value {\n\
  \    Int(i64),\n\
  \    Bool(bool),\n\
  \    String(String),\n\
  \    Unit,\n\
   }\n\
  \n";
  generate_small_derive buffer;
  Buffer.add_string buffer
  "pub struct Action {\n\
  \    dir: Direction,\n\
  \    label: Label,\n\
  \    payloads: Vec<Value>,\n\
   }\n"

let generate_monitor buffer protocol_name =
  generate_small_derive buffer;
  Buffer.add_string buffer
    (Printf.sprintf
       "pub struct %sMonitor {\n\
       \    state: State,\n\
        }\n" protocol_name)


let generate_constructor buffer start var_map rec_var_info =
  let start_vars = Map.find_exn var_map start in
  let start_rv_info =
    Option.value ~default:[] (Map.find rec_var_info start)
  in
  List.iter start_vars ~f:(fun (v, _) ->
    let is_rec_var =
      List.exists start_rv_info
        ~f:(fun (_, rv) -> VariableName.equal v rv.rv_name)
    in
    if not is_rec_var then
      Err.violationf ~here:[%here]
        "Payload variable '%s' at start state has no initial value"
        (VariableName.user v));
  Buffer.add_string buffer "    pub fn new() -> Self {\n";
  (match start_vars with
   | [] ->
       Buffer.add_string buffer
         (Printf.sprintf "        Self { state: State::S%d }\n" start)
   | _ ->
       let inits =
         List.map start_vars ~f:(fun (v, _) ->
           let (_, rv) = List.find_exn start_rv_info
             ~f:(fun (_, rv) -> VariableName.equal v rv.rv_name)
           in
           Printf.sprintf "%s: %s"
             (VariableName.user v)
             (Rustexpr.rust_show_expr rv.rv_init_expr))
       in
       Buffer.add_string buffer
         (Printf.sprintf "        Self { state: State::S%d { %s } }\n"
            start (String.concat ~sep:", " inits)));
  Buffer.add_string buffer "    }\n"

let generate_step_fn buffer g =
  Buffer.add_string buffer
    "\n\
    \    pub fn step(&mut self, action: &Action) -> bool {\n\
    \        match (self.state, action.dir, action.label) {\n";
  G.iter_edges_e (fun (src, a, dst) ->
    match a with
    | SendA (_, m, _) | RecvA (_, m, _) ->
      let dir = match a with
        | SendA _ -> "Send" | RecvA _ -> "Recv" | Epsilon -> assert false
      in
      Buffer.add_string buffer
        (Printf.sprintf
          "            (State::S%s, Direction::%s, Label::%s) => \
           match action.payloads.as_slice() {\n\
           \                %s => { self.state = State::S%s; %s }\n\
           \                _ => false\n\
           \            },\n"
          (int_to_name src)
          dir
          (upper_camel_case (LabelName.user m.label))
          (Rustexpr.payload_slice_pattern m.payload)
          (int_to_name dst)
          (Rustexpr.payload_constraints m.payload))
    | Epsilon -> ()
  ) g ;
  Buffer.add_string buffer
    "            _ => false\n\
    \        }\n\
    \    }\n"

let generate_impl buffer start g protocol_name var_map rec_var_info =
  Buffer.add_string buffer
    (Printf.sprintf "impl %sMonitor {\n" protocol_name);
  generate_constructor buffer start var_map rec_var_info;
  generate_step_fn buffer g;
  Buffer.add_string buffer "}\n"


let gen_code (start, (g, rec_var_info)) ~protocol =
  Map.iter rec_var_info ~f:(fun data ->
    List.iter data ~f:(fun (is_silent, rv) ->
      if is_silent then
        Err.unimpl ~here:[%here]
          (Printf.sprintf "Rust codegen for silent recursion variable '%s'"
             (VariableName.user rv.rv_name))));
  let buffer = Buffer.create 4096 in
  let protocol_name = upper_camel_case @@ ProtocolName.user protocol in
  let var_map = compute_var_map start g rec_var_info in
  generate_state_enum buffer var_map g;
  Buffer.add_string buffer "\n";
  generate_labels buffer g;
  Buffer.add_string buffer "\n";
  generate_support_types buffer;
  Buffer.add_string buffer "\n";
  generate_monitor buffer protocol_name;
  Buffer.add_string buffer "\n";
  generate_impl buffer start g protocol_name var_map rec_var_info;
  Buffer.contents buffer