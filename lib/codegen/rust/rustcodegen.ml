open! Base
open Names
open Efsm
open Message

(* Helpers *)
let upper_camel_case s:string =
  Stdlib.String.capitalize_ascii @@ Stdlib.String.lowercase_ascii s

let int_to_name i = upper_camel_case @@ Int.to_string i

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

let generate_states buffer g =
  generate_big_derive buffer;
  Buffer.add_string buffer "enum State {\n";
  G.iter_vertex (fun label ->
    Buffer.add_string buffer
    ("\    S" ^ (Int.to_string label) ^ ",\n")
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

let generate_monitor buffer protocol_name start_rec_vars =
  generate_small_derive buffer;
  Buffer.add_string buffer
    ("pub struct " ^ protocol_name ^ "Monitor {\n\
    \    state: State,\n");
  List.iter start_rec_vars ~f:(fun (rv : Gtype.rec_var) ->
    Rustexpr.validate_rust_ident rv.rv_name;
    Buffer.add_string buffer
      (Printf.sprintf "    %s: %s,\n"
         (VariableName.user rv.rv_name)
         (Rustexpr.rust_type_of_payload_type rv.rv_ty)));
  Buffer.add_string buffer "}\n"


let generate_transitions buffer start g protocol_name start_rec_vars =
  Buffer.add_string buffer
  ("impl " ^ protocol_name ^ "Monitor {\n\
  \    pub fn new() -> Self {\n\
  \        Self {\n\
  \            state: State::S" ^ (Int.to_string start) ^ ",\n");
  List.iter start_rec_vars ~f:(fun (rv : Gtype.rec_var) ->
    Buffer.add_string buffer
      (Printf.sprintf "            %s: %s,\n"
         (VariableName.user rv.rv_name)
         (Rustexpr.rust_show_expr rv.rv_init_expr)));
  Buffer.add_string buffer
  ("        }\n\
  \    }\n\
  \n\
  \    pub fn step(&mut self, action: &Action) -> bool {\n\
  \        match (self.state, action.dir, action.label) {\n\
  ");
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
  "            _ => false
  \        }\n\
  \    }\n\
  }\n"


let gen_code (start, (g, rec_var_info)) ~protocol =
  Map.iter rec_var_info ~f:(fun data ->
    List.iter data ~f:(fun (is_silent, (rv : Gtype.rec_var)) ->
      if is_silent then
        Err.unimpl ~here:[%here]
          (Printf.sprintf "Rust codegen for silent recursion variable '%s'"
             (VariableName.user rv.rv_name))));
  let start_rec_vars =
    List.map ~f:snd
      (Option.value ~default:[] (Map.find rec_var_info start))
  in
  let buffer = Buffer.create 4096 in
  let protocol_name = upper_camel_case @@ ProtocolName.user protocol in
  generate_states buffer g;
  Buffer.add_string buffer "\n";
  generate_labels buffer g;
  Buffer.add_string buffer "\n";
  generate_support_types buffer;
  Buffer.add_string buffer "\n";
  generate_monitor buffer protocol_name start_rec_vars;
  Buffer.add_string buffer "\n";
  generate_transitions buffer start g protocol_name start_rec_vars;
  Buffer.contents buffer