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
  generate_big_derive buffer;
  Buffer.add_string buffer 
  "pub struct Action {\n\
  \    dir: Direction,\n\
  \    label: Label,\n\
   }\n"

let generate_monitor buffer protocol_name = 
  generate_small_derive buffer;
  Buffer.add_string buffer 
  ("pub struct " ^ protocol_name ^ "Monitor {\n\
  \    state: State,\n\
  }\n")

let generate_transitions buffer start g protocol_name =
  Buffer.add_string buffer 
  ("impl " ^ protocol_name ^ "Monitor {\n\
  \    pub fn new() -> Self {\n\
  \        Self {\n\
  \            state: State::S" ^ (Int.to_string start) ^ "\n\
  \        }\n\
  \    }\n\
  \n\
  \    pub fn step(&mut self, action: &Action) -> bool {\n\
  \        match (self.state, action.dir, action.label) {\n\
  ");
  G.iter_edges_e (fun (src, a, dst) ->
    match a with
    | SendA (_, m, _) ->
      Buffer.add_string buffer
        (Printf.sprintf
          "            (State::S%s, Direction::Send, Label::%s) \
            => { self.state = State::S%s; true }\n"
          (int_to_name src)
          (upper_camel_case (LabelName.user m.label))
          (int_to_name dst))
    | RecvA (_, m, _) ->
      Buffer.add_string buffer
        (Printf.sprintf
          "            (State::S%s, Direction::Recv, Label::%s) \
          => { self.state = State::S%s; true }\n"
          (int_to_name src)
          (upper_camel_case (LabelName.user m.label))
          (int_to_name dst))
    | Epsilon -> ()
  ) g ;
  Buffer.add_string buffer
  "            _ => false
  \        }\n\
  \    }\n\
  }\n"

(* let gen_code (start, (g, rec_var_info)) ~protocol =  *)
let gen_code (start, (g,_)) ~protocol =
  let buffer = Buffer.create 4096 in
  let protocol_name = upper_camel_case @@ ProtocolName.user protocol in
  generate_states buffer g;
  Buffer.add_string buffer "\n";
  generate_labels buffer g;
  Buffer.add_string buffer "\n";
  generate_support_types buffer;
  Buffer.add_string buffer "\n";
  generate_monitor buffer protocol_name;
  Buffer.add_string buffer "\n";
  generate_transitions buffer start g protocol_name;
  Buffer.contents buffer