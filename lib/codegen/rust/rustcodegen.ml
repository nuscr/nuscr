open! Base
open Names
open Efsm
open Message

(* Helpers *)
let upper_camel_case s:string = 
  Stdlib.String.capitalize_ascii @@ Stdlib.String.lowercase_ascii s

let collect_labels g =
  let f (_, a, _) acc =
    match a with
    | SendA (_, m, _) | RecvA (_, m, _) ->
        Set.add acc (LabelName.user m.label)
    | Epsilon -> acc
  in
  G.fold_edges_e f g (Set.empty (module String))

(* Generators *)
let generate_derive buffer = 
  Buffer.add_string buffer "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n"

let generate_labels buffer g = 
  let labels = collect_labels g in
  generate_derive buffer;
  Buffer.add_string buffer
  "enum Label {\n";
  Set.iter labels ~f:(fun label ->
    Buffer.add_string buffer
    ("\    " ^ (upper_camel_case label) ^ ",\n")
  );
  Buffer.add_string buffer "}\n"

let generate_support_types buffer = 
  generate_derive buffer;
  Buffer.add_string buffer 
  "enum Direction {\n\
  \    Send,\n\
  \    Recv,\n\
   }\n\
  \n";
  generate_derive buffer;
  Buffer.add_string buffer 
  "struct Action {\n\
  \    dir: Direction,\n\
  \    label: Label,\n\
   }\n"

let generate_monitor buffer protocol = 
  generate_derive buffer;
  Buffer.add_string buffer ("pub struct " ^ protocol ^ "Monitor; \n")

(* let gen_code (start, (g, rec_var_info)) ~protocol =  *)
let gen_code (_, (g,_)) ~protocol =
  let buffer = Buffer.create 4096 in
  let protocol_name = upper_camel_case @@ ProtocolName.user protocol in
  generate_labels buffer g;
  Buffer.add_string buffer "\n";
  generate_support_types buffer;
  Buffer.add_string buffer "\n";
  generate_monitor buffer protocol_name;
  Buffer.contents buffer