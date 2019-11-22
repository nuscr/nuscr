(* Utilities to manipulate web page *)
open Js_of_ocaml

(* Returns the element with the given id *)
let get id =
  try Dom_html.getElementById id
  with Not_found ->
    failwith @@ Printf.sprintf "Element with id `%s' not found." id

let set_inner_html x fmt =
  Printf.ksprintf (fun s -> x##.innerHTML := Js.string s) fmt

let set_display x value = x##.style##.display := Js.string value

let get_textarea id =
  Js.((Unsafe.coerce (get id) :> Dom_html.textAreaElement Js.t))

let make_link on_click string =
  let a = Dom_html.createA Dom_html.document in
  a##.onclick := Dom_html.handler (fun _ -> on_click () ; Js._true) ;
  a##.innerHTML := Js.string string ;
  a

let make_combobox id list =
  let select =
    Js.((Unsafe.coerce (get id) :> Dom_html.selectElement Js.t))
  in
  let add_option (label, _f) =
    let option = Dom_html.createOption Dom_html.document in
    option##.innerHTML := Js.string label ;
    ignore @@ select##appendChild (option :> Dom.node Js.t)
  in
  let onclick _ =
    List.assoc (Js.to_string select##.value) list () ;
    Js._false
  in
  List.iter add_option list ;
  select##.onchange := Dom_html.handler onclick

let set_children (elem : #Dom.node Js.t) (children : #Dom.node Js.t list) =
  (* Remove all kids *)
  let old_kids = elem##.childNodes in
  for i = 0 to old_kids##.length do
    match Js.Opt.to_option (old_kids##item i) with
    | Some kid -> Dom.removeChild elem kid
    | None -> ()
  done ;
  List.iter (fun kid -> ignore @@ elem##appendChild kid) children
