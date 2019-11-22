(* nuScribble live *)
open Js_of_ocaml
open Nuscrlib
module Html = Dom_html
module T = Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_tyxml.Tyxml_js
module W = Webutils

let project scr (name, role) =
  let ltyp = Lib.project_role scr name role in
  let s = Ltype.show_local_type ltyp in
  (W.get "projected")##.innerHTML
  := Js.string @@ Printf.sprintf "Projected on to %s:%s:: %s" name role s

let display_role scr (name, protocol) =
  let lk =
    Of_dom.of_anchor
      (W.make_link (fun () -> project scr (name, protocol)) "Project")
  in
  T.(
    li [txt (Printf.sprintf "%s: %s" name protocol); txt " [ "; lk; txt " ] "])

let display_roles scr l =
  To_dom.of_element @@ T.(ul (List.map (display_role scr) l))

let analyse () =
  let () = Interface.Error.reset () in
  let protocol = Interface.Code.get () in
  match Lib.parse_string protocol with
  | exception e -> Interface.Error.display_exn e
  | ast -> (
    match Lib.validate_exn ast false with
    | exception e -> Interface.Error.display_exn e
    | () ->
        let roles_html = display_roles ast @@ Lib.enumerate ast in
        W.(set_children (get "roles") [(roles_html :> Dom.node Js.t)]) )

let init _ =
  let button =
    ( Js.Unsafe.coerce (Dom_html.getElementById "button")
      :> Dom_html.inputElement Js.t )
  in
  button##.onclick := Dom_html.handler (fun _ -> analyse () ; Js._false) ;
  W.make_combobox "examples"
    (List.map
       (fun (name, value) -> (name, fun () -> Interface.Code.set value))
       Examples.list) ;
  Js._false

(* This calls our init function when the page is loaded. *)
let () = Dom_html.window##.onload := Dom_html.handler init
