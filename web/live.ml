(* nuScr live *)
open Js_of_ocaml
open Nuscrlib
module Html = Dom_html
module T = Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_tyxml.Tyxml_js
module W = Webutils

let show_protocol_role protocol role =
  let protocol = Name.user protocol in
  let role = Name.user role in
  Printf.sprintf "%s@%s" role protocol

let project scr (name, role) =
  let ltyp = Lib.project_role scr ~protocol:name ~role in
  let s = Ltype.show ltyp in
  (W.get "projected")##.innerHTML
  := Js.string
     @@ Printf.sprintf "Projected on to %s :\n%s"
          (show_protocol_role name role)
          s

let fsm scr (name, role) =
  let _, fsm = Lib.generate_fsm scr ~protocol:name ~role in
  let dot = Efsm.show fsm in
  Interface.Graph.set_dot dot

let display_role scr (name, protocol) =
  let lk_p =
    Of_dom.of_anchor
      (W.make_link (fun () -> project scr (name, protocol)) "Project")
  in
  let lk_f =
    Of_dom.of_anchor (W.make_link (fun () -> fsm scr (name, protocol)) "FSM")
  in
  T.(
    li
      [ txt (show_protocol_role protocol name)
      ; txt " [ "
      ; lk_p
      ; txt " ] "
      ; txt " [ "
      ; lk_f
      ; txt " ] " ])

let display_roles scr l =
  To_dom.of_element @@ T.(ul (List.map (display_role scr) l))

let analyse () =
  let () = Interface.Error.reset () in
  let protocol = Interface.Code.get () in
  match Lib.parse_string protocol with
  | exception e -> Interface.Error.display_exn e
  | ast -> (
    match Lib.validate_exn ast ~verbose:false with
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
