(* nuScribble live *)
open Js_of_ocaml
open Nuscrlib
module W = Webutils
module Html = Dom_html
(*open Tyxml.Html*)

let project scr (name, role) =
  let ltyp = Lib.project_role scr name role in
  let s = Ltype.show_local_type ltyp in
  (W.get "projected") ##. innerHTML := Js.string @@ Printf.sprintf "Projected on to %s:%s:: %s" name role s
        

let analyse () =
  let () = Interface.Error.reset () in
  let protocol = Interface.Code.get () in
  begin match Lib.parse_string protocol with
  | exception e ->
     Interface.Error.display_exn e
  | ast -> 
     begin match Lib.validate_exn ast false with
     | exception e -> Interface.Error.display_exn e
     | () ->
        let roles = Lib.enumerate ast in
        W.(set_inner_html (get "roles")
          "Roles of the protocol: %s"
          (String.concat ", "
           @@ List.map (fun (a, b) -> Printf.sprintf "%s:%s" a b) roles))
     end
  end
  
        
  
let init _ =
  let button =
    (Js.Unsafe.coerce (Dom_html.getElementById "button") :> Dom_html.inputElement Js.t)
  in
  button ##. onclick := Dom_html.handler (fun _ -> analyse (); Js._false);
  W.make_combobox "examples"
    (List.map (fun (name, value) ->
         (name, fun () -> Interface.Code.set value))
       Examples.list);
  Js._false

  
(* This calls our init function when
   the page is loaded. *)
let () = 
  Dom_html.window##.onload := Dom_html.handler init
    
