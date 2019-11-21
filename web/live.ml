open Js_of_ocaml
open Nuscrlib
(* nuScribble live *)

let textarea_id = "protocol-textarea"
let get s = try
    Dom_html.getElementById s
  with Not_found ->
    failwith @@ Printf.sprintf "Element with id `%s' not found." s

let display_error fmt =
  let display string =
    let div = get "errorbox" in
    div ##. innerHTML := Js.string string;
    div ##. style ##. display := Js.string "block";
  in
  Printf.ksprintf display fmt
let display_exn = function
  | Err.UserError e -> display_error "<b>User error</b>: %s" (Err.show_user_error e)
  | _ -> ()
let reset_error () =
  (get "errorbox") ##. style ##. display := Js.string "None"
let get_protocol () =
  Js.(to_string
        (Unsafe.coerce (get textarea_id)
         :> Dom_html.textAreaElement Js.t) ##. value)

let analyse () =
  let () = reset_error () in
  let protocol = get_protocol () in
  begin match Lib.parse_string protocol with
  | exception e ->
     display_exn e
  | ast -> 
     begin match Lib.validate_exn ast false with
     | exception e -> display_exn e
     | () ->
        let roles = Lib.enumerate ast in
        let g = get "roles" in
        g ##. innerHTML := Js.string @@ Printf.sprintf
                                              "Roles of the protocol: %s"
                                              (String.concat ", "
                                              @@ List.map (fun (a, b) -> Printf.sprintf "%s:%s" a b) roles)
     end
  end
let init _ =
  let button =
    (Js.Unsafe.coerce (Dom_html.getElementById "button") :> Dom_html.inputElement Js.t)
  in
  button ##. onclick := Dom_html.handler (fun _ -> analyse (); Js._false);
  Js._false
(* This calls our init function when
   the page is loaded. *)
let () = 
  Dom_html.window##.onload := Dom_html.handler init
    
