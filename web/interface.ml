open Js_of_ocaml
open Nuscrlib

module Code = struct
  let id = "protocol-textarea"

  let get () = Js.(to_string (Webutils.get_textarea id)##.value)

  let set s = (Webutils.get_textarea id)##.value := Js.string s
end

module Error = struct
  let errorbox = "errorbox"

  let display fmt =
    let display string =
      let e = Webutils.get errorbox in
      Webutils.(
        set_inner_html e "%s" string ;
        set_display e "block")
    in
    Printf.ksprintf display fmt

  let display_exn = function
    | Err.UserError e ->
        display "<b>User error</b>: %s" (Err.show_user_error e)
    | Err.UnImplemented s -> display "<b>Feature not implemented</b>: %s" s
    | Err.Violation s -> display "<b>Violation</b>: %s" s
    | e -> display "<b>Unknown error</b>: %s" (Printexc.to_string e)

  let reset () = Webutils.(set_display (get "errorbox") "none")
end
