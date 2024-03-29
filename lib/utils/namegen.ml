open! Base
open Printf

module Make (N : Names.TaggedName) = struct
  type t = int Map.M(N).t

  let create () = Map.empty (module N)

  let make_unique_name name uid =
    if uid < 2 then name
    else N.rename name (sprintf "%s_%d" (N.user name) uid)

  let rec unique_name uids name =
    match Map.find uids name with
    | None -> (Map.add_exn uids ~key:name ~data:1, name)
    | Some curr_id -> (
        let uid = curr_id + 1 in
        let new_name = make_unique_name name uid in
        match Map.find uids new_name with
        | None ->
            let uids = Map.add_exn uids ~key:new_name ~data:1 in
            (Map.update uids name ~f:(fun _ -> uid), new_name)
        | Some _ -> unique_name (Map.update uids name ~f:(fun _ -> uid)) name
        )

  let curr_unique_name uids name =
    match Map.find uids name with
    | None -> None
    | Some uid -> Some (make_unique_name name uid)
end
