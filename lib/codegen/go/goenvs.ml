open! Base
open Goimpl
open Names
open! Gonames
open Ltype

module RolePair = struct
  module T = struct
    type t = ProtocolName.t * RoleName.t * RoleName.t
    [@@deriving sexp_of, ord]
  end

  include T
  include Comparable.Make (T)
end

module ProtoLblPair = struct
  module T = struct
    type t = ProtocolName.t * LabelName.t [@@deriving sexp_of, ord]
  end

  include T
  include Comparable.Make (T)
end

module Namegen = Namegen.Make (VariableName)

module ProtocolCall = struct
  module T = struct
    (* Who, Protocol, roles, new roles *)
    type t = RoleName.t * ProtocolName.t [@@deriving sexp_of, ord]
  end

  include T
  include Comparable.Make (T)
end

module GoGenM = struct
  type ctx =
    { channels: (VariableName.t * goType) Map.M(RolePair).t
    ; new_channels: (VariableName.t * goType) Map.M(RolePair).t
    ; ctx_vars: VariableName.t Map.M(LocalProtocolId).t
    ; (* Stack of variables that contain the channels *)
      in_call: (VariableName.t * LocalProtocolId.t * bool) option
    ; call_chans: (VariableName.t * bool) Map.M(LocalProtocolId).t
          (* var with chans in proto_call, is_struct? *)
    ; curr_fn: LocalProtocolId.t option }

  (* TODO: refactor this state, adding layers of "global", "local to function
     definition", etc. E.g. tail_rec should be separate from role_args *)
  type state =
    { namegen: Namegen.t
    ; tail_rec: bool
    ; role_args: (RoleName.t list * RoleName.t list) Map.M(ProtocolName).t
    ; lp_fns: FunctionName.t Map.M(LocalProtocolId).t
    ; msg_iface: VariableName.t Map.M(ProtocolName).t
    ; req_chans: (RoleName.t * RoleName.t) list Map.M(LocalProtocolId).t
    ; ctx_type: VariableName.t Map.M(LocalProtocolId).t
    ; proto_lbls: goTyDecl Map.M(ProtoLblPair).t
    ; call_lbls: LabelName.t Map.M(ProtocolCall).t
    ; choice_iface: LabelName.t list Map.M(VariableName).t
    ; invite_lbls: LabelName.t Map.M(ProtocolCall).t
    ; callbacks: (FunctionName.t * goType) list Map.M(LocalProtocolId).t
    ; lp_ctx: ctx }
  (* type 'a t = state -> state * 'a *)

  let init_lp_ctx =
    { channels= Map.empty (module RolePair)
    ; new_channels= Map.empty (module RolePair)
    ; ctx_vars= Map.empty (module LocalProtocolId)
    ; in_call= None
    ; call_chans= Map.empty (module LocalProtocolId)
    ; curr_fn= None }

  let init =
    { namegen= Namegen.create ()
    ; tail_rec= false
    ; role_args= Map.empty (module ProtocolName)
    ; lp_fns= Map.empty (module LocalProtocolId)
    ; msg_iface= Map.empty (module ProtocolName)
    ; req_chans= Map.empty (module LocalProtocolId)
    ; ctx_type= Map.empty (module LocalProtocolId)
    ; proto_lbls= Map.empty (module ProtoLblPair)
    ; call_lbls= Map.empty (module ProtocolCall)
    ; invite_lbls= Map.empty (module ProtocolCall)
    ; choice_iface= Map.empty (module VariableName)
    ; callbacks= Map.empty (module LocalProtocolId)
    ; lp_ctx= init_lp_ctx }

  let get (st : state) = (st, st)

  let put st _ = (st, ())

  let map f m st =
    let st', x = m st in
    (st', f x)

  let bind m k (st : state) =
    let st', x = m st in
    k x st'

  (* let product m1 m2 st = let (st', x) = m1 st in let (st'', y) = m2 st' in
     (st'', (x, y)) *)

  (* let run m st = m st *)
  let eval m (st : state) = snd (m st)

  (* TODO: clean up local state for current definition *)
  let cleanup st = ({st with lp_ctx= init_lp_ctx; tail_rec= false}, ())

  module Syntax = struct
    (* let ( let+ ) x f = map f x *)
    (* let ( and+ ) m1 m2 = product m1 m2 *)
    let ( let* ) x f = bind x f

    let pure x = function st -> (st, x)
  end
end

let fresh nm =
  let nm = VariableName.of_string nm in
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ng, nm' = Namegen.unique_name st.GoGenM.namegen nm in
  let* _ = GoGenM.put {st with GoGenM.namegen= ng} in
  pure nm'

let find_lp_name ~id =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  pure (Map.find st.GoGenM.lp_fns id)

let put_lp_name ~key ~data =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  GoGenM.put {st with GoGenM.lp_fns= Map.add_exn st.GoGenM.lp_fns ~key ~data}

let put_tail_rec st = ({st with GoGenM.tail_rec= true}, ())
