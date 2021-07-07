include Lib

module Pragma = struct
  include Pragma
end

module Gtype = struct
  include Mpst.Gtype
end

module Ltype = struct
  include Mpst.Ltype
end

module Efsm = struct
  include Mpst.Efsm
end

module Err = struct
  include Err
end

(* Exported for usages in Pedro *)
module Names = struct
  include Names
end
