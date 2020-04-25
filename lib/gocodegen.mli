open Names

val gen_msgs :
     Gtype.global_t
  -> ( ProtocolName.t
     , string Base.List.t
     , ProtocolName.comparator_witness )
     Base.Map.t
