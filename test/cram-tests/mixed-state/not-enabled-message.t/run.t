In the classic theory, the protocol is valid for C, where its projection contains a sending message to B.

  $ nuscr --project C@NotEnabled NotEnabledClassic.nuscr
  Nihao() to B;
  end
  

But in the revised theory, the protocol is not well-formed, since C is not enabled to send message.

  $ nuscr --project C@NotEnabled NotEnabled.nuscr
  nuscr: I'm sorry, it is unfortunate Error message for uninformed choice for role C is not implemented (raised at lib/mpst/gtype.ml: line 615)
  [124]
