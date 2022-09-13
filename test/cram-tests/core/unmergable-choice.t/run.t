Should report an issue because of unmergeable local type when projecting on C
  $ nuscr --project C@Bad Bad.nuscr
  nuscr: User error: Unable to merge:
         Ltype.EndI
         and
         (Ltype.RecvI (m3(), { Loc.loc = 7:15 to 7:16 in: Bad.nuscr; value = "A" },
            Ltype.EndI))
  [124]
