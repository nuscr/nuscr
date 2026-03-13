nuscr on PartialUpdate: two rec vars but continue only provides one update expression
(the error is an unclear internal exception)
  $ nuscr --gencode-rust=C@PartialUpdate PartialUpdate.nuscr
  nuscr: Reported problem:
          ("[Map.add_exn] got key already present" (key loop))
  [124]
  $ nuscr --gencode-rust=S@PartialUpdate PartialUpdate.nuscr
  nuscr: Reported problem:
          ("[Map.add_exn] got key already present" (key loop))
  [124]
