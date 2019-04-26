
type 'a located =
  { loc: Lexing.position * Lexing.position ; value: 'a }

type ast =
  raw_ast located

and raw_ast =
  | Con of string
