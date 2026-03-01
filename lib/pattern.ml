type t =
  | Empty
  | NonTerminal of string
  | OrderedChoice of t * t
  | Sequence of t * t
  | Repetition of t
  | Not of t

let empty = Empty
let nonterminal nt = NonTerminal nt
let choice e1 e2 = OrderedChoice (e1, e2)
let not e = Not e
let pos e = Not (Not e)
let sequence e1 e2 = Sequence (e1, e2)
let repeat0 e = Repetition e
let repeat1 e = sequence e @@ repeat0 e
