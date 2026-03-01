  type t = 
    Empty
  | Terminal of string
  | NonTerminal of string
  | OrderedChoice of t * t
  | Sequence of t * t
  | Not of t
  | Pos of t  

let empty = Empty
let choice e1 e2 = OrderedChoice (e1, e2)
let not e = Not e
let pos e = Pos e
let sequence e1 e2 = Sequence (e1, e2)

