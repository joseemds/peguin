type t =
  | Empty
  (* | NonTerminal of string *)
  | OrderedChoice of t * t
  | Sequence of t * t
  | Repetition of t
  | Char of char
  | Not of t

let empty = Empty
let char c = Char c
(* let nonterminal nt = NonTerminal nt *)
let choice e1 e2 = OrderedChoice (e1, e2)
let not e = Not e
let pos e = Not (Not e)
let sequence e1 e2 = Sequence (e1, e2)
let repeat0 e = Repetition e
let repeat1 e = sequence e @@ repeat0 e

let rec compile pattern =
  match pattern with
  | Empty -> []
  | Char c -> [Op.Char c]
  | OrderedChoice (e1, e2) ->
      let p1 = compile e1 in
      let p2 = compile e2 in
      p1 @ p2
  | Sequence (e1, e2) ->
      let p1 = compile e1 in
      let p2 = compile e2 in
      let p1' = Op.Choice (List.length p1 + 2) :: p1 in
      let p2' = Op.Commit (List.length p2 + 1) :: p2 in
      p1' @ p2'
  | Repetition e ->
      let p = compile e in
      (Op.Choice (List.length p + 2) :: p)
      @ [ Op.Commit (-1 * (List.length p + 1)) ]
  | Not e ->
      let p = compile e in
      (Op.Choice (List.length p + 3) :: p)
      @ [ Op.Commit 1; Op.Fail ]
