open Pattern
module Rules = Map.Make (String)

type grammar = { start : string; rules : t Rules.t }

let grammar ~start rules = { start; rules }

let rec compile pattern =
  match pattern with
  | Empty -> []
  | NonTerminal _ -> []
  | OrderedChoice (e1, e2) ->
      let p1 = compile e1 in
      let p2 = compile e2 in
      p1 @ p2
  | Sequence (e1, e2) ->
      let p1 = compile e1 in
      let p2 = compile e2 in
      let p1' = Machine.Choice (List.length p1 + 2) :: p1 in
      let p2' = Machine.Commit (List.length p2 + 1) :: p2 in
      p1' @ p2'
  | Repetition e ->
      let p = compile e in
      (Machine.Choice (List.length p + 2) :: p)
      @ [ Machine.Commit (-1 * (List.length p + 1)) ]
  | Not e ->
      let p = compile e in
      (Machine.Choice (List.length p + 3) :: p)
      @ [ Machine.Commit 1; Machine.Fail ]
