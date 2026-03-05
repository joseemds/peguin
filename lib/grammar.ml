module Rules = Map.Make (String)

type grammar = { start : string; rules : Pattern.t Rules.t }

let grammar ~start rules = { start; rules }

