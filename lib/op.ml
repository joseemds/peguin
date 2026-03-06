type t =
  | Empty
  | Any
  | Fail
  | FailTwice
  | PartialCommit of int
  | BackCommit of int
  | Char of char
  | Choice of int
  | Jump of int
  | Call of int
  | Commit of int
  | Return

let pp ppf op =
  let pf = Format.fprintf in
  match op with
  | Empty -> pf ppf "Empty"
  | Any -> pf ppf "Any"
  | Fail -> pf ppf "Fail"
  | FailTwice -> pf ppf "FailTwice"
  | Char c -> pf ppf "Char('%c')" c
  | Choice lbl -> pf ppf "Choice(%d)" lbl
  | Jump lbl -> pf ppf "Jump(%d)" lbl
  | Call lbl -> pf ppf "Call(%d)" lbl
  | Commit lbl -> pf ppf "Commit(%d)" lbl
  | PartialCommit lbl -> pf ppf "PartialCommit(%d)" lbl
  | BackCommit l -> pf ppf "Commit(%d)" l
  | Return -> pf ppf "Return"
