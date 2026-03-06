type t =
  | Empty
  | Any
  | Fail
  | Char of char
  | Choice of int
  | Jump of int
  | Call of int
  | Commit of int
  | Return


let pp ppf op = 
  let pf = Format.fprintf in
  match op with
  | Empty  -> pf ppf "Empty"
  | Any -> pf ppf "Any"
  | Fail -> pf ppf "Fail"
  | Char c -> pf ppf "Char %c" c
  | Choice lbl -> pf ppf "Choice(%d)" lbl
  | Jump lbl -> pf ppf "Jump(%d)" lbl
  | Call lbl -> pf ppf "Call(%d)" lbl
  | Commit lbl -> pf ppf "Commit(%d)" lbl
  | Return -> pf ppf "Return"
