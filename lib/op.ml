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
