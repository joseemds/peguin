open Pog

let test input pat expected =
  let pos = Machine.run pat input in
  Printf.printf "pos %d\n" pos;
  assert (pos = expected)

let () = test "c" (Pattern.Char 'c') 1
let () = test "du" (Pattern.sequence (Pattern.Char 'd') (Pattern.Char 'u')) 2
