open Pog

let test input pat expected =
  let (_pc, pos, _e) as _instrs = Machine.run pat input in
  assert (pos = expected)

let ()  = test "c" (Pattern.Char 'c') 1
