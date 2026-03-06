open Peguin

let n = ref 0

let test input pat expected =
  (* let instrs = Pattern.compile  pat in *)
  (* Format.pp_print_list ~pp_sep:Format.pp_print_space Op.pp Format.std_formatter instrs; *)
  incr n;
  match Machine.run pat input with
  | Ok pos -> assert (pos = expected)
  | Error pos ->
      Printf.printf "Failed at pos %d" pos;
      assert false

let () =
  let open Pattern in
  test "salve" (p "oi" / p "testando" / p "sal" % p "ve") 5;
  test "a" ((not (char 'b')) % p "a") 1
