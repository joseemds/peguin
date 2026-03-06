open Pog


let n = ref 0

let test input pat expected =
  (* let instrs = Pattern.compile  pat in *)
  (* Format.pp_print_list ~pp_sep:Format.pp_print_space Op.pp Format.std_formatter instrs; *)
  incr n;
  let pos = Machine.run pat input in
  Printf.printf "Test %d, pos %d\n" !n pos;
  assert (pos = expected)

let () = 
   let open Pattern in
   test "salve" (p"oi" / p"testando" / p"sal" % p"ve") 5
