open Peguin

let test_ok input pat expected =
  match Machine.run pat input with
  | Ok pos -> assert (pos = expected)
  | Error pos ->
      failwith ("Expected pattern to succeed, fails on pos " ^ string_of_int pos)

let test_fail input pat expected =
  match Machine.run pat input with
  | Ok pos ->
      failwith ("Expected pattern to fail, succeeds on pos " ^ string_of_int pos)
  | Error pos -> assert (pos = expected)

(* ------------------------------------------------------------------ *)
(* Empty                                                                *)
(* ------------------------------------------------------------------ *)

let test_empty () =
  test_ok "" Pattern.empty 0;
  test_ok "abc" Pattern.empty 0

(* ------------------------------------------------------------------ *)
(* Char                                                                 *)
(* ------------------------------------------------------------------ *)

let test_char () =
  test_ok "a" (Pattern.char 'a') 1;
  test_ok "z" (Pattern.char 'z') 1;
  test_fail "b" (Pattern.char 'a') 0

(* ------------------------------------------------------------------ *)
(* Sequence                                                             *)
(* ------------------------------------------------------------------ *)

let test_sequence () =
  test_ok "ab" (Pattern.sequence (Pattern.char 'a') (Pattern.char 'b')) 2;
  test_ok "abc" (Pattern.p "abc") 3;
  test_ok "abcd" (Pattern.sequence (Pattern.p "ab") (Pattern.p "cd")) 4

(* ------------------------------------------------------------------ *)
(* OrderedChoice                                                        *)
(* ------------------------------------------------------------------ *)

let test_ordered_choice () =
  test_ok "cat" (Pattern.choice (Pattern.p "cat") (Pattern.p "dog")) 3;
  test_ok "dog" (Pattern.choice (Pattern.p "cat") (Pattern.p "dog")) 3;
  test_ok "a" (Pattern.choice (Pattern.char 'a') (Pattern.char 'b')) 1;
  test_ok "b" (Pattern.choice (Pattern.char 'a') (Pattern.char 'b')) 1;
  test_ok "a"
    (Pattern.choice
       (Pattern.choice (Pattern.char 'a') (Pattern.char 'b'))
       (Pattern.char 'c'))
    1;
  test_ok "b"
    (Pattern.choice
       (Pattern.choice (Pattern.char 'a') (Pattern.char 'b'))
       (Pattern.char 'c'))
    1

(* ------------------------------------------------------------------ *)
(* Repetition — repeat0 (zero or more)                                  *)
(* ------------------------------------------------------------------ *)

let test_repeat0 () =
  test_ok "" (Pattern.repeat0 (Pattern.char 'a')) 0;
  test_ok "a" (Pattern.repeat0 (Pattern.char 'a')) 1;
  test_ok "aaa" (Pattern.repeat0 (Pattern.char 'a')) 3;
  test_ok "aaab" (Pattern.repeat0 (Pattern.char 'a')) 3;
  test_ok "ababab" (Pattern.repeat0 (Pattern.p "ab")) 6;
  test_ok "abba"
    (Pattern.repeat0 (Pattern.choice (Pattern.char 'a') (Pattern.char 'b')))
    4

(* ------------------------------------------------------------------ *)
(* repeat1 — one or more                                                *)
(* ------------------------------------------------------------------ *)

let test_repeat1 () =
  test_ok "a" (Pattern.repeat1 (Pattern.char 'a')) 1;
  test_ok "aaa" (Pattern.repeat1 (Pattern.char 'a')) 3;
  test_ok "aaab" (Pattern.repeat1 (Pattern.char 'a')) 3

(* ------------------------------------------------------------------ *)
(* Not — negative lookahead                                             *)
(* ------------------------------------------------------------------ *)

let test_not () =
  (* not 'b' succeeds on 'a', consuming nothing *)
  test_ok "a" (Pattern.not (Pattern.char 'b')) 0;
  (* not followed by char — lookahead then consume *)
  test_ok "a"
    (Pattern.sequence (Pattern.not (Pattern.char 'b')) (Pattern.char 'a'))
    1

(*p <- !'ab' 'b' *)
(* test_ok  "abb" (Pattern.sequence (Pattern.p "ab") (Pattern.not (Pattern.char 'b'))) *)
(* 2 *)

(* ------------------------------------------------------------------ *)
(* pos — positive lookahead (not (not e))                               *)
(* ------------------------------------------------------------------ *)

let test_pos () =
  test_ok "a" (Pattern.pos (Pattern.char 'a')) 0;
  test_ok "a"
    (Pattern.sequence (Pattern.pos (Pattern.char 'a')) (Pattern.char 'a'))
    1;
  test_ok "ab" (Pattern.pos (Pattern.p "ab")) 0;
  test_fail "bb" (Pattern.and_ (Pattern.p "aa")) 0

(* ------------------------------------------------------------------ *)
(* Composed patterns                                                    *)
(* ------------------------------------------------------------------ *)

let digit =
  Pattern.choice
    (Pattern.choice
       (Pattern.choice
          (Pattern.choice
             (Pattern.choice
                (Pattern.choice
                   (Pattern.choice
                      (Pattern.choice
                         (Pattern.choice (Pattern.char '0') (Pattern.char '1'))
                         (Pattern.char '2'))
                      (Pattern.char '3'))
                   (Pattern.char '4'))
                (Pattern.char '5'))
             (Pattern.char '6'))
          (Pattern.char '7'))
       (Pattern.char '8'))
    (Pattern.char '9')

let space = Pattern.repeat0 (Pattern.char ' ')
let bool_lit = Pattern.choice (Pattern.p "true") (Pattern.p "false")

let test_composed () =
  test_ok "123abc" (Pattern.repeat1 digit) 3;
  test_ok "0" (Pattern.repeat1 digit) 1;
  test_ok "   abc" (Pattern.sequence space (Pattern.p "abc")) 6;
  test_ok "true" bool_lit 4;
  test_ok "false" bool_lit 5

let test_not_keyword () =
  (* "ifoo" starts with "if" so not (p "if") fails *)
  test_fail "if"
    (Pattern.sequence
       (Pattern.not (Pattern.p "if"))
       (Pattern.repeat1 (Pattern.choice (Pattern.char 'i') (Pattern.char 'f'))))
    2;
  (* "ig" does not start with "if" so not succeeds *)
  test_ok "ig"
    (Pattern.sequence
       (Pattern.not (Pattern.p "if"))
       (Pattern.repeat1 (Pattern.choice (Pattern.char 'i') (Pattern.char 'g'))))
    2;
  test_ok "a" (Pattern.not (Pattern.char 'b')) 0

(* ------------------------------------------------------------------ *)
(* Suite                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "peguin.machine"
    [
      ("empty", [ Alcotest.test_case "empty pattern" `Quick test_empty ]);
      ("char", [ Alcotest.test_case "single char" `Quick test_char ]);
      ("sequence", [ Alcotest.test_case "sequence" `Quick test_sequence ]);
      ( "ordered_choice",
        [ Alcotest.test_case "ordered choice" `Quick test_ordered_choice ] );
      ("repeat0", [ Alcotest.test_case "zero or more" `Quick test_repeat0 ]);
      ("repeat1", [ Alcotest.test_case "one or more" `Quick test_repeat1 ]);
      ("not", [ Alcotest.test_case "negative lookahead" `Quick test_not ]);
      ("pos", [ Alcotest.test_case "positive lookahead" `Quick test_pos ]);
      ( "composed",
        [ Alcotest.test_case "composed patterns" `Quick test_composed ] );
      ( "not_keyword",
        [ Alcotest.test_case "keyword lookahead" `Quick test_not_keyword ] );
    ]
