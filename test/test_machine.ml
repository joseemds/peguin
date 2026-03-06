open Peguin

let check_pos input pat expected =
  let pos = Machine.run pat input in
  Alcotest.(check int) (Printf.sprintf "%S -> %d" input expected) expected pos

(* ------------------------------------------------------------------ *)
(* Empty                                                                *)
(* ------------------------------------------------------------------ *)

let test_empty () =
  check_pos ""    Pattern.empty 0;
  check_pos "abc" Pattern.empty 0

(* ------------------------------------------------------------------ *)
(* Char                                                                 *)
(* ------------------------------------------------------------------ *)

let test_char () =
  check_pos "a" (Pattern.char 'a') 1;
  check_pos "z" (Pattern.char 'z') 1

(* ------------------------------------------------------------------ *)
(* Sequence                                                             *)
(* ------------------------------------------------------------------ *)

let test_sequence () =
  check_pos "ab"   (Pattern.sequence (Pattern.char 'a') (Pattern.char 'b')) 2;
  check_pos "abc"  (Pattern.p "abc")                                         3;
  check_pos "abcd" (Pattern.sequence (Pattern.p "ab") (Pattern.p "cd"))      4

(* ------------------------------------------------------------------ *)
(* OrderedChoice                                                        *)
(* ------------------------------------------------------------------ *)

let test_ordered_choice () =
  check_pos "cat" (Pattern.choice (Pattern.p "cat") (Pattern.p "dog")) 3;
  check_pos "dog" (Pattern.choice (Pattern.p "cat") (Pattern.p "dog")) 3;
  check_pos "a"   (Pattern.choice (Pattern.char 'a') (Pattern.char 'b')) 1;
  check_pos "b"   (Pattern.choice (Pattern.char 'a') (Pattern.char 'b')) 1;
  check_pos "a"
    (Pattern.choice
      (Pattern.choice (Pattern.char 'a') (Pattern.char 'b'))
      (Pattern.char 'c'))
    1;
  check_pos "b"
    (Pattern.choice
      (Pattern.choice (Pattern.char 'a') (Pattern.char 'b'))
      (Pattern.char 'c'))
    1

(* ------------------------------------------------------------------ *)
(* Repetition — repeat0 (zero or more)                                  *)
(* ------------------------------------------------------------------ *)

let test_repeat0 () =
  check_pos ""       (Pattern.repeat0 (Pattern.char 'a')) 0;
  check_pos "a"      (Pattern.repeat0 (Pattern.char 'a')) 1;
  check_pos "aaa"    (Pattern.repeat0 (Pattern.char 'a')) 3;
  check_pos "aaab"   (Pattern.repeat0 (Pattern.char 'a')) 3;
  check_pos "ababab" (Pattern.repeat0 (Pattern.p "ab"))   6;
  check_pos "abba"
    (Pattern.repeat0 (Pattern.choice (Pattern.char 'a') (Pattern.char 'b')))
    4

(* ------------------------------------------------------------------ *)
(* repeat1 — one or more                                                *)
(* ------------------------------------------------------------------ *)

let test_repeat1 () =
  check_pos "a"    (Pattern.repeat1 (Pattern.char 'a')) 1;
  check_pos "aaa"  (Pattern.repeat1 (Pattern.char 'a')) 3;
  check_pos "aaab" (Pattern.repeat1 (Pattern.char 'a')) 3

(* ------------------------------------------------------------------ *)
(* Not — negative lookahead                                             *)
(* ------------------------------------------------------------------ *)

let test_not () =
  (* not 'b' succeeds on 'a', consuming nothing *)
  check_pos "a" (Pattern.not (Pattern.char 'b')) 0;
  (* not followed by char — lookahead then consume *)
  check_pos "a"
    (Pattern.sequence (Pattern.not (Pattern.char 'b')) (Pattern.char 'a'))
    1

(* ------------------------------------------------------------------ *)
(* pos — positive lookahead (not (not e))                               *)
(* ------------------------------------------------------------------ *)

let test_pos () =
  check_pos "a"  (Pattern.pos (Pattern.char 'a')) 0;
  check_pos "a"
    (Pattern.sequence (Pattern.pos (Pattern.char 'a')) (Pattern.char 'a'))
    1;
  check_pos "ab" (Pattern.pos (Pattern.p "ab")) 0

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
  check_pos "123abc" (Pattern.repeat1 digit) 3;
  check_pos "0"      (Pattern.repeat1 digit) 1;
  check_pos "   abc" (Pattern.sequence space (Pattern.p "abc")) 6;
  check_pos "true"   bool_lit 4;
  check_pos "false"  bool_lit 5

let test_not_keyword () =
  (* "ifoo" starts with "if" so not (p "if") fails *)
  check_pos "ifoo"
    (Pattern.sequence
      (Pattern.not (Pattern.p "if"))
      (Pattern.repeat1 (Pattern.choice (Pattern.char 'i') (Pattern.char 'f'))))
    2;
  (* "ig" does not start with "if" so not succeeds *)
  check_pos "ig"
    (Pattern.sequence
      (Pattern.not (Pattern.p "if"))
      (Pattern.repeat1 (Pattern.choice (Pattern.char 'i') (Pattern.char 'g'))))
    2

(* ------------------------------------------------------------------ *)
(* Suite                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "peguin.machine" [
    "empty",          [ Alcotest.test_case "empty pattern"      `Quick test_empty         ];
    "char",           [ Alcotest.test_case "single char"        `Quick test_char           ];
    "sequence",       [ Alcotest.test_case "sequence"           `Quick test_sequence       ];
    "ordered_choice", [ Alcotest.test_case "ordered choice"     `Quick test_ordered_choice ];
    "repeat0",        [ Alcotest.test_case "zero or more"       `Quick test_repeat0        ];
    "repeat1",        [ Alcotest.test_case "one or more"        `Quick test_repeat1        ];
    "not",            [ Alcotest.test_case "negative lookahead" `Quick test_not            ];
    "pos",            [ Alcotest.test_case "positive lookahead" `Quick test_pos            ];
    "composed",       [ Alcotest.test_case "composed patterns"  `Quick test_composed       ];
    "not_keyword",    [ Alcotest.test_case "keyword lookahead"  `Quick test_not_keyword    ];
  ]

