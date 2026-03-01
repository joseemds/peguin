type instr =
  | Empty
  | Any
  | Fail
  | Char of int
  | Choice of int
  | Jump of int
  | Call of int
  | Commit of int
  | Return

module Stack = struct
  type entry = ReturnAddr of int | BacktrackEntry of int * int
  type t = entry list
end

type state = int * int * Stack.t
type stack = instr Stdlib.Stack.t
type pos = int
type vm = { opcode : instr; offset : int; codesize : int }

(*Next instruction; subject curr position; Stack*)

type input = instr * pos * stack
type program = instr list
type transform = Pattern.t -> program

(* let match_ ((instruction, pos, stack) as state) input =  *)
(*    let module Stack = Stdlib.Stack in *)
(*    match instruction with *)
(*   | Char c -> (if String.get input pos = c then *)
(*       Ok(instruction, pos+1, stack) *)
(*     else *)
(*       Error(pos, stack) *)
(*     ) *)
(*   | Any  -> (if pos+1 <= String.length input  then *)
(**)
(*      Ok(instruction, pos+1, stack) *)
(*       else  *)
(*       Error(pos, stack) *)
(*     ) *)
(*   | Jump l -> Ok(l, pos, stack) *)
(*   | Return -> Ok(Stack.pop stack, pos, stack) *)
(*   | Choice l -> Ok(Stack.pop stack, pos, (Stack.push l stack; stack)) *)
(*   | Call l -> Ok((Stack.push l stack; l), pos, stack) *)
(*   | Fail -> Error(pos, stack) *)
(*   | Commit _ -> Ok(state) *)
(*   | Empty -> Ok(state) *)
