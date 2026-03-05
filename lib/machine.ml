module Stack = struct
  type entry = ReturnAddr of int | BacktrackEntry of int * int
  type t = entry list
  let empty = []
end

type state = int * int * Stack.t
type stack = Op.t Stdlib.Stack.t
type pos = int
type vm = { opcode : Op.t; offset : int; codesize : int }


type input = Op.t * pos * stack
type program = Op.t list
type transform = Pattern.t -> program


let step ((pc, pos, e) as state) buf pat = match pat with
| Op.Empty -> state
| Op.Any -> (pc+1, pos+1, e)
| Op.Fail -> state
| Op.Return -> state
| Op.Char c -> 
    if Bytes.get buf pos = c then
      (pc+1, pos+1, e)
    else
      state
| Op.Choice _ -> state
| Op.Jump _ -> state
| Op.Call _ -> state
| Op.Commit _ -> state

let run_ state buf pat =
  let state' = step state buf pat in
  state'

let run pat input =
  let buf = Bytes.of_string input in
  let _stack = Stack.empty in
  let instrs = Pattern.compile pat in
  match instrs with
  | [] -> (0, 0, Stack.empty)
  | start :: _rest -> 
    let initial_state = (0, 0, Stack.empty) in
    run_ initial_state buf start
