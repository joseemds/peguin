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
type step_result = Finished of state | Continue of state | Fail of state

let step ((pc, pos, e) as state) buf program =
  if pc >= Array.length program then Fail state
  else
    match program.(pc) with
    | Op.Empty -> Continue state
    | Op.Any ->
        if pos < Bytes.length buf then Continue (pc + 1, pos + 1, e)
        else Fail state
    | Op.Fail -> Fail state
    | Op.Return -> Finished state
    | Op.Char c ->
        if Bytes.get buf pos = c then Continue (pc + 1, pos + 1, e)
        else Fail state
    | Op.Choice _ -> Fail state
    | Op.Jump _ -> Fail state
    | Op.Call _ -> Fail state
    | Op.Commit _ -> Fail state

let rec run_ state buf program =
  match step state buf program with
  | Continue state' -> run_ state' buf program
  | Finished (_, pos, _) -> pos
  | Fail (_, pos, _) -> pos

let run pat input =
  let buf = Bytes.of_string input in
  let stack = Stack.empty in
  let program = Array.of_list @@ Pattern.compile pat in
  let state = (0, 0, stack) in
  run_ state buf program
