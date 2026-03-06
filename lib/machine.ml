module Stack = struct
  type entry =
    | ReturnAddr of int (* return_addr *)
    | BacktrackEntry of int * int (* lbl, pos *)

  type t = entry list

  let rec find_backtrack stack =
    match stack with
    | [] -> None
    | BacktrackEntry (lbl, pos) :: stack' -> Some (lbl, pos, stack')
    | _ :: rest -> find_backtrack rest

  let empty = []
end

type state = int * int * Stack.t
type stack = Op.t Stdlib.Stack.t
type pos = int
type vm = { opcode : Op.t; offset : int; codesize : int }
type program = Op.t list
type step_result = Finished of state | Continue of state | Fail of state

let backtrack_fail stack state =
  match Stack.find_backtrack stack with
  | Some state' -> Continue state'
  | None -> Fail state

let step ((pc, pos, e) as state) buf program =
  if pc >= Array.length program then Fail state
  else
    match program.(pc) with
    | Op.Empty -> Continue (pc+1, pos, e)
    | Op.Any ->
        if pos < Bytes.length buf then Continue (pc + 1, pos + 1, e)
        else Fail state
    | Op.Fail -> (
        match Stack.find_backtrack e with
        | Some state -> Continue state
        | None -> Fail state)
    | Op.Return -> Finished state
    | Op.Char c ->
        if pos < Bytes.length buf && Bytes.get buf pos = c then Continue (pc + 1, pos + 1, e)
        else backtrack_fail e state
    | Op.Choice l ->
        let new_entry = Stack.BacktrackEntry (pc + l, pos) in
        Continue (pc + 1, pos, new_entry :: e)
    | Op.Jump l -> Continue (pc + l, pos, e)
    | Op.Call l -> Continue (pc + l, pos, Stack.ReturnAddr (pc + 1) :: e)
    | Op.Commit l -> (
        match e with [] -> Fail state | _h :: e' -> Continue (pc + l, pos, e'))

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
