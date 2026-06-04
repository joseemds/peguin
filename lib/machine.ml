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

  let pp ppf = function
    | ReturnAddr l -> Format.fprintf ppf "ReturnAddr %d" l
    | BacktrackEntry (lbl, pos) ->
        Format.fprintf ppf "BacktrackEntry (%d, %d)" lbl pos

  let pop stack =
    match stack with
    | [] -> failwith "Can not pop empty stack"
    | e :: s' -> (e, s')

  let empty = []
end

type state = int * int * Stack.t
type pos = int
type program = Op.t list
type parser_result = (int, int) Result.t

type step_result =
  | Finished of parser_result
  | Continue of state
  | Fail of state

let backtrack_fail stack (_pc, pos, _) =
  match Stack.find_backtrack stack with
  | Some state' -> Continue state'
  | None -> Finished (Error pos)

let step ((pc, pos, e) as state) buf program =
  if pc >= Array.length program then begin
    failwith "pc overflow"
  end
  else
    match program.(pc) with
    | Op.Empty -> Continue (pc + 1, pos, e)
    | Op.Any ->
        if pos < Bytes.length buf then Continue (pc + 1, pos + 1, e)
        else Fail state
    | Op.Fail -> Fail state
    | Op.Return -> (
        match e with
        | Stack.ReturnAddr pc' :: e' -> Continue (pc', pos, e')
        | _ -> Fail state)
    | Op.Char c ->
        if pos < Bytes.length buf && Bytes.get buf pos = c then
          Continue (pc + 1, pos + 1, e)
        else backtrack_fail e state
    | Op.Choice l ->
        let new_entry = Stack.BacktrackEntry (pc + l, pos) in
        Continue (pc + 1, pos, new_entry :: e)
    | Op.Jump l -> Continue (pc + l, pos, e)
    | Op.Call l -> Continue (pc + l, pos, Stack.ReturnAddr (pc + 1) :: e)
    | Op.Commit l -> (
        match e with [] -> Fail state | _h :: e' -> Continue (pc + l, pos, e'))
    | Op.FailTwice ->
        let _, stack' = Stack.pop e in
        let state' = (pc, pos, stack') in
        Fail state'
    | Op.PartialCommit l ->
        let top, stack' = Stack.pop e in
        let pc', _ =
          match top with
          | BacktrackEntry (l, pos) -> (l, pos)
          | ReturnAddr _addr -> failwith "expected backtrack entry"
        in

        Continue (pc + l, pos, Stack.BacktrackEntry (pc', l) :: stack')
    | Op.BackCommit l ->
        let top, stack' = Stack.pop e in
        let _, pos' =
          match top with
          | BacktrackEntry (l, pos) -> (l, pos)
          | ReturnAddr _addr -> failwith "expected backtrack entry"
        in
        Continue (pc + l, pos', stack')
    | Op.Finish -> Finished (Ok pos)

let rec run_ state buf program =
  match step state buf program with
  | Continue state' -> run_ state' buf program
  | Finished result -> result
  | Fail (_, _, stack) -> (
      match backtrack_fail stack state with
      | Continue state' -> run_ state' buf program
      | Finished result -> result
      | _ -> failwith "Unreachable")

let run pat input =
  let buf = Bytes.of_string input in
  let stack = Stack.empty in
  let program = Array.of_list @@ Pattern.compile pat @ [ Op.Finish ] in
  let state = (0, 0, stack) in
  run_ state buf program
