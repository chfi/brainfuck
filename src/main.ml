open Core.Std

type command = IncPtr | DecPtr | IncByte   | DecByte
             | Output | Input  | JumpAhead | JumpBack

let ar_size = 30_000

type state = { ar : int array;
               data_ptr : int;
               inst_ptr : int;
               complete : bool; }



type program = command array

(* debug helper functions *)
let get_cur_byte state = state.ar.(state.data_ptr)
let get_cur_inst state program = program.(state.inst_ptr)

let command_of_char = function
  | '>' -> Some IncPtr
  | '<' -> Some DecPtr
  | '+' -> Some IncByte
  | '-' -> Some DecByte
  | '.' -> Some Output
  | ',' -> Some Input
  | '[' -> Some JumpAhead
  | ']' -> Some JumpBack
  | _   -> None


let next_inst state = { state with inst_ptr = (state.inst_ptr + 1)}

let inc_ptr state = { state with data_ptr = (state.data_ptr + 1) }
let dec_ptr state = { state with data_ptr = (state.data_ptr - 1) }

let inc_byte state =
  let newarray = Array.copy (state.ar) in
  newarray.(state.data_ptr) <- (newarray.(state.data_ptr) + 1);
  { state with ar = newarray }

let dec_byte state =
  let newarray = Array.copy (state.ar) in
  newarray.(state.data_ptr) <- (newarray.(state.data_ptr) - 1);
  { state with ar = newarray }

let output state =
  print_char (char_of_int state.ar.(state.data_ptr));
  state

let input state in_char =
  let newarray = Array.copy (state.ar) in
  newarray.(state.data_ptr) <- int_of_char in_char;
  { state with ar = newarray }


let rec skip_ahead program state depth =
  let tail =
    Array.slice program (state.inst_ptr + 1) 0
  in
  let (offset, cmd) = match Array.findi tail
      ~f:(fun i cmd -> (cmd = JumpAhead || cmd = JumpBack))
    with
    | None -> failwith "Matching ] did not exist"
    | Some (o,c) -> (o + 1,c)
  in
  let newstate = { state with inst_ptr = (state.inst_ptr + offset) } in
  if cmd = JumpBack then
    if (depth = 0) then
      newstate
    else
      skip_ahead program newstate (depth - 1)
  else
    skip_ahead program newstate (depth + 1)


let rec skip_back program state depth =
  let head =
    Array.slice program 0 state.inst_ptr
  in
    (* needs to be reversed since we're searching backwards ... *)
  Array.rev_inplace head;
  let head_len = Array.length head in
  let (offset, cmd) = match Array.findi head
                              ~f:(fun i cmd -> (cmd = JumpAhead || cmd = JumpBack))
    with
    | None -> failwith ("Matching [ did not exist")
    | Some (o,c) -> (o,c)
  in
  (* needs to reverse the offset since it's currently indexed from the end
     of the array, not the beginning *)
  let offset = (head_len - offset) in
  let newstate = { state with inst_ptr = offset } in
  if cmd = JumpAhead then
    if (depth = 0) then
      newstate
    else
      skip_back program newstate (depth - 1)
  else
    skip_back program newstate (depth + 1)


let do_command state cmd program =
  (
    match cmd with
  | IncPtr -> inc_ptr state
  | DecPtr -> dec_ptr state
  | IncByte -> inc_byte state
  | DecByte -> dec_byte state
  | Output -> output state
  | Input -> input state (input_char Core.Std.stdin)
  | JumpAhead ->
    if get_cur_byte state = 0 then
      skip_ahead program state 0
    else
      state
  | JumpBack ->
    if get_cur_byte state <> 0 then
      skip_back program state 0
    else
      state
  )
  |> next_inst


(* hello world *)
let hello_world = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

let add_vals = "[->+<]"

let program = add_vals

let program_of_string str =
  String.to_array str
  |> Array.filter_map ~f:command_of_char

let init_state () = { ar = Array.create ~len:ar_size 0;
                      data_ptr = 0;
                      inst_ptr = 0;
                      complete = false; }

let rec compute state prg =
    let cmd = prg.(state.inst_ptr) in
    let newstate = do_command state cmd prg in
    if newstate.inst_ptr >= ((Array.length prg) - 1) then
      ()
    else
      compute newstate prg

let step (state, prg) =
    let cmd = prg.(state.inst_ptr) in
    let newstate = do_command state cmd prg in
    (newstate,prg)

let rec step_n (state,prg) n =
  if n > 0 then
    let cmd = prg.(state.inst_ptr) in
    let newstate = do_command state cmd prg in
    step_n (newstate,prg) (n-1)
  else
    (state,prg)


let test = "[]++++++++++[>>+>+>++++++[<<+<+++>>>-]<<<<-]\"A*$\";?@![#>>+<<]>[>>]<<<<[>++<[-]]>.>."

let a = init_state ();;
let p = program_of_string test;;

let () =
  let state = init_state () in
  let prg = program_of_string program in

  (* compute state prg *)
    ()
