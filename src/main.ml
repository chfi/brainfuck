open Core.Std

type command = IncPtr | DecPtr | IncByte   | DecByte
             | Output | Input  | JumpAhead | JumpBack


let ar_size = 30_000



type state = { ar : int array;
               data_ptr : int;
               inst_ptr : int;
               complete : bool; }


type program = command array


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


let inc_ptr state = { state with data_ptr = (state.data_ptr + 1);
                                 inst_ptr = (state.inst_ptr + 1) }
let dec_ptr state = { state with data_ptr = (state.data_ptr - 1);
                                 inst_ptr = (state.inst_ptr + 1) }

let inc_byte state =
  let newarray = Array.copy (state.ar) in
  newarray.(state.data_ptr) <- (newarray.(state.data_ptr) + 1);
  { state with ar = newarray;
               inst_ptr = (state.inst_ptr + 1) }

let dec_byte state =
  let newarray = Array.copy (state.ar) in
  newarray.(state.data_ptr) <- (newarray.(state.data_ptr) - 1);
  { state with ar = newarray;
               inst_ptr = (state.inst_ptr + 1) }

let output state =
  print_char (char_of_int state.ar.(state.data_ptr));
  { state with inst_ptr = (state.inst_ptr + 1) }

let input state in_char =
  let newarray = Array.copy (state.ar) in
  newarray.(state.data_ptr) <- int_of_char in_char;
  { state with ar = newarray;
               inst_ptr = (state.inst_ptr + 1) }


(* I want to scan through the array, starting from state.inst_ptr,
   and upon reaching a [, recurse, and a ], step forward one and return. *)
let jump_ahead program state =
  let start = state.inst_ptr in
  let tail = Array.slice program state.inst_ptr 0 in
  let offset = match Array.findi tail ~f:(fun i c -> c = JumpBack) with
    | None -> failwith "Didn't find ]!"
    | Some (i,_) -> i
  in
  { state with inst_ptr = (start + offset) + 1 }

let jump_back program state =
  let start = state.inst_ptr in
  let head = Array.slice program 0 state.inst_ptr in
  let bracket = match Array.findi head ~f:(fun i c -> c = JumpAhead) with
    | None -> failwith "Didn't find [!"
    | Some (i,_) -> i
  in
  { state with inst_ptr = bracket + 1}


let do_command state cmd program =
  match cmd with
  | IncPtr -> inc_ptr state
  | DecPtr -> dec_ptr state
  | IncByte -> inc_byte state
  | DecByte -> dec_byte state
  | Output -> output state
  | Input -> input state (input_char Core.Std.stdin)
  | JumpAhead -> jump_ahead program state
  | JumpBack -> jump_back program state


(* hello world *)
let program = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

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

(* let run_with_program prg f = f prg *)

let () =
  let state = init_state () in
  let prg = program_of_string program in

  compute state prg
