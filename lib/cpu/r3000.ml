(* open Util *)
open Tsdl
open Insn

type state = Running
type t = { regs : int array; mutable pc : int }

let clockrate = 33_868_800_000
let state = { regs = Array.make 32 0; pc = 0xBFC00000 }

let read_u32 () =
  let pc = state.pc in
  state.pc <- (state.pc + 1) land 0xFFFFFFFF;
  Bus.read_u32 pc

let fetch = read_u32

let fetch_decode_execute () =
  let word = fetch () in
  let insn = Decoder.decode word in
  (match insn with
  | Itype { op = Invalid; _ } -> ()
  | Rtype { op = Sll; rs = Zero; rt = Zero; rd = Zero; shamt = 0 } ->
      Sdl.(log_debug Log.category_application "NOP")
  | _ -> Sdl.(log_debug Log.category_application "%s" (Insn.show_insn insn)));
  Running

let dump_registers () =
  Array.to_list state.regs @ [ 0; 0; 0; 0; 0; state.pc; 0; 0; 0 ]
