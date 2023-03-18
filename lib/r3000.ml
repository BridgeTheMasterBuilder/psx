(* open Util *)
open Tsdl

type state = Running
type t = { regs : int array; mutable pc : int }

let state = { regs = Array.make 32 0; pc = 0xBFC00000 }

let read_u32 () =
  let pc = state.pc in
  state.pc <- (state.pc + 1) land 0xFFFFFFFF;
  Bus.read_u32 pc

let fetch = read_u32

let fetch_decode_execute () =
  let word = fetch () in
  let insn = Insn.decode word in
  Sdl.(log_debug Log.category_application "%s" (Insn.show_itype insn));
  match insn with { op = Lui; rs; rt; immediate } -> ()
