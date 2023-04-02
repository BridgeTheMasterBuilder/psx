(* open Util *)
open Tsdl
open Insn

type t = { regs : int array; mutable cur_pc : int; mutable next_pc : int }

let clockrate = 33_868_800_000

let state =
  { regs = Array.make 32 0; cur_pc = 0xBFC00000; next_pc = 0xBFC00000 }

let dump_registers () =
  Array.to_list state.regs @ [ 0; 0; 0; 0; 0; state.cur_pc; 0; 0; 0 ]

let fetch () =
  let pc = state.cur_pc in
  state.cur_pc <- state.next_pc;
  state.next_pc <- (state.next_pc + 4) land 0xFFFFFFFF;
  Bus.read_u32 pc

let fetch_decode_execute () =
  let word = fetch () in
  let insn = Decoder.decode word in
  match insn with
  | Itype { op = Invalid; _ } -> ()
  | Rtype { op = Sll; rs = Zero; rt = Zero; rd = Zero; shamt = 0 } ->
      Sdl.(log_debug Log.category_application "%X: NOP" state.cur_pc)
  | _ ->
      Sdl.(
        log_debug Log.category_application "%X: %s" state.cur_pc
          (Insn.show_insn insn))

let pc () = state.cur_pc
