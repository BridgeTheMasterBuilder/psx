open Tsdl
open Insn
open Util

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

let pc () = state.cur_pc

let invalid_itype_insn op _rs _rt _imm =
  failwithf "Unimplemented I-Type instruction: %s"
    (show_mnemonic itype_opcode_map.(op))

let invalid_jtype_insn op =
  failwithf "Unimplemented J-Type instruction: %s"
    (show_mnemonic jtype_opcode_map.(op))

let invalid_rtype_insn op =
  failwithf "Unimplemented R-Type instruction: %s"
    (show_mnemonic rtype_opcode_map.(op))

let invalid_cop0_insn op =
  failwithf "Unimplemented COP-0 instruction: %s"
    (show_mnemonic cop0_opcode_map.(op))

let lui _ rt imm =
  let result = imm lsl 16 in
  state.regs.(rt) <- result

let itype_insn_map : (int -> int -> int -> unit) array =
  [|
    invalid_itype_insn 0;
    invalid_itype_insn 1;
    invalid_itype_insn 2;
    invalid_itype_insn 3;
    invalid_itype_insn 4;
    invalid_itype_insn 5;
    invalid_itype_insn 6;
    invalid_itype_insn 7;
    invalid_itype_insn 8;
    invalid_itype_insn 9;
    invalid_itype_insn 10;
    invalid_itype_insn 11;
    invalid_itype_insn 12;
    invalid_itype_insn 13;
    invalid_itype_insn 14;
    lui;
    (* TODO complete map *)
  |]

(* TODO do the other maps *)

let execute = function
  | Itype { op; rs; rt; immediate } -> itype_insn_map.(op) rs rt immediate
  (* TODO other insn types *)
  | _ -> ()

let fetch_decode_execute () =
  let pc = pc () in
  let word = fetch () in
  let insn = Decoder.decode word in
  match insn with
  | Itype { op; _ } when itype_opcode_map.(op) = Invalid -> ()
  | Rtype { op; rs = 0; rt = 0; rd = 0; shamt = 0 }
    when rtype_opcode_map.(op) = Sll ->
      Sdl.(log_debug Log.category_application "%X: NOP" pc)
  | _ ->
      Sdl.(log_debug Log.category_application "%X: %s" pc (Insn.show_insn insn));
      execute insn
