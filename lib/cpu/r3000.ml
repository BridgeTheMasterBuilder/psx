(* TODO maybe separate into different files *)
open Tsdl
open Insn
open Util
open Misc

type state = Running | Halted | Breakpoint | Watchpoint
(* [@@deriving show] *)

type t = {
  regs : int array;
  mutable cur_pc : int;
  mutable next_pc : int;
  cop0_regs : int array;
  mutable breakpoints : int list;
  mutable write_watchpoints : int list;
  mutable read_watchpoints : int list;
  mutable state : state;
  mutable old_state : state;
}

let clockrate = 33_868_800

let state =
  {
    regs = Array.make 32 0;
    cur_pc = 0xBFBFFFFC;
    next_pc = 0xBFC00000;
    cop0_regs = Array.make 32 0;
    breakpoints = [];
    write_watchpoints = [];
    read_watchpoints = [];
    state = Running;
    old_state = Running;
  }

let dump_registers () =
  Array.to_list state.regs @ [ 0; 0; 0; 0; 0; state.cur_pc; 0; 0; 0 ]

let pc () = state.cur_pc

(* TODO maybe adjust CPI *)
let set_pc addr = state.next_pc <- addr land 0xFFFFFFFF

let set_state s =
  state.state <- s;
  state.old_state <- s

let incr_pc () =
  state.cur_pc <- state.next_pc;
  set_pc (state.next_pc + 4);
  let pc = pc () in
  if List.mem pc state.breakpoints then (
    Sdl.(log_debug Log.category_application "Hit breakpoint at %X" pc);
    set_state Breakpoint)

let add_pc offset = set_pc (state.next_pc + offset)

let fetch () =
  incr_pc ();
  Bus.read_u32 state.cur_pc

(* TODO Invalid, not just unimplemented *)
let invalid_itype_insn op _ _ _ =
  failwithf "Unimplemented I-Type instruction: %s"
    (show_mnemonic itype_opcode_map.(op))

let invalid_jtype_insn op _ =
  failwithf "Unimplemented J-Type instruction: %s"
    (show_mnemonic jtype_opcode_map.(op))

let invalid_rtype_insn op _ _ _ _ =
  failwithf "Unimplemented R-Type instruction: %s"
    (show_mnemonic rtype_opcode_map.(op))

let invalid_cop0_insn op _ _ =
  failwithf "Unimplemented COP-0 instruction: %s"
    (show_mnemonic cop0_opcode_map.(op))

let bne rs rt off =
  let off = i64_of_i16 off lsl 2 in
  if state.regs.(rs) <> state.regs.(rt) then add_pc off else add_pc 4

(* TODO overflow exception *)
let addi rs rt imm =
  let result = state.regs.(rs) + i64_of_i16 imm in
  state.regs.(rt) <- result

let addiu rs rt imm =
  let result = state.regs.(rs) + i64_of_i16 imm in
  state.regs.(rt) <- result

let ori rs rt imm =
  let result = state.regs.(rs) lor imm in
  state.regs.(rt) <- result

let lui _ rt imm =
  let result = imm lsl 16 in
  state.regs.(rt) <- result

let lw base rt off =
  let addr = state.regs.(base) + i64_of_i16 off in
  state.regs.(rt) <- Bus.read_u32 addr

let sw base rt off =
  let addr = state.regs.(base) + i64_of_i16 off in
  Bus.write_u32 addr state.regs.(rt);
  quiet (fun _ ->
      assert (Bus.read_u32 addr = state.regs.(rt) || Bus.read_u32 addr = -1))

let watchpoint_acknowledged = ref false

let execute_watched read write insn rs rt immediate =
  let addr = state.regs.(rs) + i64_of_i16 immediate in
  if
    ((write && List.mem addr state.write_watchpoints)
    || (read && List.mem addr state.read_watchpoints))
    && not !watchpoint_acknowledged
  then (
    Sdl.(log_debug Log.category_application "Hit watchpoint at %X" addr);
    watchpoint_acknowledged := true;
    state.state <- Watchpoint)
  else (
    insn rs rt immediate;
    watchpoint_acknowledged := false;
    state.state <- state.old_state)

let with_watched_writes insn = execute_watched false true insn
let with_watched_reads insn = execute_watched true false insn

let itype_insn_map : (int -> int -> int -> unit) array =
  [|
    invalid_itype_insn 0;
    invalid_itype_insn 1;
    invalid_itype_insn 2;
    invalid_itype_insn 3;
    invalid_itype_insn 4;
    bne;
    invalid_itype_insn 6;
    invalid_itype_insn 7;
    addi;
    addiu;
    invalid_itype_insn 10;
    invalid_itype_insn 11;
    invalid_itype_insn 12;
    ori;
    invalid_itype_insn 14;
    lui;
    invalid_itype_insn 16;
    invalid_itype_insn 17;
    invalid_itype_insn 18;
    invalid_itype_insn 19;
    invalid_itype_insn 20;
    invalid_itype_insn 21;
    invalid_itype_insn 22;
    invalid_itype_insn 23;
    invalid_itype_insn 24;
    invalid_itype_insn 25;
    invalid_itype_insn 26;
    invalid_itype_insn 27;
    invalid_itype_insn 28;
    invalid_itype_insn 29;
    invalid_itype_insn 30;
    invalid_itype_insn 31;
    invalid_itype_insn 32;
    invalid_itype_insn 33;
    invalid_itype_insn 34;
    with_watched_reads lw;
    invalid_itype_insn 36;
    invalid_itype_insn 37;
    invalid_itype_insn 38;
    invalid_itype_insn 39;
    invalid_itype_insn 40;
    invalid_itype_insn 41;
    invalid_itype_insn 42;
    with_watched_writes sw;
  |]

let j target =
  let high_bits = bits_abs state.next_pc 28 31 in
  let target = target lsl 2 in
  let target = high_bits lor target in
  set_pc target

let jtype_insn_map : (int -> unit) array =
  [| invalid_jtype_insn 0; invalid_jtype_insn 1; j; invalid_jtype_insn 3 |]

let or_insn rs rt rd _ =
  let result = state.regs.(rs lor rt) in
  state.regs.(rd) <- result

let sltu rs rt rd _ =
  state.regs.(rd) <- (if state.regs.(rs) < state.regs.(rt) then 1 else 0)

let rtype_insn_map : (int -> int -> int -> int -> unit) array =
  [|
    invalid_rtype_insn 0;
    invalid_rtype_insn 1;
    invalid_rtype_insn 2;
    invalid_rtype_insn 3;
    invalid_rtype_insn 4;
    invalid_rtype_insn 5;
    invalid_rtype_insn 6;
    invalid_rtype_insn 7;
    invalid_rtype_insn 8;
    invalid_rtype_insn 9;
    invalid_rtype_insn 10;
    invalid_rtype_insn 11;
    invalid_rtype_insn 12;
    invalid_rtype_insn 13;
    invalid_rtype_insn 14;
    invalid_rtype_insn 15;
    invalid_rtype_insn 16;
    invalid_rtype_insn 17;
    invalid_rtype_insn 18;
    invalid_rtype_insn 19;
    invalid_rtype_insn 20;
    invalid_rtype_insn 21;
    invalid_rtype_insn 22;
    invalid_rtype_insn 23;
    invalid_rtype_insn 24;
    invalid_rtype_insn 25;
    invalid_rtype_insn 26;
    invalid_rtype_insn 27;
    invalid_rtype_insn 28;
    invalid_rtype_insn 29;
    invalid_rtype_insn 30;
    invalid_rtype_insn 31;
    invalid_rtype_insn 32;
    invalid_rtype_insn 33;
    invalid_rtype_insn 34;
    invalid_rtype_insn 35;
    invalid_rtype_insn 36;
    or_insn;
    invalid_rtype_insn 38;
    invalid_rtype_insn 39;
    invalid_rtype_insn 40;
    invalid_rtype_insn 41;
    invalid_rtype_insn 42;
    sltu;
  |]

let mfc0 rt rd = state.regs.(rt) <- state.cop0_regs.(rd)
let mtc0 rt rd = state.cop0_regs.(rd) <- state.regs.(rt)
let rfe _ _ = failwith "RFE unimplemented"

let cop0_insn_map : (int -> int -> unit) array =
  [|
    mfc0;
    invalid_cop0_insn 1;
    invalid_cop0_insn 2;
    invalid_cop0_insn 3;
    mtc0;
    invalid_cop0_insn 5;
    invalid_cop0_insn 6;
    invalid_cop0_insn 7;
    invalid_cop0_insn 8;
    invalid_cop0_insn 9;
    invalid_cop0_insn 10;
    invalid_cop0_insn 11;
    invalid_cop0_insn 12;
    invalid_cop0_insn 13;
    invalid_cop0_insn 14;
    invalid_cop0_insn 15;
    rfe;
  |]

let execute = function
  | Itype { op; rs; rt; immediate } -> itype_insn_map.(op) rs rt immediate
  | Jtype { op; target } -> jtype_insn_map.(op) target
  | Rtype { op; rs; rt; rd; shamt } -> rtype_insn_map.(op) rs rt rd shamt
  | Cop0 { op; rt; rd } -> cop0_insn_map.(op) rt rd

let fetch_decode_execute () =
  let pc = pc () in
  let word = fetch () in
  let insn = Decoder.decode word in
  (match insn with
  | Rtype { op; rs = 0; rt = 0; rd = 0; shamt = 0 }
    when rtype_opcode_map.(op) = Sll ->
      Sdl.(log_debug Log.category_application "%X: NOP" pc)
  | _ ->
      Sdl.(log_debug Log.category_application "%X: %s" pc (Insn.show_insn insn));
      execute insn);
  state.state
