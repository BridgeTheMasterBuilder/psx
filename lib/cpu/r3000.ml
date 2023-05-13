(* TODO maybe separate into different files *)
open Tsdl
open Insn
open Register
open Util
open Misc

type state = Running | Halted | Breakpoint | Watchpoint [@@deriving show]

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
  mutable load_queue : (int * int) list;
}

let clockrate = 33_868_800

let state =
  {
    regs = Array.make 32 0;
    cur_pc = 0xBFC00000;
    next_pc = 0xBFC00004;
    cop0_regs = Array.make 32 0;
    breakpoints = [];
    write_watchpoints = [];
    read_watchpoints = [];
    state = Running;
    old_state = Running;
    load_queue = [];
  }

let ( + ) x y = (x + y) land 0xFFFFFFFF
let ( - ) x y = (x - y) land 0xFFFFFFFF
let ( * ) x y = x * y land 0xFFFFFFFF
let ( / ) x y = x / y land 0xFFFFFFFF

let dump_registers () =
  Array.to_list state.regs @ [ 0; 0; 0; 0; 0; state.cur_pc; 0; 0; 0 ]

let pc () = state.cur_pc

(* TODO maybe adjust CPI *)
let set_pc addr = state.next_pc <- addr

let set_state s =
  state.state <- s;
  state.old_state <- s

let incr_pc () =
  state.cur_pc <- state.next_pc;
  set_pc (state.next_pc + 4)

let fetch () =
  let word = Bus.read_u32 state.cur_pc in
  incr_pc ();
  word

(* TODO Invalid, not just unimplemented *)
let invalid_itype_insn op _ _ _ =
  failwithf "Unimplemented I-Type instruction: %s" itype_opcode_map.(op)

let invalid_jtype_insn op _ =
  failwithf "Unimplemented J-Type instruction: %s" jtype_opcode_map.(op)

let invalid_rtype_insn op _ _ _ _ =
  failwithf "Unimplemented R-Type instruction: %s" rtype_opcode_map.(op)

let invalid_cop0_insn op _ _ =
  failwithf "Unimplemented COP-0 instruction: %s" cop0_opcode_map.(op)

let bcc cond off =
  let off = i32_of_i16 off lsl 2 in
  if cond then set_pc (state.cur_pc + off)

let bltz rs _ off = bcc (bit state.regs.(rs) 31 = 1) off
let beq rs rt off = bcc (state.regs.(rs) = state.regs.(rt)) off
let bne rs rt off = bcc (state.regs.(rs) <> state.regs.(rt)) off
let blez rs _ off = bcc (bit state.regs.(rs) 31 = 1 || state.regs.(rs) = 0) off
let bgtz rs _ off = bcc (bit state.regs.(rs) 31 = 0 && state.regs.(rs) <> 0) off

let with_overflow_check f =
  let result = f () in
  if false then failwith "Overflow" else result

let addi rs rt imm =
  let result =
    with_overflow_check (fun _ -> state.regs.(rs) + i32_of_i16 imm)
  in
  state.regs.(rt) <- result

let addiu rs rt imm =
  let result = state.regs.(rs) + i32_of_i16 imm in
  state.regs.(rt) <- result

let subi rs rt imm =
  let result =
    with_overflow_check (fun _ -> state.regs.(rs) - i32_of_i16 imm)
  in
  state.regs.(rt) <- result

let subiu rs rt imm =
  let result = state.regs.(rs) - i32_of_i16 imm in
  state.regs.(rt) <- result

let andi rs rt imm =
  let result = state.regs.(rs) land imm in
  state.regs.(rt) <- result;
  assert (result < int_of_float (2.0 ** 32.0))

let ori rs rt imm =
  let result = state.regs.(rs) lor imm in
  state.regs.(rt) <- result

let lui _ rt imm =
  let result = imm lsl 16 in
  state.regs.(rt) <- result

let load base off reader =
  let addr = state.regs.(base) + i32_of_i16 off in
  reader addr

let delay reg value =
  (* if state.load_queue = [] then
     state.load_queue <- (reg, state.regs.(reg)) :: state.load_queue; *)
  state.load_queue <- (reg, value) :: state.load_queue

let lb base rt off =
  let result = load base off Bus.read_u8 |> i32_of_i8 in
  delay rt result

let lh base rt off =
  let result = load base off Bus.read_u16 |> i32_of_i16 in
  delay rt result

let lw base rt off =
  my_assert ((state.regs.(base) + i32_of_i16 off) land 0x3) 0;
  let result = load base off Bus.read_u32 in
  delay rt result

let lbu base rt off =
  let result = load base off Bus.read_u8 in
  delay rt result

let store value base off writer =
  let addr = state.regs.(base) + i32_of_i16 off in
  writer addr value
(* quiet (fun _ -> my_assert_either (Bus.read_u32 addr) value (-1)) *)

let sb base rt off = store state.regs.(rt) base off Bus.write_u8
let sh base rt off = store state.regs.(rt) base off Bus.write_u16

let sw base rt off =
  my_assert ((state.regs.(base) + i32_of_i16 off) land 0x3) 0;
  store state.regs.(rt) base off Bus.write_u32

let watchpoint_acknowledged = ref false

let execute_watched read write insn rs rt immediate =
  let addr = state.regs.(rs) + i32_of_i16 immediate in
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
    bltz;
    invalid_itype_insn 1;
    invalid_itype_insn 2;
    invalid_itype_insn 3;
    beq;
    bne;
    blez;
    bgtz;
    addi;
    addiu;
    subi;
    subiu;
    andi;
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
    with_watched_reads lb;
    with_watched_reads lh;
    invalid_itype_insn 34;
    with_watched_reads lw;
    with_watched_reads lbu;
    invalid_itype_insn 37;
    invalid_itype_insn 38;
    invalid_itype_insn 39;
    with_watched_writes sb;
    with_watched_writes sh;
    invalid_itype_insn 42;
    with_watched_writes sw;
  |]

let calculate_effective_address target =
  let high_bits = bits_abs state.next_pc 28 31 in
  let target = target lsl 2 in
  high_bits lor target

let j target =
  let target = calculate_effective_address target in
  set_pc target

let jal target =
  let ra = 31 in
  state.regs.(ra) <- state.next_pc;
  let target = calculate_effective_address target in
  set_pc target

let jtype_insn_map : (int -> unit) array =
  [| invalid_jtype_insn 0; invalid_jtype_insn 1; j; jal |]

let sll _ rt rd shamt =
  let result = state.regs.(rt) lsl shamt in
  state.regs.(rd) <- result

let sllv rs rt rd _ =
  let shamt = state.regs.(rs) land 0x1F in
  let result = state.regs.(rt) lsl shamt in
  state.regs.(rd) <- result

(* TODO address error exception *)
let jr rs _ _ _ =
  let target = state.regs.(rs) in
  set_pc target;
  my_assert (target land 0x3) 0

let jalr rs _ rd _ =
  let target = state.regs.(rs) in
  state.regs.(rd) <- state.next_pc;
  set_pc target;
  my_assert (target land 0x3) 0

let add rs rt rd _ =
  let result =
    with_overflow_check (fun _ -> state.regs.(rs) + state.regs.(rt))
  in
  state.regs.(rd) <- result

let addu rs rt rd _ =
  let result = state.regs.(rs) + state.regs.(rt) in
  state.regs.(rd) <- result

let sub rs rt rd _ =
  let result =
    with_overflow_check (fun _ -> state.regs.(rs) - state.regs.(rt))
  in
  state.regs.(rd) <- result

let subu rs rt rd _ =
  let result = state.regs.(rs) - state.regs.(rt) in
  state.regs.(rd) <- result

let and_insn rs rt rd _ =
  let result = state.regs.(rs) land state.regs.(rt) in
  state.regs.(rd) <- result

let or_insn rs rt rd _ =
  let result = state.regs.(rs) lor state.regs.(rt) in
  state.regs.(rd) <- result

let sltu rs rt rd _ =
  state.regs.(rd) <- (if state.regs.(rs) < state.regs.(rt) then 1 else 0)

let rtype_insn_map : (int -> int -> int -> int -> unit) array =
  [|
    sll;
    invalid_rtype_insn 1;
    invalid_rtype_insn 2;
    invalid_rtype_insn 3;
    sllv;
    invalid_rtype_insn 5;
    invalid_rtype_insn 6;
    invalid_rtype_insn 7;
    jr;
    jalr;
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
    add;
    addu;
    sub;
    subu;
    and_insn;
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

let check_for_breakpoint pc =
  if List.mem pc state.breakpoints then (
    Sdl.(log_debug Log.category_application "Hit breakpoint at %X" pc);
    set_state Breakpoint)

let show_insn = function
  | Itype { op; rs; rt; immediate }
    when op = 32 || op = 33 || op = 35 || op = 36 || op = 40 || op = 41
         || op = 43 -> (
      let mnemonic, reader =
        match op with
        | 32 -> ("lb", Bus.read_u8)
        | 33 -> ("lh", Bus.read_u16)
        | 35 -> ("lw", Bus.read_u32)
        | 36 -> ("lbu", Bus.read_u8)
        | 40 -> ("sb", Bus.read_u8)
        | 41 -> ("sh", Bus.read_u16)
        | 43 -> ("sw", Bus.read_u32)
        | _ -> failwith " foo"
      in
      match op with
      | 32 | 36 | 40 ->
          Printf.sprintf "%-6s $%s(%08x), 0x%04x(%s)([%08x] = %02x)" mnemonic
            register_map.(rt) state.regs.(rt) immediate register_map.(rs)
            (state.regs.(rs) + i32_of_i16 immediate)
            (load rs immediate reader)
      | 33 | 41 ->
          Printf.sprintf "%-6s $%s(%08x), 0x%04x(%s)([%08x] = %04x)" mnemonic
            register_map.(rt) state.regs.(rt) immediate register_map.(rs)
            (state.regs.(rs) + i32_of_i16 immediate)
            (load rs immediate reader)
      | 35 | 43 ->
          Printf.sprintf "%-6s $%s(%08x), 0x%04x(%s)([%08x] = %08x)" mnemonic
            register_map.(rt) state.regs.(rt) immediate register_map.(rs)
            (state.regs.(rs) + i32_of_i16 immediate)
            (load rs immediate reader)
      | _ -> failwith "unreachable")
  | Itype { op; rs; rt; immediate } when rs = 0 || rs = rt ->
      Printf.sprintf "%-6s $%s(%08x), 0x%04x"
        (* (match op with 9 -> "move" | _ ->  *)
        itype_opcode_map.(op)
        (* ) *)
        register_map.(rt) state.regs.(rt) immediate
  | Itype { op; rs; rt; immediate } ->
      Printf.sprintf "%-6s $%s(%08x), $%s(%08x), 0x%04x" itype_opcode_map.(op)
        register_map.(rs) state.regs.(rs) register_map.(rt) state.regs.(rt)
        (state.cur_pc + (i32_of_i16 immediate lsl 2))
  (* | Itype { op; rs; rt; immediate } ->
      "" *)
  (* Printf.sprintf "%-6s $%s(%08x), 0x%04X" itype_opcode_map.(op)
     register_map.(rt) 0 immediate *)
  | Jtype { op; target } ->
      Printf.sprintf "%-6s 0x%08x" jtype_opcode_map.(op)
        (calculate_effective_address target)
  | Rtype { op = 0; rs = 0; rt = 0; rd = 0; shamt = 0 } -> "nop"
  | Rtype { op = 8; rs; rt = 0; rd = 0; shamt = 0 } ->
      Printf.sprintf "%-6s $%s(%08x)" "jr" register_map.(rs) state.regs.(rs)
  | Rtype { op = 9; rs; rt = 0; rd; shamt = 0 } ->
      Printf.sprintf "%-6s $%s(%08x)" "jalr" register_map.(rs) state.regs.(rs)
  | Rtype { op; rs; rt = 0; rd; _ } ->
      Printf.sprintf "%-6s $%s(%08x), $%s(%08x)"
        (* (match op with 37 -> "move" |
           _ -> *)
        rtype_opcode_map.(op)
        (* ) *)
        register_map.(rd) state.regs.(rd) register_map.(rs) state.regs.(rs)
  | Rtype { op; rs; rt; rd; _ } ->
      Printf.sprintf "%-6s $%s(%08x), $%s(%08x), $%s(%08x)"
        rtype_opcode_map.(op) register_map.(rd) state.regs.(rd)
        register_map.(rs) state.regs.(rs) register_map.(rt) state.regs.(rt)
  | Cop0 { op = 0; rt; rd } ->
      Printf.sprintf "%-6s $%s(%08x), $%s(%08x)" "mfc0" register_map.(rt)
        state.regs.(rt) cop0_map.(rd) state.cop0_regs.(rd)
  | Cop0 { op = 4; rt; rd } ->
      Printf.sprintf "%-6s $%s(%08x), $%s(%08x)" "mtc0" cop0_map.(rd)
        state.cop0_regs.(rd) register_map.(rt) state.regs.(rt)
  | _ -> failwith ""

let fetch_decode_execute () =
  let pc = pc () in
  let word = fetch () in
  let insn = Decoder.decode word in
  Printf.printf "%08x %08x: %s\n" pc word (show_insn insn);
  flush stdout;
  (match state.load_queue with
  | (reg, value) :: rest ->
      state.regs.(reg) <- value;
      state.load_queue <- rest
  | [] -> ());
  (* TODO *)
  execute insn;
  assert (state.regs.(0) = 0);
  (* Sdl.(log_debug Log.category_application "State: %s" (show_state state.state)); *)
  ()
