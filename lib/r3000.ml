(* open Util *)

type state = Running
type t = { regs : int array; mutable pc : int; }

let state = { regs = Array.make 32 0; pc = 0xBFC00000; }

let read_u32 () =
  let pc = state.pc in
  state.pc <- (state.pc + 1) land 0xFFFFFFFF;
  Ram.read_u32 pc

(* let read_u16 () =
  let high = read_u8 () in
  let low = read_u8 () in
  (high lsl 8) lor low *)

(* let jump target = state.pc <- target
let mov reg value = state.regs.(reg) <- value
let set_flag () = state.regs.(0xF) <- 1
let clear_flag () = state.regs.(0xF) <- 0
let add reg value = state.regs.(reg) <- (state.regs.(reg) + value) land 0xFF

let draw x y n =
  let rec draw_row i collision =
    let row = y + i in
    if i >= n || row > 31 then collision
    else
      let idx_reg = state.idx_reg in
      let idx = (idx_reg + i) land 0xFFFF in
      let spr_row = Lwt_bytes.get Ram.ram idx |> int_of_char in

      let rec draw_col j collision =
        let col = x + j in

        if j >= 8 || col > 63 then draw_row (i + 1) collision
        else
          let spr_px = bit spr_row (7 - j) > 0 in
          let scr_px = Vram.read row col > 0 in

          if spr_px && scr_px then (
            Vram.write row col 0xFFFFFFFFl;
            draw_col (j + 1) true)
          else (
            Vram.write row col (if spr_px <> scr_px then 0xFFl else 0xFFFFFFFFl);
            draw_col (j + 1) collision)
      in
      draw_col 0 collision
  in
  let collision = draw_row 0 false in
  if collision then set_flag () else clear_flag () *)

let fetch_decode_execute () = 
  let insn = read_u32 () in
  match insn with
  | x -> failwith (Printf.sprintf "Unknown opcode %X\n" (x land 0b11_1111)) 
  (* let insn = read_u16 () in
  let opcode = bits insn 12 15 in
  match opcode with
  | 0x0 -> (
      match bits_abs insn 0 11 with
      | 0x0E0 ->
          Vram.clear ();
          Running
      | _ ->
          Printf.eprintf "Unknown inner opcode %X\n" opcode;
          exit 1)
  | 0x1 ->
      let target = bits_abs insn 0 11 in
      jump target;
      Running
  | 0x6 ->
      let reg = bits insn 8 11 in
      let value = bits insn 0 7 in
      mov reg value;
      Running
  | 0x7 ->
      let reg = bits insn 8 11 in
      let value = bits insn 0 7 in
      add reg value;
      Running
  | 0xA ->
      let value = bits insn 0 11 in
      state.idx_reg <- value;
      Running
  | 0xD ->
      let x = bits insn 8 11 in
      let y = bits insn 4 7 in
      let n = bits insn 0 3 in
      let x = state.regs.(x) mod 64 in
      let y = state.regs.(y) mod 32 in
      draw x y n;
      Running *)
  (* | _ ->
      Printf.eprintf "Unknown opcode %X\n" opcode;
      exit 1 *)
