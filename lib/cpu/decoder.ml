open Insn
open Uint32

(* open Tsdl *)

let decode_itype opcode word =
  let rs = bits word 21 25 in
  let rt = bits word 16 20 in
  let immediate = bits_abs word 0 15 in
  (* my_assert ((opcode lsl 26) lor (rs lsl 21) lor (rt lsl 16) lor immediate) word; *)
  Itype { op = opcode; rs; rt; immediate }

let decode_rtype opcode word =
  let rs = bits word 21 25 in
  let rt = bits word 16 20 in
  let rd = bits word 11 15 in
  let shamt = bits word 6 10 in
  assert (
    (rs lsl 21) lor (rt lsl 16) lor (rd lsl 11) lor (shamt lsl 6) lor opcode
    = word);
  Rtype { op = opcode; rs; rt; rd; shamt }

let decode_cop0 opcode word =
  let rt = bits word 16 20 in
  let rd = bits word 11 15 in
  assert ((16 lsl 26) lor (opcode lsl 21) lor (rt lsl 16) lor (rd lsl 11) = word);
  Cop0 { op = opcode; rt; rd }

(* TODO reserved instruction exception excode 0Ah *)
let decode word =
  let opcode = bits word 26 31 in
  match opcode with
  | 0 ->
      let opcode = bits_abs word 0 5 in
      decode_rtype opcode word
  | 1 ->
      let opcode = bits word 16 20 in
      decode_itype opcode word
  | 2 | 3 ->
      let op = opcode in
      let target = bits_abs word 0 25 in
      assert ((opcode lsl 26) lor target = word);
      Jtype { op; target }
  | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 32 | 33 | 34 | 35 | 36
  | 37 | 38 | 40 | 41 | 42 | 43 | 46 ->
      decode_itype opcode word
  | 16 ->
      let opcode = bits word 21 25 in
      decode_cop0 opcode word
  | 17 -> failwith "COP1 not present"
  | _ -> failwith (Printf.sprintf "Unknown opcode %X" opcode)
(* TODO exception *)
(* | _ ->
    Sdl.(log_error Log.category_application "Unknown opcode %X" opcode);
    Itype { op = Invalid; rs = Zero; rt = Zero; immediate = 0 } *)
