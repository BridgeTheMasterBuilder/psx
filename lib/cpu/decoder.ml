open Insn
open Uint32

(* open Tsdl *)

let decode_itype opcode word =
  let rs = bits word 21u 25u in
  let rt = bits word 16u 20u in
  let immediate = bits_abs word 0u 15u in
  (* my_assert ((opcode lsl 26u) lor (rs lsl 21u) lor (rt lsl 16u) lor immediate) word; *)
  Itype { op = opcode; rs; rt; immediate }

let decode_rtype opcode word =
  let rs = bits word 21u 25u in
  let rt = bits word 16u 20u in
  let rd = bits word 11u 15u in
  let shamt = bits word 6u 10u in
  assert (
    (rs lsl 21u) lor (rt lsl 16u) lor (rd lsl 11u) lor (shamt lsl 6u) lor opcode
    = word);
  Rtype { op = opcode; rs; rt; rd; shamt }

let decode_cop0 opcode word =
  let rt = bits word 16u 20u in
  let rd = bits word 11u 15u in
  assert (
    (16u lsl 26u) lor (opcode lsl 21u) lor (rt lsl 16u) lor (rd lsl 11u) = word);
  Cop0 { op = opcode; rt; rd }

(* TODO reserved instruction exception excode 0Ah *)
let decode word =
  let opcode = bits word 26u 31u in
  match Uint32.to_int opcode with
  | 0 ->
      let opcode = bits_abs word 0u 5u in
      decode_rtype opcode word
  | 1 ->
      let opcode = bits word 16u 20u in
      decode_itype opcode word
  | 2 | 3 ->
      let op = opcode in
      let target = bits_abs word 0u 25u in
      assert ((opcode lsl 26u) lor target = word);
      Jtype { op; target }
  | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 32 | 33 | 34 | 35 | 36
  | 37 | 38 | 40 | 41 | 42 | 43 | 46 ->
      decode_itype opcode word
  | 16 ->
      let opcode = bits word 21u 25u in
      decode_cop0 opcode word
  | 17 -> failwith "COP1 not present"
  | _ -> failwith (Printf.sprintf "Unknown opcode %X" (Uint32.to_int opcode))
(* TODO exception *)
(* | _ ->
    Sdl.(log_error Log.category_application "Unknown opcode %X" opcode);
    Itype { op = Invalid; rs = Zero; rt = Zero; immediate = 0 } *)
