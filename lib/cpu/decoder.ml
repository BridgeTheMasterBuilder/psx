open Util
open Insn
(* open Tsdl *)

let decode_itype opcode word =
  let op = itype_opcode_map.(opcode) in
  let rs = bits word 21 25 |> Register.of_int in
  let rt = bits word 16 20 |> Register.of_int in
  let immediate = bits_abs word 0 15 in
  Itype { op; rs; rt; immediate }

let decode_rtype opcode word =
  let op = rtype_opcode_map.(opcode) in
  let rs = bits word 21 25 |> Register.of_int in
  let rt = bits word 16 20 |> Register.of_int in
  let rd = bits word 11 15 |> Register.of_int in
  let shamt = bits word 6 10 in
  Rtype { op; rs; rt; rd; shamt }

let decode_cop0 opcode word =
  let op = cop0_opcode_map.(opcode) in
  let rt = bits word 16 20 |> Register.of_int in
  let rd = bits word 11 15 |> Register.of_int in
  Cop0 { op; rt; rd }

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
      Jtype { op = jtype_opcode_map.(opcode - 2); target = bits_abs word 0 25 }
  | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 32 | 33 | 34 | 35 | 36
  | 37 | 38 | 40 | 41 | 42 | 43 | 46 ->
      decode_itype opcode word
  | 16 ->
      let opcode = bits word 21 25 in
      decode_cop0 opcode word
  | 17 -> failwith "COP1 not present" (* TODO exception *)
  | _ -> failwith (Printf.sprintf "Unknown opcode %X" opcode)
(* | _ ->
    Sdl.(log_error Log.category_application "Unknown opcode %X" opcode);
    Itype { op = Invalid; rs = Zero; rt = Zero; immediate = 0 } *)
