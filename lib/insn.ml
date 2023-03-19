(* open Tsdl *)
open Util

type mnemonic =
  | Sll
  | Srl
  | Sra
  | Sllv
  | Srlv
  | Srav
  | Jr
  | Jalr
  | Syscall
  | Break
  | Mfhi
  | Mthi
  | Mflo
  | Mtlo
  | Mult
  | Multu
  | Div
  | Divu
  | Add
  | Addu
  | Sub
  | Subu
  | And
  | Or
  | Xor
  | Nor
  | Slt
  | Sltu
  | Bltz
  | Bgez
  | Bltzal
  | Bgezal
  | J
  | Jal
  | Beq
  | Bne
  | Blez
  | Bgtz
  | Addi
  | Addiu
  | Subi
  | Subiu
  | Andi
  | Ori
  | Xori
  | Lui
  | Lb
  | Lh
  | Lwl
  | Lw
  | Lbu
  | Lhu
  | Lwr
  | Sb
  | Sh
  | Swl
  | Sw
  | Swr
  | Invalid
[@@deriving show]

type insn =
  | Itype of {
      op : mnemonic;
      rs : Register.t;
      rt : Register.t;
      immediate : int;
    }
  | Jtype of { op : mnemonic; target : int }
  | Rtype of {
      op : mnemonic;
      rs : Register.t;
      rt : Register.t;
      rd : Register.t;
      shamt : int;
    }
[@@deriving show]

let itype_opcode_map =
  [|
    Bltz;
    Bgez;
    Invalid;
    Invalid;
    Beq;
    Bne;
    Blez;
    Bgtz;
    Addi;
    Addiu;
    Subi;
    Subiu;
    Andi;
    Ori;
    Xori;
    Lui;
    Bltzal;
    Bgezal;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Lb;
    Lh;
    Lwl;
    Lw;
    Lbu;
    Lhu;
    Lwr;
    Invalid;
    Sb;
    Sh;
    Swl;
    Sw;
    Invalid;
    Invalid;
    Swr;
  |]

let jtype_opcode_map = [| J; Jal |]

let rtype_opcode_map =
  [|
    Sll;
    Invalid;
    Sra;
    Sllv;
    Invalid;
    Srlv;
    Invalid;
    Srav;
    Jr;
    Jalr;
    Invalid;
    Invalid;
    Syscall;
    Break;
    Invalid;
    Invalid;
    Mfhi;
    Mthi;
    Mflo;
    Mtlo;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Mult;
    Multu;
    Div;
    Divu;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Add;
    Addu;
    Sub;
    Subu;
    And;
    Or;
    Xor;
    Nor;
    Invalid;
    Invalid;
    Slt;
    Sltu;
  |]

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
  | _ -> failwith (Printf.sprintf "Unknown opcode %X" opcode)
(* | _ ->
    Sdl.(log_error Log.category_application "Unknown opcode %X" opcode);
    Itype { op = Invalid; rs = Zero; rt = Zero; immediate = 0 } *)
