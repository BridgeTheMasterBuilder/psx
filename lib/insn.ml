type register =
  | Zero
  | At
  | V0
  | V1
  | A0
  | A1
  | A2
  | A3
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | T7
  | T8
  | T9
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | K0
  | K1
  | Gp
  | Sp
  | Fp
  | Ra
[@@deriving show]

type opcode = Lui [@@deriving show]

type itype = { op : opcode; rs : register; rt : register; immediate : int }
[@@deriving show]

type insn = itype

let decode word =
  let opcode = word lsr 26 in
  match opcode with
  | 0xF -> { op = Lui; rs = Zero; rt = Zero; immediate = 0 }
  | _ -> failwith (Printf.sprintf "Unknown opcode %X" opcode)
