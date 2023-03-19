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

type itype = {
  op : mnemonic;
  rs : Register.t;
  rt : Register.t;
  immediate : int;
}
[@@deriving show]

type insn = itype

let itype_opcode_map =
  [|
    Invalid;
    Invalid;
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

let decode_itype opcode word =
  let op = itype_opcode_map.(opcode) in
  let rs = (word land 0x03E00000) lsr 21 |> Register.of_int in
  let rt = (word land 0x001F0000) lsr 16 |> Register.of_int in
  let immediate = word land 0xFFFF in
  { op; rs; rt; immediate }

let decode word =
  let opcode = word lsr 26 in
  match opcode with
  (* | 0 -> ()
     | 1 -> ()
     | 2 -> ()
     | 3 -> () *)
  | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 32 | 33 | 34 | 35 | 36
  | 37 | 38 | 40 | 41 | 42 | 43 | 46 ->
      decode_itype opcode word
  | _ -> failwith (Printf.sprintf "Unknown opcode %X" opcode)
