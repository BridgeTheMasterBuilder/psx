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
