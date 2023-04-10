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
  | Mfc0
  | Mtc0
  | Rfe
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

let jtype_opcode_map = [| Invalid; Invalid; J; Jal |]

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

let cop0_opcode_map =
  [|
    Mfc0;
    Invalid;
    Invalid;
    Invalid;
    Mtc0;
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
    Rfe;
  |]

type insn =
  | Itype of {
      op : int;
          [@printer
            fun fmt op -> fprintf fmt "%s" (show_mnemonic itype_opcode_map.(op))]
      rs : int;
          [@printer
            fun fmt reg -> fprintf fmt "%s" Register.(show (of_int reg))]
      rt : int;
          [@printer
            fun fmt reg -> fprintf fmt "%s" Register.(show (of_int reg))]
      immediate : int;
    }
  | Jtype of {
      op : int;
          [@printer
            fun fmt op -> fprintf fmt "%s" (show_mnemonic jtype_opcode_map.(op))]
      target : int;
    }
  | Rtype of {
      op : int;
          [@printer
            fun fmt op -> fprintf fmt "%s" (show_mnemonic rtype_opcode_map.(op))]
      rs : int;
          [@printer
            fun fmt reg -> fprintf fmt "%s" Register.(show (of_int reg))]
      rt : int;
          [@printer
            fun fmt reg -> fprintf fmt "%s" Register.(show (of_int reg))]
      rd : int;
          [@printer
            fun fmt reg -> fprintf fmt "%s" Register.(show (of_int reg))]
      shamt : int;
    }
  | Cop0 of {
      op : int;
          [@printer
            fun fmt op -> fprintf fmt "%s" (show_mnemonic cop0_opcode_map.(op))]
      rt : int;
          [@printer
            fun fmt reg -> fprintf fmt "%s" Register.(show (of_int reg))]
      rd : int;
          [@printer
            fun fmt reg -> fprintf fmt "%s" Register.(show (of_int reg))]
    }
[@@deriving show]
