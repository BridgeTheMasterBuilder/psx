let itype_opcode_map =
  [|
    "bltz";
    "bgez";
    "invalid";
    "invalid";
    "beq";
    "bne";
    "blez";
    "bgtz";
    "addi";
    "addiu";
    "subi";
    "subiu";
    "andi";
    "ori";
    "xori";
    "lui";
    "bltzal";
    "bgezal";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "lb";
    "lh";
    "lwl";
    "lw";
    "lbu";
    "lhu";
    "lwr";
    "invalid";
    "sb";
    "sh";
    "swl";
    "sw";
    "invalid";
    "invalid";
    "swr";
  |]

let jtype_opcode_map = [| "invalid"; "invalid"; "j"; "jal" |]

let rtype_opcode_map =
  [|
    "sll";
    "invalid";
    "invalid";
    "sra";
    "sllv";
    "srlv";
    "invalid";
    "srav";
    "jr";
    "jalr";
    "invalid";
    "invalid";
    "syscall";
    "break";
    "invalid";
    "invalid";
    "mfhi";
    "mthi";
    "mflo";
    "mtlo";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "mult";
    "multu";
    "div";
    "divu";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "add";
    "addu";
    "sub";
    "subu";
    "and";
    "or";
    "xor";
    "nor";
    "invalid";
    "invalid";
    "slt";
    "sltu";
  |]

let cop0_opcode_map =
  [|
    "mfc0";
    "invalid";
    "invalid";
    "invalid";
    "mtc0";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "invalid";
    "rfe";
  |]

type insn =
  | Itype of {
      op : Uint32.t;
      rs : Uint32.t;
      rt : Uint32.t;
      immediate : Uint32.t;
    }
  | Jtype of { op : Uint32.t; target : Uint32.t }
  | Rtype of {
      op : Uint32.t;
      rs : Uint32.t;
      rt : Uint32.t;
      rd : Uint32.t;
      shamt : Uint32.t;
    }
  | Cop0 of { op : Uint32.t; rt : Uint32.t; rd : Uint32.t }
