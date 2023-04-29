type t =
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
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | T8
  | T9
  | K0
  | K1
  | Gp
  | Sp
  | Fp
  | Ra
[@@deriving show]

type cop0 =
  | Bpc
  | Bda
  | Jumpdest
  | Dcic
  | BadVaddr
  | Bdam
  | Bpcm
  | Sr
  | Cause
  | Epc
  | Prid
  | Invalid
[@@deriving show]

let of_int i =
  match i with
  | 0 -> Zero
  | 1 -> At
  | 2 -> V0
  | 3 -> V1
  | 4 -> A0
  | 5 -> A1
  | 6 -> A2
  | 7 -> A3
  | 8 -> T0
  | 9 -> T1
  | 10 -> T2
  | 11 -> T3
  | 12 -> T4
  | 13 -> T5
  | 14 -> T6
  | 15 -> T7
  | 16 -> S0
  | 17 -> S1
  | 18 -> S2
  | 19 -> S3
  | 20 -> S4
  | 21 -> S5
  | 22 -> S6
  | 23 -> S7
  | 24 -> T8
  | 25 -> T9
  | 26 -> K0
  | 27 -> K1
  | 28 -> Gp
  | 29 -> Sp
  | 30 -> Fp
  | 31 -> Ra
  | _ -> failwith (Printf.sprintf "Invalid register %d" i)

let cop0_of_int i =
  match i with
  | 3 -> Bpc
  | 5 -> Bda
  | 6 -> Jumpdest
  | 7 -> Dcic
  | 8 -> BadVaddr
  | 9 -> Bdam
  | 11 -> Bpcm
  | 12 -> Sr
  | 13 -> Cause
  | 14 -> Epc
  | 15 -> Prid
  | _ -> failwith (Printf.sprintf "Invalid COP0 register %d" i)
