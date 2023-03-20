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
  | 16 -> T8
  | 17 -> T9
  | 18 -> S0
  | 19 -> S1
  | 20 -> S2
  | 21 -> S3
  | 22 -> S4
  | 23 -> S5
  | 24 -> S6
  | 25 -> S7
  | 26 -> K0
  | 27 -> K1
  | 28 -> Gp
  | 29 -> Sp
  | 30 -> Fp
  | 31 -> Ra
  | _ -> failwith (Printf.sprintf "Invalid register %d" i)
