type t = int

let int_as_uint32 x = x
let of_int x = x land 0xFFFFFFFF
let to_int x = x
let ( land ) x y = x land y
let ( lor ) x y = x lor y
let ( lxor ) x y = x lxor y
let ( lsl ) x y = x lsl y
let ( lsr ) x y = x lsr y
let ( + ) x y = (x + y) land of_int 0xFFFFFFFF
let ( / ) x y = x / y
let ( * ) x y = x * y land of_int 0xFFFFFFFF

let mask n m =
  let mask_m = (1 lsl (m + 1)) - 1 in
  let mask_n = (1 lsl n) - 1 in
  mask_m land lnot mask_n

let bit data n =
  let mask = 1 lsl n in
  (data land mask) lsr n

let bits data n m =
  let mask = mask n m in
  (data land mask) lsr n

let bits_abs data n m =
  let mask = mask n m in
  data land mask
