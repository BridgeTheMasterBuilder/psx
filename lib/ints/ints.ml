module type Integer = sig
  type t

  val as_t : int -> t
  val of_int : int -> t
  val to_int : t -> int
  val ( land ) : t -> t -> t
  val ( lor ) : t -> t -> t
  val ( lxor ) : t -> t -> t
  val ( lsl ) : t -> t -> t
  val ( lsr ) : t -> t -> t
  val ( + ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( * ) : t -> t -> t
  val bit : t -> t -> t
  val bits : t -> t -> t -> t
  val bits_abs : t -> t -> t -> t
end

module Make (M : sig
  val bitmask : int
end) : Integer = struct
  type t = int

  let as_t x = x
  let of_int x = x land M.bitmask
  let to_int x = x
  let ( land ) x y = x land y
  let ( lor ) x y = x lor y
  let ( lxor ) x y = x lxor y
  let ( lsl ) x y = x lsl y
  let ( lsr ) x y = x lsr y
  let ( + ) x y = (x + y) land of_int M.bitmask
  let ( / ) x y = x / y
  let ( * ) x y = x * y land of_int M.bitmask

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
end

module Uint32 = Make (struct
  let bitmask = 0xFFFFFFFF
end)
