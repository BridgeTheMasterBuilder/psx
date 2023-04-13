open Util

(* TODO unaligned *)
let read_u32 mem addr =
  let addr = addr lsr 2 in
  mem.{addr} land 0xFFFFFFFF

let read_u8 mem addr =
  let bit_offset = addr land 3 * 8 in
  let word = read_u32 mem addr in
  bits word bit_offset (bit_offset + 7)

(* TODO write queue? *)
let write_u32 mem addr data =
  let addr = addr lsr 2 in
  mem.{addr} <- data land 0xFFFFFFFF
