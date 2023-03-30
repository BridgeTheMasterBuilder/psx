open Bigarray
open Util

let read_u32 mem addr = Array1.get mem addr land 0xFFFFFFFF

let read_u8 mem addr =
  let bit_offset = addr land 3 * 8 in
  let addr = addr / 4 in
  let word = read_u32 mem addr in
  bits word bit_offset (bit_offset + 7)
