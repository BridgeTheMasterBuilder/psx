(* open Util *)
open Misc

(* TODO unaligned *)
let read_u32 mem addr =
  let addr = Uint32.to_int addr lsr 2 in
  mem.{addr} |> Uint32.of_int

(* TODO mask *)
let read_u16 mem addr =
  my_assert (Uint32.to_int addr land 0x1) 0;
  let open Uint32 in
  let bit_offset = addr land 1u * 16u in
  let word = read_u32 mem addr in
  bits word bit_offset (bit_offset + 15u)

(* TODO mask *)
let read_u8 mem addr =
  let open Uint32 in
  let bit_offset = addr land 3u * 8u in
  let word = read_u32 mem addr in
  bits word bit_offset (bit_offset + 7u)

(* TODO write queue? *)
let write_u32 mem addr data =
  my_assert (Uint32.to_int addr land 0x3) 0;
  let addr = Uint32.to_int addr lsr 2 in
  mem.{addr} <- data

let write_u16 mem addr data =
  my_assert (addr land 0x1) 0;
  let bit_offset = addr land 1 * 16 in
  let word = read_u32 mem addr in
  let mask = (0xFFFF lsl bit_offset) lxor 0xFFFFFFFF in
  let data = word land mask lor (data lsl bit_offset) in
  let addr = addr lsr 2 in
  mem.{addr} <- data land 0xFFFFFFFF

let write_u8 mem addr data =
  let bit_offset = addr land 3 * 8 in
  let word = read_u32 mem addr in
  let mask = (0xFF lsl bit_offset) lxor 0xFFFFFFFF in
  let data = word land mask lor (data lsl bit_offset) in
  let addr = addr lsr 2 in
  mem.{addr} <- data land 0xFFFFFFFF
