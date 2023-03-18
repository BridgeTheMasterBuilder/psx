(* open Bigarray

let ram =
  Genarray.create Bigarray.Int32 Bigarray.C_layout [|(2 * 1024 * 1024) / 4|]

let load_bios bios =
  Genarray.blit bios (Genarray.sub_left ram 0 ((512 * 1024) / 4))

let read_u32 addr = 
  (Genarray.get ram [|addr|] |> Int32.to_int) land 0xFFFFFFFF *)