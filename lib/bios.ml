open Bigarray

let rom =
  Array1.create Bigarray.Int32 Bigarray.C_layout ((512 * 1024) / 4)

let load bios =
  Array1.blit bios rom

let read_u32 addr = 
  (Array1.get rom addr |> Int32.to_int) land 0xFFFFFFFF