open Bigarray

let bios_size = 512 * 1024 / 4
let rom = Array1.create Bigarray.int Bigarray.C_layout bios_size

let load bios =
  for i = 0 to bios_size - 1 do
    Array1.set rom i (Array1.get bios i |> Int32.to_int)
  done

let read_u32 = Mem.read_u32 rom
let read_u8 = Mem.read_u8 rom
