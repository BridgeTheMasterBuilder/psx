open Bigarray

let ram = Array1.create int Bigarray.C_layout (2 * 1024 * 1024 / 4)
