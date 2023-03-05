open Bigarray

let w = 640
let h = 480
let vram = Array1.init Int32 C_layout (w * h) (fun _ -> 0l)
(* let clear () = Array1.fill vram 0x000000FFl
let read y x = Bigarray.Array1.get vram ((y * w) + x) |> Int32.to_int
let write y x data = Bigarray.Array1.set vram ((y * w) + x) data *)
