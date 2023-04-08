open Bigarray
open Tsdl

let page_size = 64 * 1024 / 4

let page_table_r =
  let page_table = Array.make 0x10000 None in
  for idx = 0 to 127 do
    let pointer =
      Array1.sub Ram.ram (idx * page_size land (0x1FFFFF / 4)) page_size
    in
    Array.set page_table (idx + 0x0000) (Some pointer);
    Array.set page_table (idx + 0x8000) (Some pointer);
    Array.set page_table (idx + 0xA000) (Some pointer)
  done;
  for idx = 0 to 7 do
    let pointer = Array1.sub Bios.rom (idx * page_size) page_size in
    Array.set page_table (idx + 0x1FC0) (Some pointer);
    Array.set page_table (idx + 0x9FC0) (Some pointer);
    Array.set page_table (idx + 0xBFC0) (Some pointer)
  done;
  page_table
(* let page_table_w = Array.make 0x10000 None *)

let read addr reader =
  let page = addr lsr 16 in
  let offset = addr land 0xFFFF in
  let pointer = page_table_r.(page) in
  Sdl.(
    log_debug Log.category_application "READ 0x%X - Page #%d - Offset #%d" addr
      page offset);
  match pointer with
  | None -> 0 (* TODO slow page *)
  | Some pointer ->
      let value = reader pointer offset in
      Sdl.(log_debug Log.category_application "Value there %X" value);
      value

let read_u32 addr = read addr Mem.read_u32
let read_u8 addr = read addr Mem.read_u8
