open Bigarray
open Tsdl
open Util

let page_size = 64 * 1024 / 4

let page_table_r =
  let page_table = Array.make 0x10000 None in
  for idx = 0 to 127 do
    let pointer =
      (* 0x7FFFF *)
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

let page_table_w_normal =
  let page_table = Array.make 0x10000 None in
  for idx = 0 to 127 do
    let pointer =
      Array1.sub Ram.ram (idx * page_size land (0x1FFFFF / 4)) page_size
    in
    Array.set page_table (idx + 0x0000) (Some pointer);
    Array.set page_table (idx + 0x8000) (Some pointer);
    Array.set page_table (idx + 0xA000) (Some pointer)
  done;
  page_table

let page_table_w_with_cache_isolation :
    (int, int_elt, c_layout) Array1.t option array =
  Array.make 0x10000 None

let page_table_w = ref page_table_w_normal

let read_slow addr reader =
  let unmirrored = addr land 0x1FFFFFFF in
  if unmirrored >= 0x1F800000 && unmirrored < 0x1F800400 then (
    let offset = addr land 0xFFF in
    Sdl.(
      log_debug Log.category_application "READ scratchpad at address #%d" addr);
    reader Scratchpad.data offset)
  else if unmirrored >= 0x1F801000 && unmirrored < 0x1F802000 then (
    Sdl.(
      log_debug Log.category_application
        "READ from I/O port at %X (unimplemented)" addr);
    0)
  else failwithf "Unknown address %X" addr

let write_slow addr data writer =
  let unmirrored = addr land 0x1FFFFFFF in
  if unmirrored >= 0x1F800000 && unmirrored < 0x1F800400 then (
    let offset = addr land 0xFFF in
    Sdl.(
      log_debug Log.category_application
        "WRITE value %X to scratchpad at address #%d" data addr);
    writer Scratchpad.data offset data)
  else if unmirrored >= 0x1F801000 && unmirrored < 0x1F802000 then
    Sdl.(
      log_debug Log.category_application
        "WRITE value %X to I/O port at %X (unimplemented)" data addr)
  else if addr = 0xFFFE0130 then
    if data = 0x1E988 then page_table_w := page_table_w_normal
    else page_table_w := page_table_w_with_cache_isolation
  else failwithf "Unknown address %X" addr

let read addr reader =
  let page = addr lsr 16 in
  let offset = addr land 0xFFFF in
  let pointer = page_table_r.(page) in
  match pointer with
  | None ->
      let value = read_slow addr reader in
      Sdl.(log_debug Log.category_application "Value there %X" value);
      value
  | Some pointer ->
      Sdl.(
        log_debug Log.category_application "READ 0x%X - Page #%d - Offset #%d"
          addr page offset);
      let value = reader pointer offset in
      Sdl.(log_debug Log.category_application "Value there %X" value);
      value

let write addr data writer =
  let page = addr lsr 16 in
  let offset = addr land 0xFFFF in
  let pointer = !page_table_w.(page) in
  match pointer with
  | None -> write_slow addr data writer
  | Some pointer ->
      Sdl.(
        log_debug Log.category_application
          "WRITE value %X to address 0x%X - Page #%d - Offset #%d" data addr
          page offset);
      writer pointer offset data

let read_u32 addr = read addr Mem.read_u32
let read_u8 addr = read addr Mem.read_u8
let write_u32 addr data = write addr data Mem.write_u32
