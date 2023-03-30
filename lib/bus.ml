(* TODO fastmem? *)
let read addr bios_read =
  match addr with
  (* TODO *)
  | addr when addr >= 0xBFC00000 ->
      let addr = addr - 0xBFC00000 in
      bios_read addr
  (* | _ -> failwith (Printf.sprintf "Unknown address %X" addr) *)
  (* TODO *)
  | _ -> 0

let read_u32 addr = read addr Bios.read_u32
let read_u8 addr = read addr Bios.read_u8
