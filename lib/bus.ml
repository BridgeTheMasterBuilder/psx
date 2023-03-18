let read_u32 addr =
  match addr with
            (* TODO *)
            | addr when addr >= 0xBFC00000 -> let addr = addr - 0xBFC00000 in Bios.read_u32 addr
            | _ -> failwith (Printf.sprintf "Unknown address %X" addr)
