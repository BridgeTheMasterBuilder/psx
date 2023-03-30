open Util

let buffer_size = 512

let wait_for_connection () =
  let open Unix in
  let server = Unix.socket PF_INET SOCK_STREAM 0 in
  let addr = inet_addr_loopback in
  bind server (ADDR_INET (addr, 1234));
  listen server 1;
  let client, _ = accept server in
  client

let running = ref true
let last_message = ref ""

let respond client data =
  last_message := data;
  let message =
    Printf.sprintf "+$%s#%02x" data (Lexer.calculate_checksum data)
  in
  print_endline ("-> " ^ message);
  Unix.send client (Bytes.of_string message) 0 (String.length message) []
  |> ignore

let string_of_registers registers =
  List.fold_left
    (fun accum reg ->
      accum
      ^ Printf.sprintf "%02x%02x%02x%02x" (bits_abs reg 0 7) (bits reg 8 15)
          (bits reg 16 23) (bits reg 24 31))
    "" registers

let string_of_memory bytes =
  List.fold_left (fun accum byte -> accum ^ Printf.sprintf "%02x" byte) "" bytes

let connect () =
  let client = wait_for_connection () in
  let channel = Unix.in_channel_of_descr client in
  Thread.create
    (fun _ ->
      let lexbuf = ref (Lexing.from_channel channel) in
      let open Lexer in
      while !running do
        let command = lex !lexbuf in
        print_endline ("<- " ^ Lexing.lexeme !lexbuf);
        match command with
        | Acknowledge -> ()
        | RequestRetransmission -> respond client !last_message
        | Packet QueryHaltReason -> respond client "S05"
        | Packet ReadGeneralRegisters ->
            let registers = R3000.dump_registers () in
            respond client (string_of_registers registers)
        | Packet (ReadMemory (addr, length)) ->
            let memory = List.init length (fun i -> Bus.read_u8 (addr + i)) in
            respond client (string_of_memory memory)
        | Packet (QSupported _features) -> respond client ""
        | _ -> respond client ""
      done)
    ()
  |> ignore
