let buffer_size = 512

let wait_for_connection () =
  let open Unix in
  let server = Unix.socket PF_INET SOCK_STREAM 0 in
  let addr = inet_addr_loopback in
  bind server (ADDR_INET (addr, 1235));
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
  print_endline ("Sending " ^ message);
  Unix.send client (Bytes.of_string message) 0 (String.length message) []
  |> ignore

let connect () =
  let client = wait_for_connection () in
  let buf = Bytes.create buffer_size in
  Thread.create
    (fun _ ->
      Unix.recv client buf 0 buffer_size [] |> ignore;
      let lexbuf = ref (Lexing.from_string (Bytes.to_string buf)) in
      let open Lexer in
      while !running do
        (match lex !lexbuf with
        | RequestRetransmission -> respond client !last_message
        | Packet QueryHaltReason -> respond client "S05"
        | _ -> respond client ""
        | exception Lexer.Refill ->
            Unix.recv client buf 0 buffer_size [] |> ignore;
            lexbuf := Lexing.from_string (Bytes.to_string buf));
        Unix.sleepf 0.5
      done)
    ()
  |> ignore
