open Util

let buffer_size = 512
let port = ref 1234
let client_socket = ref Unix.stdin (* TODO *)

let wait_for_connection () =
  let open Unix in
  let server = Unix.socket PF_INET SOCK_STREAM 0 in
  setsockopt server SO_REUSEADDR true;
  let addr = inet_addr_loopback in
  bind server (ADDR_INET (addr, !port));
  listen server 1;
  let client, _ = accept server in
  set_nonblock client;
  client_socket := client

let running = ref true
let last_message = ref ""
let pid = ref 0
let thread = ref None

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
  pid := Unix.fork ();
  if !pid = 0 then
    Sys.command
      ("alacritty -e gdb-multiarch -q -ex 'set architecture mips:3000' -ex \
        'set debug remote 1' -ex 'target remote localhost:"
     ^ string_of_int !port ^ "' -ex 'x/5i $pc' -ex 'set pagination off'")
    |> ignore
  else (
    (* let client = wait_for_connection () in *)
    wait_for_connection ();
    let channel = Unix.in_channel_of_descr !client_socket in
    Psx.state.state <- Halted;
    thread :=
      Some
        (Thread.create
           (fun _ ->
             let lexbuf = ref (Lexing.from_channel channel) in
             let open Lexer in
             while !running do
               try
                 let command = lex !lexbuf in
                 print_endline ("<- " ^ Lexing.lexeme !lexbuf);
                 match command with
                 | Acknowledge -> ()
                 | RequestRetransmission -> respond !client_socket !last_message
                 | Packet QueryHaltReason -> respond !client_socket "S05"
                 | Packet ReadGeneralRegisters ->
                     let registers = R3000.dump_registers () in
                     respond !client_socket (string_of_registers registers)
                 | Packet (ReadMemory { addr; length }) ->
                     let memory =
                       List.init length (fun i -> Bus.read_u8 (addr + i))
                     in
                     respond !client_socket (string_of_memory memory)
                 | Packet (WriteMemory { addr; length; data }) ->
                     (* let memory = List.init length (fun i -> Bus.read_u8 (addr + i)) in
                        respond client (string_of_memory memory) *)
                     respond !client_socket "E00"
                 | Packet (QSupported _features) ->
                     respond !client_socket "swbreak+;"
                 | Packet Kill ->
                     respond !client_socket "";
                     running := false;
                     Psx.state.running <- false
                 (* | Packet (Step None) ->
                     R3000.fetch_decode_execute () |> ignore;
                     respond client "S05" *)
                 (* | Packet (Step (Some addr)) -> () *)
                 | Packet (InsertSwBreak { addr; _ }) ->
                     Psx.state.breakpoints <- addr :: Psx.state.breakpoints;
                     respond !client_socket "OK"
                 | Packet (RemoveSwBreak { addr; _ }) ->
                     Psx.state.breakpoints <-
                       List.filter
                         (fun breakpoint -> breakpoint <> addr)
                         Psx.state.breakpoints;
                     respond !client_socket "OK"
                 | Packet Continue ->
                     Psx.state.state <- Running;
                     while Psx.state.state <> Breakpoint do
                       Unix.sleepf 0.5
                     done;
                     respond !client_socket "S05";
                     Unix.send !client_socket (Bytes.of_string "+") 0 1 []
                     |> ignore
                 | _ -> respond !client_socket ""
               with Sys_blocked_io ->
                 Unix.select [ !client_socket ] [] [] 0.1 |> ignore
             done)
           ()))

let disconnect () =
  running := false;
  Thread.join (Option.get !thread);
  Unix.(
    shutdown !client_socket SHUTDOWN_ALL;
    close !client_socket;
    Sys.command "pkill gdb-multiarch" |> ignore;
    waitpid [] !pid |> ignore)
