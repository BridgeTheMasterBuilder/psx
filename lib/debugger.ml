open Util

type state = {
  buffer_size : int;
  mutable port : int;
  mutable client : Unix.file_descr option;
  mutable running : bool;
  mutable last_message : string;
  mutable pid : int;
  mutable thread : Thread.t option;
}

let state =
  {
    buffer_size = 512;
    port = 1234;
    client = None;
    running = true;
    last_message = "";
    pid = 0;
    thread = None;
  }

let wait_for_connection () =
  let open Unix in
  let server = Unix.socket PF_INET SOCK_STREAM 0 in
  setsockopt server SO_REUSEADDR true;
  let addr = inet_addr_loopback in
  bind server (ADDR_INET (addr, state.port));
  listen server 1;
  let client, _ = accept server in
  set_nonblock client;
  state.client <- Some client

let respond client data =
  state.last_message <- data;
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
  state.pid <- Unix.fork ();
  if state.pid = 0 then
    Unix.execvp "gdb-multiarch"
      [|
        "gdb-multiarch";
        "-q";
        "-ex";
        "set architecture mips:3000";
        "-ex";
        "set debug remote 1";
        "-ex";
        "target remote localhost:" ^ string_of_int state.port;
        "-ex";
        "x/5i $pc";
        "-ex";
        "set pagination off";
      |]
    |> ignore
  else (* let client = wait_for_connection () in *)
    wait_for_connection ();
  let client = Option.get state.client in
  let channel = Unix.in_channel_of_descr client in
  Psx.state.state <- Halted;
  state.thread <-
    Some
      (Thread.create
         (fun _ ->
           let lexbuf = ref (Lexing.from_channel channel) in
           let open Lexer in
           while state.running do
             try
               let command = lex !lexbuf in
               print_endline ("<- " ^ Lexing.lexeme !lexbuf);
               match command with
               | Acknowledge -> ()
               | RequestRetransmission -> respond client state.last_message
               | Packet QueryHaltReason -> respond client "S05"
               | Packet ReadGeneralRegisters ->
                   let registers = R3000.dump_registers () in
                   respond client (string_of_registers registers)
               | Packet (ReadMemory { addr; length }) ->
                   let memory =
                     List.init length (fun i -> Bus.read_u8 (addr + i))
                   in
                   respond client (string_of_memory memory)
               | Packet (WriteMemory { addr; length; data }) ->
                   (* let memory = List.init length (fun i -> Bus.read_u8 (addr + i)) in
                      respond client (string_of_memory memory) *)
                   respond client "E00"
               | Packet (QSupported _features) -> respond client "swbreak+;"
               | Packet Kill ->
                   respond client "";
                   state.running <- false;
                   Psx.state.running <- false
               (* | Packet (Step None) ->
                   R3000.fetch_decode_execute () |> ignore;
                   respond client "S05" *)
               (* | Packet (Step (Some addr)) -> () *)
               | Packet (InsertSwBreak { addr; _ }) ->
                   Psx.state.breakpoints <- addr :: Psx.state.breakpoints;
                   respond client "OK"
               | Packet (RemoveSwBreak { addr; _ }) ->
                   Psx.state.breakpoints <-
                     List.filter
                       (fun breakpoint -> breakpoint <> addr)
                       Psx.state.breakpoints;
                   respond client "OK"
               | Packet Continue ->
                   Psx.state.state <- Running;
                   while Psx.state.state <> Breakpoint do
                     Unix.sleepf 0.5
                   done;
                   respond client "S05";
                   Unix.send client (Bytes.of_string "+") 0 1 [] |> ignore
               | _ -> respond client ""
             with Sys_blocked_io -> Unix.select [ client ] [] [] 0.1 |> ignore
           done)
         ())

let disconnect () =
  state.running <- false;
  Thread.join (Option.get state.thread);
  let client = Option.get state.client in
  Unix.(
    shutdown client SHUTDOWN_ALL;
    close client;
    kill state.pid Sys.sigterm |> ignore;
    waitpid [] state.pid |> ignore)
