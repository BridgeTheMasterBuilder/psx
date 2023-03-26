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

let calculate_checksum payload =
  String.fold_left (fun sum char -> sum + Char.code char) 0 payload mod 256

let init debug =
  if debug then
    let client = wait_for_connection () in
    let inbuf = Bytes.create buffer_size in
    let outbuf = Bytes.of_string "+$#00" in
    Thread.create
      (fun _ ->
        let received = ref (Unix.recv client inbuf 0 512 []) in
        while !received > 0 do
          let packet = Bytes.sub_string inbuf 0 !received in
          print_endline packet;
          if packet = "+$?#3f" then
            Unix.send client
              (Bytes.of_string
                 (Printf.sprintf "+$S05#%2x" (calculate_checksum "S05")))
              0 8 []
            |> ignore
          else if packet = "+$g#67" then
            let regs =
              Printf.sprintf
                "+$000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000C0BF000000000000000000000000#%2x"
                (calculate_checksum "0000C0BF")
            in
            Unix.send client (Bytes.of_string regs) 0
              (2 + (32 * 8) + (6 * 8) + (3 * 8) + 3)
              []
            |> ignore
          else Unix.send client outbuf 0 5 [] |> ignore;
          Unix.sleepf 0.5;
          received := Unix.recv client inbuf 0 512 []
        done)
      ()
    |> ignore
