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

let init debug =
  if debug then
    let client = wait_for_connection () in
    let inbuf = Bytes.create buffer_size in
    let outbuf = Bytes.of_string "+" in
    Thread.create
      (fun _ ->
        let received = Unix.recv client inbuf 0 512 [] in
        while received > 0 do
          print_endline (Bytes.sub_string inbuf 0 received);
          Unix.send client outbuf 0 1 [] |> ignore;
          Unix.sleepf 0.5
        done)
      ()
    |> ignore
