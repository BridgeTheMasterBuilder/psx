(* let run () = R3000.fetch_decode_execute () *)
let running = ref true
let terminate () = raise_notrace Exit

let run () =
  if not !running then terminate ();
  R3000.Running
