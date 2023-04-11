open Cmdliner
open Psx_lib

let set_port port = Debugger.port := port

let main port =
  set_port port;
  let renderer, framebuffer = Emu.init () in
  try
    while true do
      Emu.run renderer framebuffer
    done
  with Exit -> Debugger.disconnect ()

let parse_args () =
  (* let rom = Arg.(required & pos 0 (some string) None & info [] ~docv:"ISO") in *)
  let port =
    Arg.(value & opt int 1234 & info [ "p"; "port" ] ~docv:"PORT" ~doc:"")
  in
  let info = Cmd.info "psx" in
  (* let cmd = Cmd.v info Term.(const Psx.main $ iso) in *)
  let cmd = Cmd.v info Term.(const main $ port) in
  exit (Cmd.eval cmd)

let () = parse_args ()
