open Cmdliner
open Psx_lib

let set_port port = Debugger.state.port <- port

(* TODO run with debug? use exceptions to select which version *)
let main debug =
  set_port 1234;
  let renderer, framebuffer = Emu.init debug in
  try
    while true do
      Emu.run renderer framebuffer
    done
  with Exit -> if debug then Debugger.disconnect ()

let parse_args () =
  (* let rom = Arg.(required & pos 0 (some string) None & info [] ~docv:"ISO") in *)
  let debug =
    Arg.(value & flag & info [ "d"; "debug" ] ~docv:"DEBUG" ~doc:"")
  in
  let info = Cmd.info "psx" in
  (* let cmd = Cmd.v info Term.(const Psx.main $ iso) in *)
  let cmd = Cmd.v info Term.(const main $ debug) in
  exit (Cmd.eval cmd)

let () = parse_args ()
