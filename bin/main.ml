open Cmdliner
open Psx_lib

let main =
  let renderer, framebuffer = Emu.init () in
  try
    while true do
      Emu.run renderer framebuffer
    done
  with Exit -> 
    Debugger.running := false

let parse_args () =
  (* let rom = Arg.(required & pos 0 (some string) None & info [] ~docv:"ISO") in *)
  let info = Cmd.info "psx" in
  (* let cmd = Cmd.v info Term.(const Psx.main $ iso) in *)
  let cmd = Cmd.v info Term.(const main) in
  exit (Cmd.eval cmd)

let () = parse_args ()
