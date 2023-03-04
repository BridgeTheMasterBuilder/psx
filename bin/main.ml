open Cmdliner

let parse_args () =
  (* let rom = Arg.(required & pos 0 (some string) None & info [] ~docv:"ISO") in *)
  let info = Cmd.info "psx" in
  (* let cmd = Cmd.v info Term.(const Psx.main $ iso) in *)
  let cmd = Cmd.v info Term.(const Psx.main $ const ()) in
  exit (Cmd.eval cmd)

let () = parse_args ()
