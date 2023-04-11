open Tsdl
open Util

type state = Running | Halted | Breakpoint

let terminate () = raise_notrace Exit

type t = {
  mutable state : state;
  mutable running : bool;
  mutable breakpoints : int list;
  mutable cyc : int;
}

let state = { running = true; state = Running; breakpoints = []; cyc = 0 }

let handle_input () =
  let event = Sdl.Event.create () in
  let exception Continue in
  try
    while true do
      if Sdl.poll_event (Some event) then
        match Sdl.Event.(get event typ |> enum) with
        | `Quit -> terminate ()
        | `Key_down -> (
            let key =
              Sdl.Event.(get event keyboard_scancode |> Sdl.Scancode.enum)
            in
            match key with `Escape -> terminate () | _ -> ())
        | _ -> ()
      else raise_notrace Continue
    done
  with
  | Continue -> ()
  | Exit -> raise_notrace Exit

let render renderer framebuffer =
  Sdl.update_texture framebuffer None Vram.vram Vram.w |> unwrap |> ignore;
  Sdl.render_copy renderer framebuffer |> unwrap |> ignore;
  Sdl.render_present renderer

let update () =
  while state.state = Running && state.cyc < R3000.clockrate do
    R3000.fetch_decode_execute ();
    state.cyc <- state.cyc + 2
  done

(* TODO debug vs non-debug versions *)
let run renderer framebuffer =
  if not state.running then terminate ();
  (* let frame_start = Unix.gettimeofday () in *)
  let pc = R3000.pc () in
  handle_input ();
  if List.mem pc state.breakpoints then state.state <- Breakpoint;
  update ();
  render renderer framebuffer;
  (* Psx.(match run () with _ -> ()); *)
  (* let frame_end = Unix.gettimeofday () in *)
  ()
(* Sdl.(
   log_debug Log.category_application "%f fps\n"
     (1.0 /. (frame_end -. frame_start))) *)
