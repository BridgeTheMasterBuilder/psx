open Tsdl
open Util

(* let run () = R3000.fetch_decode_execute () *)
type state = Running

let running = ref true
let terminate () = raise_notrace Exit
let state = ref Running

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

let run renderer framebuffer =
  if not !running then terminate ();
  (* let frame_start = Unix.gettimeofday () in *)
  handle_input ();
  render renderer framebuffer;
  (* Psx.(match run () with _ -> ()); *)
  (* let frame_end = Unix.gettimeofday () in *)
  ()
(* Sdl.(
   log_debug Log.category_application "%f fps\n"
     (1.0 /. (frame_end -. frame_start))) *)
