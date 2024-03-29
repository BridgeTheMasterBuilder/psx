open Tsdl
open Util

let terminate () = raise_notrace Exit

type t = { mutable running : bool; mutable cyc : int; mutable cpi : int }

let state = { running = true; cyc = 0; cpi = 20 }

exception Break

let handle_input () =
  let event = Sdl.Event.create () in
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
      else raise_notrace Break
    done
  with
  | Break -> ()
  | Exit -> raise_notrace Exit

let render renderer framebuffer =
  Sdl.update_texture framebuffer None Vram.vram Vram.w |> unwrap |> ignore;
  Sdl.render_copy renderer framebuffer |> unwrap |> ignore;
  Sdl.render_present renderer

(* TODO debug vs non-debug versions *)
let update () =
  (* TODO reset cyc *)
  (try
     while R3000.state.state = Running && state.cyc < R3000.clockrate / 60 do
       let pc = R3000.pc () in
       R3000.check_for_breakpoint pc;
       if R3000.state.state <> Running then raise_notrace Break;
       R3000.fetch_decode_execute ();
       state.cyc <- state.cyc + state.cpi;
       (* Sdl.(log_debug Log.category_application "Cycles: %d\n" state.cyc) *)
       ()
     done
   with Break -> ());
  if state.cyc = R3000.clockrate / 60 then state.cyc <- 0 (* TODO *)

let run renderer framebuffer =
  if not state.running then terminate ();
  (* let frame_start = Unix.gettimeofday () in *)
  handle_input ();
  update ();
  render renderer framebuffer;
  (* Psx.(match run () with _ -> ()); *)
  (* let frame_end = Unix.gettimeofday () in *)
  ()
(* Sdl.(
   log_critical Log.category_application "%f fps\n"
     (1.0 /. (frame_end -. frame_start))) *)
