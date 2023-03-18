open Tsdl
open Util
(* open Containers *)
open Psx_lib

let w = 640
let h = 480

let handle_events () =
  let event = Sdl.Event.create () in
  let continue = ref true in
  while !continue do
    if Sdl.poll_event (Some event) then
      match Sdl.Event.(get event typ |> enum) with
      | `Quit -> raise Exit
      | `Key_down -> (
          let key =
            Sdl.Event.(get event keyboard_scancode |> Sdl.Scancode.enum)
          in
          match key with `Escape -> raise Exit | _ -> ())
      | _ -> ()
    else continue := false
  done

let run renderer framebuffer =
  while true do
    let frame_start = Unix.gettimeofday () in
    handle_events ();
    Psx.(
      match run () with
      | _ -> ());
    Sdl.update_texture framebuffer None Vram.vram Vram.w |> unwrap |> ignore;
    Sdl.render_copy renderer framebuffer |> unwrap |> ignore;
    Sdl.render_present renderer;
    let frame_end = Unix.gettimeofday () in
    Printf.printf "%f fps\n" (1.0 /. (frame_end -. frame_start))
  done

let main =
  try
    Sdl.init Sdl.Init.video |> unwrap |> ignore;
    at_exit (fun () -> Sdl.quit ());
    let x = Sdl.Window.pos_centered in
    let y = Sdl.Window.pos_centered in
    let window =
      Sdl.create_window "PlayStation Emulator" ~x ~y ~w ~h Sdl.Window.shown |> unwrap
    in
    at_exit (fun () -> Sdl.destroy_window window);
    let renderer =
      Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.presentvsync
      (* Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated *)
      |> unwrap
    in
    at_exit (fun () -> Sdl.destroy_renderer renderer);
    let texture =
      Sdl.create_texture renderer Sdl.Pixel.format_rgba8888
        Sdl.Texture.access_streaming ~w:Vram.w ~h:Vram.h
      |> unwrap
    in
    at_exit (fun () -> Sdl.destroy_texture texture);

    let bios =
       CCIO.(
        with_in "/home/master/projects/psx/SCPH1001.BIN" (fun ic ->
            let fd = Unix.descr_of_in_channel ic in
            Unix.map_file fd Bigarray.Int32 Bigarray.C_layout false [|(512 * 1024) / 4|]))
       in
    Ram.load_bios bios;
    run renderer texture
  with
  | SdlError e ->
    Sdl.log_error Sdl.Log.category_application "%s" e;
    exit 1
  | Exit -> ()
