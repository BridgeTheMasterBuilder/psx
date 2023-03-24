open Tsdl
open Util

(* open Containers *)
open Psx_lib

let bios_path = "/home/master/projects/psx/SCPH1001.BIN"
let w = 640
let h = 480
let debug = true

let map_file_array1 path =
  CCIO.(
    with_in path (fun ic ->
        let fd = Unix.descr_of_in_channel ic in
        let open Bigarray in
        Unix.map_file fd Int32 C_layout false [| -1 |] |> array1_of_genarray))

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
    Psx.(match run () with _ -> ());
    Sdl.update_texture framebuffer None Vram.vram Vram.w |> unwrap |> ignore;
    Sdl.render_copy renderer framebuffer |> unwrap |> ignore;
    Sdl.render_present renderer;
    let frame_end = Unix.gettimeofday () in
    ()
    (* Sdl.(
       log_debug Log.category_application "%f fps\n"
         (1.0 /. (frame_end -. frame_start))) *)
  done

let wait_for_debugger () =
  let open Unix in
  let server = Unix.socket PF_INET SOCK_STREAM 0 in
  let addr = inet_addr_loopback in
  bind server (ADDR_INET (addr, 1234));
  listen server 1;
  let client, _ = accept server in
  client

let main =
  try
    Sdl.init Sdl.Init.video |> unwrap |> ignore;
    at_exit (fun () -> Sdl.quit ());
    Sdl.(log_set_priority Log.category_application Log.priority_debug);
    let x = Sdl.Window.pos_centered in
    let y = Sdl.Window.pos_centered in
    let window =
      Sdl.create_window "PlayStation Emulator" ~x ~y ~w ~h Sdl.Window.shown
      |> unwrap
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

    let bios = map_file_array1 bios_path in
    Bios.load bios;
    (* TODO debugger module, server in another thread *)
    let client =
      if debug then (
        let client = wait_for_debugger () in
        let inbuf = Bytes.create 512 in
        let outbuf = Bytes.of_string "+" in
        let received = Unix.recv client inbuf 0 512 [] in
        if received > 0 then (
          print_endline (Bytes.sub_string inbuf 0 received);
          Unix.send client outbuf 0 1 [] |> ignore);
        Some client)
      else None
    in
    run renderer texture
  with
  | SdlError e ->
      Sdl.log_error Sdl.Log.category_application "%s" e;
      exit 1
  | Exit -> ()
