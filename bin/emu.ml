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

let terminate () = raise_notrace Exit
let run renderer texture = Psx.run renderer texture

let init debug =
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
    if debug then Debugger.connect ();
    (renderer, texture)
    (* run renderer texture *)
  with SdlError e ->
    Sdl.log_error Sdl.Log.category_application "%s" e;
    exit 1
