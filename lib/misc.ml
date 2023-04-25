open Tsdl

let quiet f =
  let log_level = Sdl.(log_get_priority Log.category_application) in
  Sdl.(log_set_priority Log.category_application Log.priority_critical);
  f ();
  Sdl.(log_set_priority Log.category_application log_level)
