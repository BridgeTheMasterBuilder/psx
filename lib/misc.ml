open Tsdl

let my_assert lhs rhs =
  if not (lhs = rhs) then (
    Printf.eprintf "Assertion failed: %X <> %X\n" lhs rhs;
    assert false)

let my_assert_either lhs rhs1 rhs2 =
  if not (lhs = rhs1 || lhs = rhs2) then (
    Printf.eprintf "Assertion failed: %X <> %X && %X <> %X\n" lhs rhs1 lhs rhs2;
    assert false)

let quiet f =
  let log_level = Sdl.(log_get_priority Log.category_application) in
  Sdl.(log_set_priority Log.category_application Log.priority_critical);
  f ();
  Sdl.(log_set_priority Log.category_application log_level)
