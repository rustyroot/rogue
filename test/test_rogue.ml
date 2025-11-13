open Roguelib
open World
open Entity
open Light

let _ = descrete_ray_cast (0, 0) (0, 4)
let _ = descrete_ray_cast (0, 0) (5, 0)
let _ = descrete_ray_cast (0, 0) (5, 3)
let _ = descrete_ray_cast (5, 3) (0, 0)
let _ = descrete_ray_cast (2, 0) (3, 3)
let _ = descrete_ray_cast (0, 3) (5, 0)


(*let lives = ref 2
let level_number = ref 0
let level_activated = true*)
let () = override_world [|
  [|Key   ;Camel ;Empty ;Empty ;Empty|];
  [|Snake ;Empty ;Empty ;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|];
  [|Camel ;Empty ;Spider;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|]|];;

assert (world.(3).(2) = Spider);;
assert (world.(3).(1) = Empty);;
assert (move (3, 2) (3, 1) = (3, 1));;
assert (world.(3).(2) = Empty);;
assert (world.(3).(1) = Spider)







