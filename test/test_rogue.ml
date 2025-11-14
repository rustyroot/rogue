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




let () = override_world [|
  [|Key   ;Camel ;Empty ;Empty ;Empty|];
  [|Snake ;Empty ;Empty ;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|];
  [|Camel ;Empty ;Spider;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|]|];;

(* On déplace l'araignée de 1 *)
assert (world.(3).(2) = Spider);;
assert (world.(3).(1) = Empty);;
assert (move (3, 2) (3, 1) = (3, 1));;
assert (world.(3).(2) = Empty);;
assert (world.(3).(1) = Spider)

(* L'araignée attaque le chameau du bas *)
let () = lives := 12;;
assert (move (3, 1) (3, 0) = (3, 1));;
assert (world.(3).(1) = Spider);;
assert (world.(3).(0) = Camel);;
assert (!lives = 11)

(* Le serpent essaye de récupérer la clé *)
let () = level_number := 42;; 
assert (move (1, 0) (0, 0) = (1, 0));;
assert (world.(1).(0) = Snake);;
assert (world.(0).(0) = Key);;
assert (!level_number = 42)

(* Le chameau du haut essaye de récupérer la clé *)
let () = level_number := 42;;
assert (move (0, 1) (0, 0) = (0, 1));;
assert (world.(0).(1) = Camel);;
assert (world.(0).(0) = Key);;
assert (!level_number = 42) (* Comportement voulu car ici level_activated = false *)

(* L'araignée tue le chameau du bas : 
   Le assert ne passe que si l'effet End_of_game est catch
   et que la position renvoyée est celle du camel *)
let () = lives := 1;;
assert (
  try let _ = move (3, 1) (3, 0) in false with
  | effect End_of_game pos, _ -> (pos = (3, 0))
  | _ -> false
  );;
assert (world.(3).(1) = Spider);;
assert (world.(3).(0) = Camel);;
assert (!lives = 0);;

(* Cas limite ou il n'y a qu'une case vide *)
let monde_plein = Array.make_matrix width height Snake
let () = monde_plein.(12).(7) <- Empty
let () = override_world monde_plein
let () = assert(safe_random_position () = (12, 7))
let () = Printf.printf "safe_random_position : OK\n"















