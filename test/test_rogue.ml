open Roguelib
open World
open Entity
open Light

let () = override_world [|
  [|Key   ;Camel ;Empty ;Empty ;Empty|];
  [|Snake ;Empty ;Empty ;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|];
  [|Camel ;Empty ;Spider;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|]|]

(* On déplace l'araignée de 1 *)
let _ = assert (world.(3).(2) = Spider)
let _ = assert (world.(3).(1) = Empty)
let _ = assert (move (3, 2) (3, 1) = (3, 1))
let _ = assert (world.(3).(2) = Empty)
let _ = assert (world.(3).(1) = Spider)
let _ = Printf.printf "spider_move :                      OK \n"

(* L'araignée attaque le chameau du bas *)
let () = lives := 12
let _ = assert (move (3, 1) (3, 0) = (3, 1))
let _ = assert (world.(3).(1) = Spider)
let _ = assert (world.(3).(0) = Camel)
let _ = assert (!lives = 11)
let _ = Printf.printf "spider_attack :                    OK \n"

(* Le serpent essaye de récupérer la clé *)
let () = level_number := 42 
let _ = assert (move (1, 0) (0, 0) = (1, 0))
let _ = assert (world.(1).(0) = Snake)
let _ = assert (world.(0).(0) = Key)
let _ = assert (!level_number = 42)
let _ = Printf.printf "entity_key_collapse_behaviour :    OK \n"

(* Le chameau du haut essaye de récupérer la clé *)
let () = level_number := 42
let _ = assert (move (0, 1) (0, 0) = (0, 1))
let _ = assert (world.(0).(1) = Camel)
let _ = assert (world.(0).(0) = Key)
let _ = assert (!level_number = 42) (* Comportement voulu car ici level_activated = false *)
let _ = Printf.printf "camel_key_collapse_behaviour :     OK \n"

(* L'araignée tue le chameau du bas : 
   Le assert ne passe que si l'effet End_of_game est catch
   et que la position renvoyée est celle du camel *)
let () = lives := 1
let _ = assert (
  try let _ = move (3, 1) (3, 0) in false with
  | effect End_of_game pos, _ -> (pos = (3, 0))
  | _ -> false
  )
let _ = assert (world.(3).(1) = Spider)
let _ = assert (world.(3).(0) = Camel)
let _ = assert (!lives = 0)
let _ = assert (world.(3).(1) = Spider)
let _ = assert (world.(3).(0) = Camel)
let _ = assert (!lives = 0)
let _ = Printf.printf "spider_kill_camel :                OK \n"

(* Discretisation d'un segment diagonale *)
let _ = assert (
  descrete_ray_cast (0, 0) (6, 6) = 
  [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (3, 2); (3, 3); (4, 3); (4, 4); (5, 4); (5, 5); (6, 5); (6, 6)])
let _ = Printf.printf "descrete_ray_cast diag :           OK \n"

(* Discretisation d'un segment diagonale plus raide*)
let _ = assert (
  descrete_ray_cast (2, 0) (4, 6) = 
  [(2, 0); (2, 1); (3, 1); (3, 2); (3, 3); (3, 4); (4, 4); (4, 5); (4, 6)])
let _ = Printf.printf "descrete_ray_cast diag2 :          OK \n"

(* Discretisation d'un segment vertical *)
let _ = assert (
  descrete_ray_cast (0, 0) (0, 6) = 
  [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6)])
let _ = Printf.printf "descrete_ray_cast vertical :       OK \n"

(* Discretisation d'un segment horizontal *)
let _ = assert (
  descrete_ray_cast (0, 0) (6, 0) =
  [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0)])
let _ = Printf.printf "descrete_ray_cast horizontal :     OK \n"

(* Discretisation d'un point *)
let _ = assert (
  descrete_ray_cast (2, 2) (2, 2) =
  [(2, 2)])
let _ = Printf.printf "descrete_ray_cast point :          OK \n"

(* Discretisation d'un segment avec x' < x *)
let _ = assert (
  descrete_ray_cast (5, 3) (2, 4) =
  [(5, 3); (4, 3); (3, 3); (3, 4); (2, 4)])
let _ = Printf.printf "descrete_ray_cast right_to_left :  OK \n"
    
(* Cas limite ou il n'y a qu'une case vide *)
let monde_plein = Array.make_matrix width height Snake
let () = monde_plein.(12).(7) <- Empty
let () = override_world monde_plein
let () = assert(safe_random_position () = (12, 7))
let _ = Printf.printf "safe_random_position :             OK \n"
















