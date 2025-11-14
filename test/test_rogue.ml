open Roguelib
open World
open Entity
open Light
open Heap
open Monkey

let _ = override_world [|
  [|Key   ;Camel ;Empty ;Empty ;Empty|];
  [|Snake ;Empty ;Empty ;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|];
  [|Camel ;Empty ;Spider;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|]|]

(* On déplace l'araignée de 1 *)
let () = Printf.printf "spider_move :                      "
let _ = assert (world.(3).(2) = Spider)
let _ = assert (world.(3).(1) = Empty)
let _ = assert (move (3, 2) (3, 1) = (3, 1))
let _ = assert (world.(3).(2) = Empty)
let _ = assert (world.(3).(1) = Spider)
let () = Printf.printf "OK \n"

(* L'araignée attaque le chameau du bas *)
let () = Printf.printf "spider_attack :                    "
let () = lives := 12
let _ = assert (move (3, 1) (3, 0) = (3, 1))
let _ = assert (world.(3).(1) = Spider)
let _ = assert (world.(3).(0) = Camel)
let _ = assert (!lives = 11)
let () = Printf.printf "OK\n"

(* Le serpent essaye de récupérer la clé *)
let () = Printf.printf "entity_key_collapse_behaviour :    "
let () = level_number := 42 
let _ = assert (move (1, 0) (0, 0) = (1, 0))
let _ = assert (world.(1).(0) = Snake)
let _ = assert (world.(0).(0) = Key)
let _ = assert (!level_number = 42)
let () = Printf.printf "OK \n"

(* Le chameau du haut essaye de récupérer la clé *)
let _ = Printf.printf "camel_key_collapse_behaviour :     "
let () = level_number := 42
let _ = assert (move (0, 1) (0, 0) = (0, 1))
let _ = assert (world.(0).(1) = Camel)
let _ = assert (world.(0).(0) = Key)
let _ = assert (!level_number = 42) (* Comportement voulu car ici level_activated = false *)
let () = Printf.printf "OK \n"

(* L'araignée tue le chameau du bas : 
   Le assert ne passe que si l'effet End_of_game est catch
   et que la position renvoyée est celle du camel *)
let () = Printf.printf "spider_kill_camel :                "
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
let () = Printf.printf "OK \n"

(* Discretisation d'un segment diagonale *)
let () = Printf.printf "descrete_ray_cast diag :           "
let _ = assert (
  descrete_ray_cast (0, 0) (6, 6) = 
  [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (3, 2); (3, 3); (4, 3); (4, 4); (5, 4); (5, 5); (6, 5); (6, 6)])
let () = Printf.printf "OK\n"

(* Discretisation d'un segment diagonale plus raide*)
let () = Printf.printf "descrete_ray_cast diag2 :          "
let _ = assert (
  descrete_ray_cast (2, 0) (4, 6) = 
  [(2, 0); (2, 1); (3, 1); (3, 2); (3, 3); (3, 4); (4, 4); (4, 5); (4, 6)])
let () = Printf.printf "OK \n"

(* Discretisation d'un segment vertical *)
let () = Printf.printf "descrete_ray_cast vertical :       "
let _ = assert (
  descrete_ray_cast (0, 0) (0, 6) = 
  [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6)])
let () = Printf.printf "OK \n"

(* Discretisation d'un segment horizontal *)
let () = Printf.printf "descrete_ray_cast horizontal :     "
let _ = assert (
  descrete_ray_cast (0, 0) (6, 0) =
  [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0)])
let () = Printf.printf "OK \n"

(* Discretisation d'un point *)
let () = Printf.printf "descrete_ray_cast point :          "
let _ = assert (
  descrete_ray_cast (2, 2) (2, 2) =
  [(2, 2)])
let () = Printf.printf "OK \n"

(* Discretisation d'un segment avec x' < x *)
let () = Printf.printf "descrete_ray_cast right_to_left :  "
let _ = assert (
  descrete_ray_cast (5, 3) (2, 4) =
  [(5, 3); (4, 3); (3, 3); (3, 4); (2, 4)])
let () = Printf.printf "OK \n"
    
(* Cas limite de safe_random_position où il n'y a qu'une case vide *)
let () = Printf.printf "safe_random_position :             "
let monde_plein = Array.make_matrix width height Snake
let () = monde_plein.(12).(7) <- Empty
let () = override_world monde_plein
let () = assert(safe_random_position () = (12, 7))
let () = Printf.printf "OK \n"

(* Test d'un tas min d'entier *)
let tas = Heap.init 50 (>=) 8
let () = Printf.printf "Heap.is_empty :                    "
let _ = assert (is_empty tas = false)
let () = Printf.printf ("OK\n")

let () = Heap.push tas 6
let () = Heap.push tas 7
let () = Heap.push tas 3
let () = Heap.push tas 4
let () = Heap.push tas 5
let () = Heap.push tas 5
let () = Heap.push tas 1

let () = Printf.printf "Heap.pop :                         "
let () = assert (Heap.pop tas = 1)
let () = assert (Heap.pop tas = 3)
let () = assert (Heap.pop tas = 4)
let () = assert (Heap.pop tas = 5)
let () = assert (Heap.pop tas = 5)
let () = assert (Heap.pop tas = 6)
let () = assert (Heap.pop tas = 7)
let () = assert (Heap.pop tas = 8)
let () = Printf.printf ("OK\n")


(* Test si [a_star] renvoie bien un chemin le plus court*)
let () = Printf.printf "a_star no_obstacle :               "
let _ = override_world [|
  [|Empty ;Empty ;Empty ;Empty ;Empty |];
  [|Empty ;Empty ;Empty ;Empty ;Empty |];
  [|Camel ;Empty ;Empty ;Empty ;Monkey|];
  [|Empty ;Empty ;Empty ;Empty ;Empty |];
  [|Empty ;Empty ;Empty ;Empty ;Empty |]|]
let length = List.length (a_star (2, 4) (2, 0) distance)
let _ = assert(length = 4)
let () = Printf.printf "OK \n"

let () = Printf.printf "a_star small_wall :                "
let _ = override_world [|
  [|Empty ;Empty ;Empty ;Empty ;Empty |];
  [|Empty ;Empty ;Cactus;Empty ;Empty |];
  [|Camel ;Empty ;Cactus;Empty ;Empty |];
  [|Empty ;Empty ;Cactus;Empty ;Monkey|];
  [|Empty ;Empty ;Empty ;Empty ;Empty |]|]
let length = List.length (a_star (3, 4) (2, 0) distance)
let _ = assert(length = 7)
let () = Printf.printf "OK \n"

let () = Printf.printf "a_star big_wall :                  "
let _ = override_world [|
  [|Empty ;Empty ;Empty ;Empty ;Empty |];
  [|Empty ;Empty ;Cactus;Empty ;Empty |];
  [|Camel ;Empty ;Cactus;Empty ;Empty |];
  [|Empty ;Empty ;Cactus;Empty ;Monkey|];
  [|Empty ;Empty ;Cactus;Empty ;Empty |]|]
let length = List.length (a_star (3, 4) (2, 0) distance)
let _ = assert(length = 9)
let () = Printf.printf "OK \n"















