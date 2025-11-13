open Roguelib

open World
open Ui
open Utils
open Light
open Engine

open Player
open Snake
open Elephant
open Spider
open Entity
open Monkey

open Flag

(* Initialisation des niveaux *)
let enemies = [snake, elephant, spider, monkey]
let level_enemies = [
  [3, 0, 1, 0], (* level 1 *)
  [3, 0, 1, 0], (* level 2 *)
  [3, 1, 2, 0], (* level 3 *)
  [4, 1, 2, 0], (* level 4 *)
  [4, 2, 2, 1], (* level 5 *)
  [4, 2, 2, 1], (* level 6 *)
  [4, 2, 3, 1], (* level 7 *)
  [6, 3, 3, 1], (* level 8 *)
  [6, 3, 3, 1], (* level 9 *)
  [6, 3, 4, 2], (* level 10 *)
  [6, 4, 4, 2]  (* level 11 *) (* niveau en boucle à l'infini *)
]

(* Initialisation du module Random*)
let () = Random.self_init ()

(* Initialisation du monde *)
let () = fill_world ()

(* Crée les différentes instances des entités *)
let camel_initial_position = random_position ()
let () = set camel_initial_position Camel
let camel_instance = new entity camel_initial_position

let snake_initial_position = random_position ()
let () = set snake_initial_position Snake
let snake_instance = new entity snake_initial_position

let elephant_initial_position = random_position ()
let () = set elephant_initial_position Elephant
let elephant_instance = new elephant elephant_initial_position

let spider_initial_position = random_position ()
let () = set spider_initial_position Spider
let spider_instance = new entity spider_initial_position

let monkey_initial_position = random_position ()
let () = set monkey_initial_position Monkey
let monkey_instance = new entity monkey_initial_position

let () = 
  if is_curse_darkness_on then
    enlighten_the_world camel_initial_position
  else
    ()

(* La file contient uniquement le tour du chameau *)

let () = Queue.add (fun () -> player (fun () -> camel camel_instance)) queue

let () = Queue.add (fun () -> player (fun () -> snake snake_instance)) queue

let () = Queue.add (fun () -> player (fun () -> elephant elephant_instance)) queue

let () = Queue.add (fun () -> player (fun () -> spider spider_instance)) queue

let () = Queue.add (fun () -> player (fun () -> monkey monkey_instance)) queue

(* Début du jeu *)
let () = run_queue ()


