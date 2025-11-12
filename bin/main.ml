open World
open Ui
open Utils

open Player
open Snake
open Entity

open Engine
open Player

(* Initialisation du monde *)

(* Initialisation du module Random*)
let () = Random.self_init ()

(** [random_position ()] renvoie une position aléatoire dans le monde*)
let random_position () : int * int = (Random.int width, Random.int height)

(* Place les cactus et le chameau initialement.*)

let () =
  for _ = 0 to 200 do set (random_position ()) Cactus   done 

let camel_initial_position = random_position ()
let () = set camel_initial_position Camel

let snake_initial_position = random_position ()
let () = set snake_initial_position Snake
let snake_instance = new entity snake_initial_position



(* La file contient uniquement le tour du chameau *)

let () = Queue.add (fun () -> player (fun () -> camel camel_initial_position)) queue

let () = Queue.add (fun () -> player (fun () -> snake snake_instance)) queue


(* Début du jeu *)
let () = run_queue ()


